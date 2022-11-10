package playground

import cats.MonadThrow
import cats.implicits._
import smithy4s.Document
import smithy4s.Endpoint
import smithy4s.Service
import smithy4s.schema.Schema
import smithy4s.kinds._

class DynamicServiceProxy[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
  service: Service.Aux[Alg, Op]
) {

  def tryProxy[AlgStatic[_[_, _, _, _, _]], OpStatic[_, _, _, _, _], F[_]: MonadThrow](
    interp: FunctorAlgebra[AlgStatic, F]
  )(
    implicit serviceStatic: Service.Aux[AlgStatic, OpStatic]
  ): Option[FunctorInterpreter[Op, F]] =
    Option.when(service.id == serviceStatic.id)(proxy(interp)(serviceStatic))

  def proxy[AlgStatic[_[_, _, _, _, _]], OpStatic[_, _, _, _, _], F[_]: MonadThrow](
    interp: FunctorAlgebra[AlgStatic, F]
  )(
    serviceStatic: Service.Aux[AlgStatic, OpStatic]
  ): FunctorInterpreter[Op, F] = {
    val grp = serviceStatic.endpoints.groupBy(_.id).fmap(_.head)

    type Proxy[I, E, O, SE, EO] = I => F[O]

    def makeProxy[A, B](schemaIn: Schema[A], schemaOut: Schema[B]): A => F[B] = {
      val inputEncoder = Document.Encoder.fromSchema(schemaIn)
      val outputDecoder = Document.Decoder.fromSchema(schemaOut)

      in => outputDecoder.decode(inputEncoder.encode(in)).liftTo[F]
    }

    val endpointMapping =
      new (PolyFunction5[Endpoint[Op, *, *, *, *, *], Proxy]) {
        private val trans = serviceStatic.toPolyFunction(interp)

        private def applyWithStatic[I, E, O, SI, SO, STI, STE, STO, STSI, STSO](
          endpoint: Endpoint[Op, I, E, O, SI, SO],
          endpointStatic: Endpoint[OpStatic, STI, STE, STO, STSI, STSO],
        ): I => F[O] = {
          val mapInput = makeProxy(endpoint.input, endpointStatic.input)
          val mapOutput = makeProxy(endpointStatic.output, endpoint.output)

          def errorMapper[A]: Throwable => F[A] =
            endpointStatic.errorable match {
              case None => _.raiseError[F, A]
              case Some(errorableStatic) =>
                val errorable = endpoint.errorable.get // should be there at this point
                val mapError = makeProxy(errorableStatic.error, errorable.error)

                e =>
                  errorableStatic.liftError(e) match {
                    case None => e.raiseError[F, A]
                    case Some(liftedStatic) =>
                      mapError(liftedStatic)
                        .flatMap(errorable.unliftError(_).raiseError[F, A])
                  }
            }

          input =>
            mapInput(input)
              .map(endpointStatic.wrap)
              .flatMap(trans(_))
              .handleErrorWith(errorMapper)
              .flatMap(mapOutput)
        }

        def apply[I, E, O, SI, SO](endpoint: Endpoint[Op, I, E, O, SI, SO]): I => F[O] =
          applyWithStatic(endpoint, grp(endpoint.id))
      }
    // TODO https://github.com/disneystreaming/smithy4s/issues/590
    // .precompute(service.endpoints)

    new FunctorInterpreter[Op, F] {
      def apply[I, E, O, SI, SO](op: Op[I, E, O, SI, SO]): F[O] = {
        val (input, endpoint) = service.endpoint(op)
        endpointMapping(endpoint)(input)
      }
    }
  }

}
