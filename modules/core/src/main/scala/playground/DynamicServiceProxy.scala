package playground

import cats.MonadThrow
import cats.implicits._
import smithy4s.Document
import smithy4s.Endpoint
import smithy4s.Service
import smithy4s.kinds._
import smithy4s.schema.Schema

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
      new smithy4s.kinds.PolyFunction5[Endpoint[Op, *, *, *, *, *], Proxy] {
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
        .precomputeBy(service.endpoints, _.name)

    new FunctorInterpreter[Op, F] {
      def apply[I, E, O, SI, SO](op: Op[I, E, O, SI, SO]): F[O] = {
        val (input, endpoint) = service.endpoint(op)
        endpointMapping(endpoint)(input)
      }
    }
  }

  private final implicit class PolyFunction5Ops[F[_, _, _, _, _], G[_, _, _, _, _]](
    self: PolyFunction5[F, G]
  ) {

    // copied from smithy4s PolyFunction5's unsafeCacheBy
    final def precomputeBy[K](
      allPossibleInputs: Seq[Kind5.Existential[F]],
      getKey: Kind5.Existential[F] => K,
    ): PolyFunction5[F, G] =
      new PolyFunction5[F, G] {

        private val map: Map[K, Any] = {
          val builder = Map.newBuilder[K, Any]
          allPossibleInputs.foreach(input =>
            builder += getKey(input) -> self
              .apply(input.asInstanceOf[F[Any, Any, Any, Any, Any]])
              .asInstanceOf[Any]
          )
          builder.result()
        }

        def apply[A0, A1, A2, A3, A4](input: F[A0, A1, A2, A3, A4]): G[A0, A1, A2, A3, A4] = map(
          getKey(Kind5.existential(input))
        ).asInstanceOf[G[A0, A1, A2, A3, A4]]

      }

  }

}
