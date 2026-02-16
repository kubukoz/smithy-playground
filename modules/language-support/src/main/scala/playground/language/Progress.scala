package playground.language

import cats.Applicative
import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.effect.std.UUIDGen
import cats.effect.syntax.all.*
import cats.syntax.all.*

trait Progress[F[_]] {
  def report(message: Option[String]): F[Unit]
}

object Progress {

  def apply[F[_]](
    using f: Progress[F]
  ): Progress[F] = f

  trait Make[F[_]] {

    def create(
      title: String,
      message: Option[String],
    ): Resource[F, Progress[F]]

    def fromToken(
      token: String,
      title: String,
      message: Option[String],
    ): Resource[F, Progress[F]]

  }

  object Make {

    given [F[_]: Feedback: UUIDGen: MonadCancelThrow]: Make[F] =
      new {
        def create(title: String, message: Option[String]): Resource[F, Progress[F]] = Feedback[F]
          .hasProgressCapability
          .toResource
          .ifM(
            ifTrue = createProgress
              .toResource
              .flatMap(fromToken(_, title, message)),
            ifFalse = fallback[F].pure[Resource[F, *]],
          )

        private def createProgress = UUIDGen[F]
          .randomUUID
          .map(_.toString())
          .flatTap(Feedback[F].createWorkDoneProgress)

        def fromToken(token: String, title: String, message: Option[String])
          : Resource[F, Progress[F]] = Resource
          .makeCase(
            Feedback[F]
              .beginProgress(token, title, message)
          ) { (_, exit) =>
            Feedback[F]
              .endProgress(
                token = token,
                message = exit.toOutcome[F].fold("canceled", _ => "failed", _ => "completed").some,
              )
          }
          .as(Feedback[F].reportProgress(token, _))
      }

  }

  def create[F[_]](
    title: String,
    message: Option[String],
  )(
    using make: Make[F]
  ): Resource[F, Progress[F]] = make.create(title, message)

  def ignored[F[_]: Applicative]: Progress[F] = _ => Applicative[F].unit

  // instead of progress, uses window messages.
  // Used in case it's not possible to access or create a token
  def fallback[F[_]: Feedback]: Progress[F] =
    message =>
      Feedback[F].showInfoMessage(
        message.getOrElse("(no message)")
      )

  def fromToken[F[_]](
    token: String,
    title: String,
    message: Option[String],
  )(
    using make: Make[F]
  ): Resource[F, Progress[F]] = make.fromToken(token, title, message)

}
