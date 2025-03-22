package playground.language

trait Feedback[F[_]] {

  def showInfoMessage(
    msg: String
  ): F[Unit]

  def showWarnMessage(
    msg: String
  ): F[Unit]

  def showErrorMessage(
    msg: String
  ): F[Unit]

  // custom smithyql/showOutputPanel notification in the client
  def showOutputPanel: F[Unit]

  def logOutput(
    msg: String
  ): F[Unit]

}

object Feedback {

  def apply[F[_]](
    implicit F: Feedback[F]
  ): Feedback[F] = F

}
