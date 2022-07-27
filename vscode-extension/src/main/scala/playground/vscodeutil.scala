package playground

import cats.effect.kernel.Sync
import typings.vscode.anon.Dispose
import typings.vscode.mod.Disposable
import typings.vscode.mod.workspace

object vscodeutil {
  implicit def disposableToDispose(d: Disposable): Dispose = Dispose(() => d.dispose())

  def unsafeGetConfig[A](key: String): A = workspace.getConfiguration().get[A](key).get
  def getConfigF[F[_]: Sync, A](key: String): F[A] = Sync[F].delay(unsafeGetConfig[A](key))

}
