package playground.lsp

import cats.MonadThrow
import cats.effect.kernel.Ref
import cats.implicits._
import playground.BuildConfig

trait ServerLoader[F[_]] {
  type Params
  def prepare: F[ServerLoader.PrepareResult[Params]]
  def perform(params: Params): F[ServerLoader.WorkspaceStats]
  def server: LanguageServer[F]
}

object ServerLoader {
  def apply[F[_]](implicit F: ServerLoader[F]): F.type = F

  case class PrepareResult[A](params: A, isChanged: Boolean)
  case class WorkspaceStats(importCount: Int, dependencyCount: Int, pluginCount: Int)

  object WorkspaceStats {

    def fromBuildConfig(bc: BuildConfig): WorkspaceStats = WorkspaceStats(
      importCount = bc.imports.combineAll.size,
      dependencyCount = bc.mavenDependencies.combineAll.size,
      pluginCount = bc.plugins.flatMap(_.smithyPlayground).flatMap(_.extensions).combineAll.size,
    )

  }

  def instance[F[_]: ServerBuilder: BuildLoader: Ref.Make: MonadThrow]: F[ServerLoader[F]] = {
    case class State(currentServer: LanguageServer[F], lastUsedConfig: Option[BuildConfig])
    object State {
      val initial: State = State(LanguageServer.notAvailable[F], none)
    }

    Ref[F].of(State.initial).flatMap { serverRef =>
      val instance =
        new ServerLoader[F] {
          type Params = BuildLoader.Loaded

          val prepare: F[PrepareResult[Params]] = serverRef.get.map(_.lastUsedConfig).flatMap {
            lastUsedConfig =>
              BuildLoader[F].load.map { case params =>
                PrepareResult(params, !lastUsedConfig.contains(params.config))
              }
          }

          def perform(params: Params): F[WorkspaceStats] = ServerBuilder[F]
            .build(params, this)
            .map(server => State(server, Some(params.config)))
            .flatMap(serverRef.set)
            .as(WorkspaceStats.fromBuildConfig(params.config))

          val server: LanguageServer[F] = LanguageServer.defer(serverRef.get.map(_.currentServer))
        }

      // Initial load
      BuildLoader[F].load.flatMap(instance.perform).as(instance)
    }
  }

}
