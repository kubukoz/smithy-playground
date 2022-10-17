package playground.lsp

import cats.MonadThrow
import cats.effect.kernel.Ref
import cats.implicits._
import playground.BuildConfig
import playground.language.Uri

trait ServerLoader[F[_]] {
  type Params
  def prepare(workspaceFolders: Option[List[Uri]]): F[ServerLoader.PrepareResult[Params]]
  def perform(params: Params): F[ServerLoader.WorkspaceStats]
  def server: LanguageServer[F]
}

object ServerLoader {
  def apply[F[_]](implicit F: ServerLoader[F]): F.type = F

  type Aux[F[_], Params_] = ServerLoader[F] { type Params = Params_ }

  case class PrepareResult[A](params: A, isChanged: Boolean)

  case class WorkspaceStats(importCount: Int, dependencyCount: Int, pluginCount: Int) {

    def render: String =
      s"$importCount imports, $dependencyCount dependencies and $pluginCount plugins"

  }

  object WorkspaceStats {

    def fromBuildConfig(bc: BuildConfig): WorkspaceStats = WorkspaceStats(
      importCount = bc.imports.combineAll.size,
      dependencyCount = bc.mavenDependencies.combineAll.size,
      pluginCount = bc.plugins.flatMap(_.smithyPlayground).flatMap(_.extensions).combineAll.size,
    )

  }

  def instance[
    F[_]: ServerBuilder: BuildLoader: Ref.Make: MonadThrow
  ]: F[ServerLoader.Aux[F, BuildLoader.Loaded]] = {
    case class State(
      currentServer: LanguageServer[F],
      lastUsedConfig: Option[BuildLoader.Loaded],
    )
    object State {
      val initial: State = apply(LanguageServer.notAvailable[F], none)
    }

    Ref[F]
      .of(State.initial)
      .map[ServerLoader.Aux[F, BuildLoader.Loaded]] { stateRef =>
        new ServerLoader[F] {
          type Params = BuildLoader.Loaded

          def prepare(
            workspaceFolders: Option[List[Uri]]
          ): F[PrepareResult[Params]] = stateRef.get.flatMap { state =>
            BuildLoader[F]
              .load(
                workspaceFolders
                  .orElse(state.lastUsedConfig.map(_.workspaceFolders))
                  .getOrElse(sys.error("FATAL: no workspace folders available"))
              )
              .map { case params =>
                PrepareResult(params, !state.lastUsedConfig.contains(params.config))
              }
          }

          def perform(params: Params): F[WorkspaceStats] = ServerBuilder[F]
            .build(params, this)
            .map(server => State(server, Some(params)))
            .flatMap(stateRef.set)
            .as(WorkspaceStats.fromBuildConfig(params.config))

          val server: LanguageServer[F] = LanguageServer.defer(stateRef.get.map(_.currentServer))
        }
      }
      .flatTap { serverLoader =>
        // loading with dummy config to initialize server without dependencies
        serverLoader.perform(BuildLoader.Loaded.default)
      }
  }

}
