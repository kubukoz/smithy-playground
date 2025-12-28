package playground.lsp

import cats.MonadThrow
import cats.effect.kernel.Concurrent
import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import playground.PlaygroundConfig
import playground.Uri
import playground.language.Progress

trait ServerLoader[F[_]] {
  type Params

  def prepare(
    workspaceFolders: Option[List[Uri]]
  ): F[ServerLoader.PrepareResult[Params]]

  def perform(
    params: Params,
    progress: Progress[F],
  ): F[ServerLoader.WorkspaceStats]

  def server: LanguageServer[F]
  def isInitialized: Signal[F, Boolean]
}

object ServerLoader {

  trait Queue[F[_]] {
    def schedule(task: F[Unit]): F[Unit]
  }

  object Queue {

    def apply[F[_]](
      implicit F: Queue[F]
    ): Queue[F] = F

    def createCancelable[F[_]: Concurrent]
      : Resource[F, Queue[F]] = cats.effect.std.Queue.synchronous[F, F[Unit]].toResource.flatMap {
      q =>
        fs2
          .Stream
          .fromQueueUnterminated(q)
          .switchMap(fs2.Stream.exec)
          .compile
          .drain
          .background
          .as(q.offer(_))
    }

  }

  def apply[F[_]](
    implicit F: ServerLoader[F]
  ): F.type = F

  type Aux[F[_], Params_] = ServerLoader[F] { type Params = Params_ }

  case class PrepareResult[A](
    params: A,
    isChanged: Boolean,
  )

  case class WorkspaceStats(
    sourceCount: Int,
    importCount: Int,
    dependencyCount: Int,
    pluginCount: Int,
  ) {

    def render: String =
      s"$sourceCount source entries, $importCount imports, $dependencyCount dependencies and $pluginCount plugins"

  }

  object WorkspaceStats {

    def fromPlaygroundConfig(
      bc: PlaygroundConfig
    ): WorkspaceStats = WorkspaceStats(
      sourceCount = bc.sources.size,
      importCount = bc.imports.size,
      dependencyCount = bc.dependencies.size,
      pluginCount = bc.extensions.size,
    )

  }

  def instance[F[_]: ServerBuilder: BuildLoader: Concurrent]
    : F[ServerLoader.Aux[F, BuildLoader.Loaded]] = {
    case class State(
      currentServer: LanguageServer[F],
      lastUsedConfig: Option[BuildLoader.Loaded],
    )
    object State {
      val initial: State = apply(LanguageServer.notAvailable[F], none)
    }

    (
      SignallingRef[F]
        .of(State.initial),
      Ref[F].of(Option.empty[List[Uri]]),
    )
      .mapN[ServerLoader.Aux[F, BuildLoader.Loaded]] {
        (
          stateRef,
          workspaceFoldersRef,
        ) =>
          new ServerLoader[F] {
            type Params = BuildLoader.Loaded

            override def prepare(
              workspaceFolders: Option[List[Uri]]
            ): F[PrepareResult[Params]] = workspaceFoldersRef
              .modify { oldFolders =>
                val newValue = workspaceFolders
                  .orElse(oldFolders)
                  .getOrElse(sys.error("FATAL: no workspace folders available"))

                (newValue.some, newValue)
              }
              .flatMap { workspaceFolders =>
                stateRef.get.flatMap { state =>
                  BuildLoader[F]
                    .load(workspaceFolders)
                    .map { params =>
                      PrepareResult(
                        params,
                        !state.lastUsedConfig.map(_.config).contains_(params.config),
                      )
                    }
                }
              }
            override def perform(
              params: Params,
              progress: Progress[F],
            ): F[WorkspaceStats] = ServerBuilder[F]
              .build(params, this, progress)
              .map(server => State(server, Some(params)))
              .flatMap(stateRef.set)
              .as(WorkspaceStats.fromPlaygroundConfig(params.config))

            val server: LanguageServer[F] = LanguageServer.defer(stateRef.get.map(_.currentServer))

            val isInitialized: Signal[F, Boolean] = stateRef.map(
              _.lastUsedConfig.exists(!_.isDummy)
            )
          }
      }
      .flatTap { serverLoader =>
        // loading with dummy config to initialize server without dependencies
        // we don't report progress either, as this is a very quick / uninteresting operation
        serverLoader.perform(BuildLoader.Loaded.default, Progress.ignored[F])
      }
  }

}
