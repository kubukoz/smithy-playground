package playground.lsp2

import bsp.BuildClient
import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.effect.kernel.Resource
import concurrent.duration.*
import fs2.io.file.Files
import fs2.io.net.Network
import fs2.io.net.unixsocket.UnixSocketAddress
import fs2.io.net.unixsocket.UnixSockets
import fs2.io.process.Processes
import jsonrpclib.Channel
import jsonrpclib.Monadic
import jsonrpclib.fs2.given
import jsonrpclib.smithy4sinterop.ClientStub
import langoustine.lsp.Communicate
import langoustine.lsp.LSPBuilder
import langoustine.lsp.app.LangoustineApp
import langoustine.lsp.requests.LSPNotification
import langoustine.lsp.requests.LSPRequest
import smithy4s.kinds.Kind1
import smithy4s.kinds.Kind5
import smithy4sbsp.bsp4s.BSPCodecs

object Main extends LangoustineApp {

  def server(args: List[String]): Resource[IO, LSPBuilder[IO]] = IO
    .deferred[playground.lsp.LanguageClient[IO]]
    .toResource
    .flatMap { clientRef =>
      implicit val lc
        : playground.lsp.LanguageClient[IO] = playground.lsp.LanguageClient.defer(clientRef.get)

      import jsonrpclib.fs2.*

      val mkBloopClient = Files[IO].tempDirectory.flatMap { temp =>
        Processes[IO]
          .spawn(
            fs2
              .io
              .process
              .ProcessBuilder(
                "bloop",
                "bsp",
                "--socket",
                (temp / "bloop.sock").toNioPath.toString,
              )
          )
          .evalTap(p =>
            IO.consoleForIO.errorln("socket: " + (temp / "bloop.sock")) *> IO.sleep(1.second)
          )
          .flatMap { proc =>
            UnixSockets
              .forAsync[IO]
              .client(UnixSocketAddress((temp / "bloop.sock").toNioPath))
              .flatMap { socket =>
                jsonrpclib.fs2.FS2Channel[IO]().compile.resource.onlyOrError.flatMap { chan =>
                  // we use the sock

                  fs2
                    .Stream
                    .never[IO]
                    .concurrently(
                      socket
                        .reads
                        .observe(_.through(fs2.text.utf8.decode[IO]).evalMap(lc.logOutput).drain)
                        .through(jsonrpclib.fs2.lsp.decodeMessages)
                        .through(chan.inputOrBounce)
                    )
                    .concurrently(
                      chan
                        .output
                        .through(jsonrpclib.fs2.lsp.encodeMessages)
                        .observe(_.through(fs2.text.utf8.decode[IO]).evalMap(lc.logOutput).drain)
                        .through(socket.writes)
                    )
                    .compile
                    .drain
                    .background
                    .flatMap { _ =>
                      val extraEndpoints = BSPCodecs.serverEndpoints(
                        bsp
                          .BuildClient
                          .impl(new bsp.BuildClient.FunctorEndpointCompiler[IO] {
                            def apply[A0, A1, A2, A3, A4](
                              fa: BuildClient.Endpoint[A0, A1, A2, A3, A4]
                            ): A0 => IO[A2] =
                              in =>
                                IO {
                                  System
                                    .err
                                    .println(s"${fa.name}: received: " + in)
                                    .asInstanceOf[A2]
                                }
                          })
                      )

                      chan.withEndpoints(extraEndpoints).map(_ => chan)
                    }
                }

              }

          }
      }
      mkBloopClient.flatMap { (bloop: jsonrpclib.Channel[IO]) =>
        playground
          .lsp
          .MainServer
          .makeServer[IO](bloop)
          .map(LangoustineServerAdapter.adapt(_).apply(LSPBuilder.create[IO]))
          .map(bindClient(_, clientRef))
      }
    }

  private def bindClient(
    lsp: LSPBuilder[IO],
    clientDef: Deferred[IO, playground.lsp.LanguageClient[IO]],
  ): LSPBuilder[IO] =
    new LSPBuilder[IO] {
      export lsp.build
      export lsp.handleNotification
      export lsp.handleRequest

      override def bind[T <: Channel[IO]](
        channel: T,
        communicate: Communicate[IO],
      )(
        using Monadic[IO]
      ): IO[T] =
        clientDef.complete(LangoustineClientAdapter.adapt(communicate)) *>
          lsp.bind(channel, communicate)
    }

}
