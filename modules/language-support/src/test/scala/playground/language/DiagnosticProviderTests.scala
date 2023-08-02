package playground.language

import alloy.SimpleRestJson
import aws.protocols.AwsJson1_0
import aws.protocols.AwsJson1_1
import aws.protocols.AwsQuery
import aws.protocols.Ec2Query
import aws.protocols.RestJson1
import aws.protocols.RestXml
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.kernel.Resource
import demo.smithy.DemoServiceGen
import noop.NoRunnerServiceGen
import org.http4s.HttpApp
import org.http4s.client.Client
import playground.Assertions._
import playground.CompilationError
import playground.CompilationErrorDetails
import playground.CompilationFailed
import playground.DiagnosticSeverity
import playground.Diffs._
import playground.FileCompiler
import playground.FileRunner
import playground.OperationCompiler
import playground.OperationRunner
import playground.PreludeCompiler
import playground.ServiceIndex
import playground.ServiceUtils._
import playground.smithyql.QualifiedIdentifier
import playground.smithyql.SourceRange
import playground.smithyql.StringRangeUtils._
import playground.std.ClockGen
import playground.std.RandomGen
import playground.std.Stdlib
import playground.std.StdlibRuntime
import smithy4s.HasId
import smithy4s.aws.AwsEnvironment
import weaver._

object DiagnosticProviderTests extends SimpleIOSuite {

  private val client = Client.fromHttpApp(HttpApp.notFound[IO])

  private implicit val stdlib: StdlibRuntime[IO] = StdlibRuntime.instance[IO]

  private val services = List(
    wrapService(DemoServiceGen),
    wrapService(RandomGen),
    wrapService(ClockGen),
    wrapService(NoRunnerServiceGen),
  )

  private val knownServiceIds = NonEmptyList.of(
    QualifiedIdentifier.of("demo", "smithy", "DemoService"),
    QualifiedIdentifier.of("playground", "std", "Random"),
    QualifiedIdentifier.of("playground", "std", "Clock"),
    QualifiedIdentifier.of("noop", "NoRunnerService"),
  )

  // note: this is plugin-aware, hence we're not defining it in any main files
  private val knownProtocols = NonEmptyList
    .of[HasId](
      SimpleRestJson,
      AwsJson1_0,
      AwsJson1_1,
      RestJson1,
      AwsQuery,
      RestXml,
      Ec2Query,
      Stdlib,
    )
    .map(_.id)

  private val provider = DiagnosticProvider.instance(
    compiler = FileCompiler
      .instance(
        PreludeCompiler.instance[CompilationError.InIorNel](ServiceIndex.fromServices(services)),
        OperationCompiler.fromServices(services),
      )
      .mapK(CompilationFailed.wrapK),
    fileRunner = FileRunner.instance(
      OperationRunner.merge(
        OperationRunner.forServices[IO](
          services = services,
          getSchema = _ => None,
          client = client,
          baseUri = IO.stub,
          awsEnv = Resource.eval(IO.stub: IO[AwsEnvironment[IO]]),
          plugins = Nil,
        ),
        ServiceIndex.fromServices(services),
      )
    ),
  )

  pureTest("empty file - no diagnostics") {
    assertNoDiff(provider.getDiagnostics("test.smithyql", ""), Nil)
  }

  pureTest("file doesn't parse at all") {
    val input = "horrendous <parsing mistake>"
    assertNoDiff(
      provider.getDiagnostics("test.smithyql", input),
      List(
        CompilationError(
          err = CompilationErrorDetails.ParseError(expectationString = "{"),
          range = SourceRange.empty(input.positionOf("<")),
          severity = DiagnosticSeverity.Error,
          tags = Set(),
        )
      ),
    )
  }

  pureTest("file doesn't compile") {
    val input = "AnyOp {}"

    assertNoDiff(
      provider.getDiagnostics("test.smithyql", input),
      List(
        CompilationError(
          err = CompilationErrorDetails.AmbiguousService(
            workspaceServices = knownServiceIds.toList
          ),
          range = input.rangeOf("AnyOp"),
          severity = DiagnosticSeverity.Error,
          tags = Set(),
        )
      ),
    )
  }

  pureTest("file parses with multiple queries, doesn't compile yet") {
    val input =
      """op1 {}
        |op2 {}""".stripMargin

    assertNoDiff(
      provider.getDiagnostics(
        "test.smithyql",
        input,
      ),
      List(
        CompilationError.error(
          err = CompilationErrorDetails.AmbiguousService(
            workspaceServices = knownServiceIds.toList
          ),
          range = input.rangeOf("op1"),
        ),
        CompilationError.error(
          err = CompilationErrorDetails.AmbiguousService(
            workspaceServices = knownServiceIds.toList
          ),
          range = input.rangeOf("op2"),
        ),
      ),
    )
  }

  pureTest("file parses and compiles with multiple queries, missing runner for one of them") {

    val input =
      """playground.std#Random.NextUUID {}
        |noop#NoRunnerService.Noop {}""".stripMargin

    assertNoDiff(
      provider.getDiagnostics("test.smithyql", input),
      List(
        CompilationError.info(
          err = CompilationErrorDetails.UnsupportedProtocols(
            knownProtocols,
            Nil,
          ),
          range = input.rangeOf("noop#NoRunnerService.Noop"),
        )
      ),
    )
  }

  pureTest("file is correct with multiple queries") {

    val input =
      """playground.std#Random.NextUUID {}
        |playground.std#Clock.CurrentTimestamp {}""".stripMargin

    assertNoDiff(provider.getDiagnostics("test.smithyql", input), Nil)
  }

}
