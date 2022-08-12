package playground

import cats.data.NonEmptyList
import com.amazonaws.lambda.LambdaGen
import demo.smithy.DemoServiceGen
import playground.smithyql.QualifiedIdentifier
import weaver._

object ServiceNameExtractorTests extends FunSuite {
  test("extract name of service with an AWS hint") {
    assert.eql(
      ServiceNameExtractor.fromService(LambdaGen.service),
      QualifiedIdentifier(NonEmptyList.of("com", "amazonaws", "lambda"), "Lambda"),
    )
  }

  test("extract name of demo service") {

    assert.eql(
      ServiceNameExtractor.fromService(DemoServiceGen.service),
      QualifiedIdentifier(NonEmptyList.of("demo", "smithy"), "DemoService"),
    )
  }
}
