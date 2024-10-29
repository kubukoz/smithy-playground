package playground

import cats.data.NonEmptyList
import demo.fake_aws.MyGoodThing
import demo.fake_aws.MyThing
import demo.smithy.DemoServiceGen
import playground.smithyql.QualifiedIdentifier
import smithy4s.ShapeId
import weaver.*

object ServiceNameExtractorTests extends FunSuite {
  test("extract name of service with an AWS hint") {
    assert.eql(
      ServiceNameExtractor.fromService(MyThing.service),
      QualifiedIdentifier(NonEmptyList.of("demo", "fake_aws"), "MyThing"),
    )
  }

  test("extract name of service with an AWS hint and whitespace") {
    assert.eql(
      ServiceNameExtractor.fromService(MyGoodThing.service),
      QualifiedIdentifier(NonEmptyList.of("demo", "fake_aws"), "MyGoodThing"),
    )
  }

  test("aws hint dynamic") {
    val dsi = DynamicModel.discover()

    assert.eql(
      ServiceNameExtractor.fromService(
        dsi.getService(ShapeId("demo.fake_aws", "MyAwsService")).get.service
      ),
      QualifiedIdentifier(NonEmptyList.of("demo", "fake_aws"), "MyThing"),
    )

  }

  test("extract name of demo service") {

    assert.eql(
      ServiceNameExtractor.fromService(DemoServiceGen.service),
      QualifiedIdentifier(NonEmptyList.of("demo", "smithy"), "DemoService"),
    )
  }

}
