package playground.lsp

import software.amazon.smithy.model.shapes.ShapeId
import weaver._

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

object ModelLoaderTests extends FunSuite {

  test("Empty loader config can see stdlib") {
    val result =
      loadModelEmpty()
        .getServiceShapes()
        .asScala
        .map(_.getId())
        .toSet

    assert.eql(result.map(_.getName()), Set("Random", "Clock"))
  }

  test("Empty loader cannot see UUID without a dependency") {
    val shapeId = ShapeId.from("smithy4s.api#UUID")
    val result =
      loadModelEmpty()
        .getShape(shapeId)
        .toScala

    assert.same(result, None)
  }

  test("Loader with dependencies can see external shapes") {
    val shapeId = ShapeId.from("smithy4s.api#UUID")
    val result = ModelLoader
      .loadUnsafe(
        specs = Set.empty,
        dependencies = List("com.disneystreaming.smithy4s:smithy4s-protocol:0.16.10"),
        repositories = Nil,
      )
      ._2
      .expectShape(shapeId)

    assert.same(result.getId(), shapeId)
  }

  private def loadModelEmpty() =
    ModelLoader
      .loadUnsafe(specs = Set.empty, dependencies = Nil, repositories = Nil)
      ._2

}
