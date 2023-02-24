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

  test("Empty loader cannot see alloy without a dependency") {
    val shapeId = ShapeId.from("alloy#UUID")
    val result =
      loadModelEmpty()
        .getShape(shapeId)
        .toScala

    assert.same(result, None)
  }

  test("Loader with dependencies can see external shapes") {
    val shapeId = ShapeId.from("alloy#UUID")
    val result = ModelLoader
      .loadUnsafe(
        specs = Set.empty,
        dependencies = List("com.disneystreaming.alloy:alloy-core:0.1.13"),
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
