package playground.lsp

import playground.PlaygroundConfig
import software.amazon.smithy.model.shapes.ShapeId
import weaver._

import java.util.stream.Collectors
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

object ModelLoaderTests extends FunSuite {

  test("Empty loader config can see stdlib") {
    val result =
      loadModel()
        .getServiceShapes()
        .asScala
        .map(_.getId())
        .toSet

    assert.eql(result.map(_.getName()), Set("Random", "Clock"))
  }

  test("Empty loader can only see smithy.api and playground.std namespaces") {
    val result =
      loadModel()
        .shapes()
        .collect(Collectors.toList())
        .asScala
        .map(_.getId().getNamespace())
        .toSet

    assert.same(result, Set("smithy.api", "playground.std"))
  }

  test("Empty loader cannot see alloy without a dependency") {
    val shapeId = ShapeId.from("alloy#UUID")
    val result =
      loadModel()
        .getShape(shapeId)
        .toScala

    assert.same(result, None)
  }

  test("Loader with dependencies can see external shapes") {
    val shapeId = ShapeId.from("alloy#UUID")
    val result = loadModel(
      PlaygroundConfig
        .empty
        .copy(
          dependencies = List("com.disneystreaming.alloy:alloy-core:0.1.15")
        )
    )
      .expectShape(shapeId)

    assert.same(result.getId(), shapeId)
  }

  private def loadModel(
    config: PlaygroundConfig = PlaygroundConfig.empty
  ) = ModelLoader
    .load(
      // note: config is ignored here
      specs = Set.empty,
      jars = ModelLoader.resolveModelDependencies(config),
    )

}
