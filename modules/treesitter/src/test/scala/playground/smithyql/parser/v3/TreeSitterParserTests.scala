package playground.smithyql.parser.v3

import org.polyvariant.treesitter4s.Node
import org.polyvariant.treesitter4s.TreeSitterAPI
import playground.generated.nodes.SourceFile
import weaver.*

object TreeSitterParserTests extends FunSuite {

  private def parse(s: String): SourceFile = {
    val p = TreeSitterAPI.make("smithyql")
    SourceFile.unsafeApply(p.parse(s).rootNode.get)
  }

  test("SourceFile fields") {
    val in = parse("""use service foo.bar.baz.bax#Baz
                     |GetBaz{}""".stripMargin)

    assert.eql(in.prelude.map(_.use_clause.size), Some(1)) &&
    assert(in.statements.nonEmpty)
  }

  test("All parents of deep child") {
    val allNodes = parse("""use service foo.bar.baz.bax#Baz
                           |GetBaz { a = { x = 42 }}""".stripMargin)
      .fold[List[Node]](_ :: _.flatten.toList)

    val parentTypesAndSources = allNodes
      .find(_.source == "x")
      .get
      .selfAndParents
      .map(n => n.tpe -> n.source)
      .mkString("\n")

    val expected = List(
      "identifier" -> "x",
      "binding" -> "x = 42",
      "struct" -> "{ x = 42 }",
      "binding" -> "a = { x = 42 }",
      "struct" -> "{ a = { x = 42 }}",
      "run_query" -> "GetBaz { a = { x = 42 }}",
      "top_level_statement" -> "GetBaz { a = { x = 42 }}",
      "source_file" -> "use service foo.bar.baz.bax#Baz\nGetBaz { a = { x = 42 }}",
    ).mkString("\n")

    assert.same(expected, parentTypesAndSources)
  }

  test("Deep insight into field") {
    val in = parse("""use service foo.bar.baz.bax#Baz
                     |GetBaz { a = { x = 42 } }""".stripMargin)

    // this ain't pretty huh
    // watch out for the upcoming lookup DSL
    val valueOfX =
      in.statements
        .head
        .run_query
        .get
        .input
        .get
        .bindings
        .find(_.key.get.source == "a")
        .get
        .value
        .get
        .asStruct
        .get
        .bindings
        .find(_.key.get.source == "x")
        .get
        .value
        .get
        .asNumber
        .get
        .source
        .toInt

    assert.eql(42, valueOfX)
  }

  test("Deep insight into field, but the file isn't valid") {
    val in = parse("""use service fo o.b ar.b/az.bax/#//B//,,{}az
                     |GetBa z { a = { x = 42, 50 }, z, 42 }""".stripMargin)

    // this ain't pretty huh
    // watch out for the upcoming lookup DSL
    val valueOfX =
      in.statements
        .head
        .run_query
        .get
        .input
        .get
        .bindings
        .find(_.key.get.source == "a")
        .get
        .value
        .get
        .asStruct
        .get
        .bindings
        .find(_.key.get.source == "x")
        .get
        .value
        .get
        .asNumber
        .get
        .source
        .toInt

    assert.eql(42, valueOfX)
  }
}
