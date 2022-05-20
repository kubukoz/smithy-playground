package playground.cli

import java.nio.file.Files
import java.nio.file.Paths
import smithy4s.http.json.codecs
import smithy4s.schema.Schema
import jsonschema._
import smithy4s.dynamic.model.Model
import smithy4s.dynamic.model.Shape
import smithy4s.dynamic.model.IdRef
import jsonschema.Tpe.ArrCase
import jsonschema.Tpe.SimpleCase
import smithy4s.dynamic.model.StructureShape
import smithy4s.dynamic.model.MemberShape
import cats.data.Writer
import cats.data.WriterT
import cats.implicits._
import smithy4s.dynamic.model.UnionShape
import smithy4s.api.Untagged
import smithy4s.Document
import smithy4s.dynamic.model.ListShape
import smithy4s.dynamic.model.StringShape
import smithy4s.dynamic.model.BooleanShape
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.ShapeId
import smithy4s.dynamic.model.IntegerShape

object WIP extends App {

  case class Shh(id: IdRef, value: Shape)

  def mkShape(name: String, s: jsonschema.Definition): Writer[List[Shh], Shh] = {
    s._type match {
      case None =>
        s.anyOf match {
          case Some(kinds) =>
            val shap = Shape.UnionCase(
              UnionShape(
                members = Some(
                  kinds
                    .zipWithIndex
                    .map { case (deff, i) =>
                      val rr = mkShape(name + "_bullshit_" + i, deff).value
                      rr.id.value -> MemberShape(target = rr.id)
                    }
                    .toMap
                ),
                traits = Some(Map(IdRef(Untagged.id.show) -> Document.obj())),
              )
            )

            Shh(IdRef(name), shap)
          case None =>
            s.ref match {
              case Some(v) =>
                val stripped = v.replace("#/definitions/", "")
                Shh(IdRef(ShapeId("temp", stripped).show), Shape.StructureCase(StructureShape()))
            }
        }
      case Some(v) =>
        val rrr: Shape =
          v match {
            case ArrCase(arr) =>
              Shape.UnionCase(
                UnionShape(
                  members = Some(
                    arr.map { a =>
                      a match {
                        case DefinitionType.NUMBER =>
                          "int" -> MemberShape(IdRef(Schema.int.shapeId.show), None)
                        case DefinitionType.STRING =>
                          "string" -> MemberShape(IdRef(Schema.string.shapeId.show), None)
                      }
                    }.toMap
                  ),
                  traits = Some(Map(IdRef(Untagged.id.show) -> Document.obj())),
                )
              )
            case SimpleCase(DefinitionType.BOOLEAN) => Shape.BooleanCase(BooleanShape())
            case SimpleCase(DefinitionType.STRING)  => Shape.StringCase(StringShape())
            case SimpleCase(DefinitionType.NUMBER)  => Shape.IntegerCase(IntegerShape())
            case SimpleCase(DefinitionType.ARRAY) =>
              Shape.ListCase {
                val ch = mkShape(name, s.items.get).value
                ListShape(member = MemberShape(target = ch.id))
              }
            case SimpleCase(DefinitionType.OBJECT) =>
              Shape.StructureCase(
                StructureShape(
                  members = s.properties.map {
                    _.map { case (k, v) =>
                      val r = v
                        ._type
                        .collect {
                          case SimpleCase(DefinitionType.STRING)  => Schema.string.shapeId.show
                          case SimpleCase(DefinitionType.BOOLEAN) => Schema.boolean.shapeId.show
                          case SimpleCase(DefinitionType.NUMBER)  => Schema.int.shapeId.show
                          case SimpleCase(DefinitionType.ARRAY)   =>
                            // this array type will need to be synthesised
                            ShapeId(
                              "temp",
                              v.items.get.ref.get.replace("#/definitions/", "") + "s",
                            ).show
                        }
                        .map(IdRef(_))
                        .getOrElse(mkShape(name + "_bullshit array_" + k, v).value.id)

                      k -> MemberShape(target = r)
                    }
                  }
                )
              )
          }

        Shh(IdRef(name), rrr)
    }
  }.pure[Writer[List[Shh], *]]

  def exec(path: String) = {
    val txt = Files.readAllBytes(
      Paths.get(path)
    )

    val capi = codecs()
    val codec = capi.compileCodec(jsonschema.Schema.schema)
    val schema = capi.decodeFromByteArray(codec, txt).toTry.get

    println(schema)

    val mod = Model(shapes = schema.definitions /* .take(1) */.map { case (k, v) =>
      val r = mkShape(k, v).run
      r._2.id -> r._2.value
    })

    println()
    Files.write(Paths.get("out.json"), capi.writeToArray(capi.compileCodec(Model.schema), mod))
    println(mod)
  }

  // println("can we load the schema?")
  // exec("/Users/kubukoz/Downloads/schema")
  println("can we load the metamodel?")
  exec("/Users/kubukoz/projects/vscode-languageserver-node/protocol/metaModel.schema.json")

  // {
  //   val capi = codecs()
  //   val codec = capi.compileCodec(jsonschema.Example.schema)
  //   val schema =
  //     capi
  //       .decodeFromByteArray(
  //         codec,
  //         """{"items": {"a": {"items": {"a": {"items": {}}}}}}""".getBytes(),
  //       )
  //       .toTry
  //       .get

  //   println(schema)
  // }
}
