import smithy4s.SchemaIndex

import org.http4s.Uri

import org.http4s.HttpApp

import org.http4s.client.Client

import smithy4s.Service

import smithy4s.http4s.SimpleRestJsonBuilder

import smithy4s.api.SimpleRestJson

import fs2.io.file.Path

import cats.effect.IO
import fs2.io.file.Files
import smithy4s.dynamic.model.Model
import smithy4s.dynamic.DynamicSchemaIndex

import cats.effect.unsafe.implicits._

// val modelText = IO
//   .blocking {
//     import sys.process._

//     List(
//       "smithy4s-codegen",
//       "dump-model",
//       "/Users/kubukoz/projects/smithy-playground/core/src/main/smithy/demo.smithy",
//     ).!!
//   }
//   .unsafeRunSync()

// val capi = smithy4s.http.json.codecs()

// val services =
//   DynamicSchemaIndex
//     .load(
//       capi.decodeFromByteArray(capi.compileCodec(Model.schema), modelText.getBytes()).toTry.get,
//       SimpleRestJson
//         .protocol
//         .schemas ++
//         // todo: should this be included?
//         SchemaIndex(SimpleRestJson),
//     )
//     .allServices
//     .head
