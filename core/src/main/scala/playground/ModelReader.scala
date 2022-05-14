package playground

import smithy4s.dynamic.model.Model
import smithy4s.dynamic.DynamicSchemaIndex
import smithy4s.api.SimpleRestJson
import smithy4s.SchemaIndex
import aws.protocols.AwsJson1_0
import aws.protocols.AwsJson1_1

object ModelReader {

  val modelParser: String => Model = {
    val capi = smithy4s.http.json.codecs()
    val codec = capi.compileCodec(Model.schema)

    text => capi.decodeFromByteArray(codec, text.getBytes()).toTry.get
  }

  def buildSchemaIndex(model: Model): DynamicSchemaIndex = {
    val supportedSchemas =
      SimpleRestJson
        .protocol
        .schemas ++
        // todo: should be included
        SchemaIndex(
          SimpleRestJson,
          smithy.api.Error,
          smithy.api.Documentation,
          smithy.api.ExternalDocumentation,
          smithy.api.Deprecated,
        ) ++
        AwsJson1_0.protocol.schemas ++
        AwsJson1_1.protocol.schemas ++
        SchemaIndex(
          AwsJson1_0,
          AwsJson1_1,
          aws.api.Arn,
          aws.api.ArnNamespace,
          aws.api.ArnReference,
          aws.api.ClientDiscoveredEndpoint,
          aws.api.ClientEndpointDiscovery,
          aws.api.ClientEndpointDiscoveryId,
          aws.api.CloudFormationName,
          aws.api.ControlPlane,
          aws.api.Data,
          aws.api.DataPlane,
          aws.api.Service,
        )

    DynamicSchemaIndex
      .load(
        model,
        supportedSchemas,
      )
  }

}
