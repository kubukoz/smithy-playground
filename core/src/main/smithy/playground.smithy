namespace com.disneystreaming.demo.smithy

use smithy4s.api#simpleRestJson

@simpleRestJson
service PlaygroundService {
  version: "0.0.1",
  operations: [RunQuery],
}

@http(method: "POST", uri: "/run")
operation RunQuery {
  input: RunQueryInput,
  output: RunQueryOutput,
}

structure RunQueryInput {
  @httpPayload
  @required
  input: String
}

structure RunQueryOutput {
  @httpPayload
  @required
  output: String
}
