namespace playground.cli
use smithy4s.api#simpleRestJson

@simpleRestJson
service CliService {
  operations: [Run, Format, Compile, Info]
}

@http(method: "POST", uri: "/run")
operation Run {
  input: RunInput,
  output: RunOutput,
  errors: [RunningError]
}

@error("server")
@httpError(500)
structure RunningError {
  @required
  response: String
}

structure RunOutput {
  @required
  response: String
}

structure RunInput {
  @required
  input: String,
  @required
  baseUri: Url,
  width: Integer,
  context: String
}

string Url

@http(method: "POST", uri: "/format")
operation Format {
  input: FormatInput
}

structure FormatInput {
  @required
  input: String
}

@http(method: "POST", uri: "/compile")
operation Compile {

}

@http(method: "POST", uri: "/info")
operation Info {
}

