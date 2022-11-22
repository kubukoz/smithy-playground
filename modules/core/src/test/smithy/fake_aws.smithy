$version: "2"

namespace demo.fake_aws

@aws.api#service(sdkId: "MyThing", endpointPrefix: "mything")
service MyAwsService {
  operations: [MyAwsOp]
}

operation MyAwsOp {}
