$version: "2"

namespace demo.fake_aws

@aws.api#service(sdkId: "MyThing", endpointPrefix: "mything")
service MyAwsService {
    operations: [
        MyAwsOp
    ]
}

operation MyAwsOp {}

@aws.api#service(sdkId: "My Good Thing", endpointPrefix: "mygoodthing")
service MyAwsService2 {
    operations: [
        MyAwsOp
    ]
}
