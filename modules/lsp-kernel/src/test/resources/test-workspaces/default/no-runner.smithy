$version: "2"

namespace noop

service NoRunnerService {
    operations: [Noop]
}

@http(method: "GET", uri: "/")
@readonly
operation Noop {
    input := {}
    output := {}
}
