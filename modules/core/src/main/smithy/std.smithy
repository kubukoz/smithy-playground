$version: "2"

namespace playground.std

use alloy#UUID

@trait(selector: "service")
@protocolDefinition
structure stdlib {}

@stdlib
@documentation("A standard library service providing random generators of data.")
service Random {
    operations: [
        NextUUID
    ]
}

@documentation("Generates a new UUID.")
operation NextUUID {
    output := {
        @required
        value: UUID
    }
}

@stdlib
@documentation("A standard library service providing time operations.")
service Clock {
    operations: [
        CurrentTimestamp
    ]
}

@documentation("Provides the current time as a Timestamp.")
operation CurrentTimestamp {
    output := {
        @required
        value: Timestamp
    }
}
