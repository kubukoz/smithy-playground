namespace playground.std
use smithy4s.api#UUID

@trait(selector: "service")
@protocolDefinition
structure stdlib {}

@stdlib
service Random {
  operations: [NextUUID]
}

operation NextUUID {
  output: NextUUIDOutput
}

structure NextUUIDOutput {
  @required
  value: UUID
}

@stdlib
service Clock {
  operations: [CurrentTimestamp]
}

operation CurrentTimestamp {
  output: CurrentTimestampOutput
}

structure CurrentTimestampOutput {
  @required
  value: Timestamp
}
