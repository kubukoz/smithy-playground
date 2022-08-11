namespace demo.smithy

use smithy4s.api#simpleRestJson
use smithy4s.meta#indexedSeq
use smithy4s.meta#refinement

@refinement(targetType: "java.time.Instant", providerImport: "demo.smithy.InstantProvider._")
@trait
structure instant { }

@simpleRestJson
service DemoService {
  version: "0.0.1",
  operations: [CreateHero, GetPowers, CreateSubscription],
  errors: [GenericServerError]
}

@simpleRestJson
service DemoService2 {
  version: "0.0.1",
  operations: [GetVersion, CreateSubscription],
}


@readonly
@http(uri: "/version", method: "GET")
operation GetVersion {

}

@http(method: "POST", uri: "/heroes")
@documentation("""
Create a hero.
""")
operation CreateHero {
  input: CreateHeroInput,
  output: CreateHeroOutput,
  errors: [HeroIsBad]
}

structure CreateHeroInput {
  @required
  hero: Hero,

  @httpQuery("verbose")
  verbose: Boolean,

  powers: Powers,

  powerMap: PowerMap,

  friends: Friends,

  intSet: IntSet,
  friendSet: FriendSet,

  hasNewtypes: HasNewtypes,

  hasDeprecations: HasDeprecations
}

set FriendSet {
  member: Hero
}

list Friends {
  member: Hero
}

structure CreateHeroOutput {
  @httpPayload
  @required
  hero: Hero
}

union Hero {
  good: Good,
  bad: Bad,
  @deprecated(message: "No reason", since: "0.0.1")
  badder: Bad
}

structure Good {
  @required
  howGood: Integer,
}

structure Bad {
  @required
  evilName: String,
  @required
  powerLevel: Integer
}

@httpError(422)
@error("client")
structure HeroIsBad {
  @required
  powerLevel: Integer
}

@httpError(500)
@error("server")
structure GenericServerError {
  @required
  msg: String
}

@http(method: "GET", uri: "/poweres")
@readonly
operation GetPowers {
  output: GetPowersOutput,
}

structure GetPowersOutput {
  @httpPayload
  @required
  powers: Powers
}

list Powers {
  member: Power
}

map PowerMap {
  key: Power,
  value: Hero
}

@enum([{value: "Ice", name: "ICE"}, {value: "Fire", name: "FIRE"}, {value: "Lightning", name: "LIGHTNING"}, {value: "Wind", name: "WIND"}])
string Power


@http(method: "PUT", uri: "/subscriptions")
@idempotent
@documentation("""
Create a subscription.
""")
operation CreateSubscription {
  input: CreateSubscriptionInput,
  output: CreateSubscriptionOutput,
}

structure CreateSubscriptionInput {
  @httpPayload
  @required
  subscription: Subscription
}

structure CreateSubscriptionOutput {
  @httpPayload
  @required
  subscription: Subscription
}

structure Subscription {
  @required
  id: String,
  name: String,
  createdAt: Timestamp,
  status: SubscriptionStatus,
  skus: Skus,
  // recursive calls are currently not working - todo https://github.com/disneystreaming/smithy4s/issues/181
  // next: Subscription
}

list Skus {
  member: Sku
}

structure Sku {
  @required
  id: Integer,
  @required
  sku: String
}

@enum([{name: "ACTIVE", value: "ACTIVE"}, {name: "INACTIVE", value: "INACTIVE"}])
string SubscriptionStatus

@indexedSeq
list Ints {
  member: Integer
}

set IntSet {
  member: Integer
}

structure HasNewtypes {
  intSet: IntSet,
  myInt: MyInt,
  str: MyString,
  power: Power,
  powerMap: PowerMap,
  anUUID: smithy4s.api#UUID,
  anInstant: MyInstant,
}

@instant
timestamp MyInstant

integer MyInt

string MyString

@length(min: 1)
string StringWithLength

structure HasDeprecations {
  @deprecated(message: "Made-up reason")
  hasMessage: Boolean,
  @deprecated(since: "0.1.0")
  @required
  hasSince: Boolean,
  @deprecated(message: "Another reason", since: "1.0.0")
  @required
  hasBoth: Boolean,
}
