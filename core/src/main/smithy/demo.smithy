namespace demo.smithy

use smithy4s.api#simpleRestJson

@simpleRestJson
service DemoService {
  version: "0.0.1",
  operations: [CreateHero, CreateSubscription],
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
  @httpPayload
  @required
  hero: Hero
}

structure CreateHeroOutput {
  @httpPayload
  @required
  hero: Hero
}

union Hero {
  good: Good,
  bad: Bad,
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
  createdAt: Timestamp
}
