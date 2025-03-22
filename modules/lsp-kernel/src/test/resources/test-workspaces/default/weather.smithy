$version: "2"

namespace weather

use alloy#simpleRestJson

@simpleRestJson
service WeatherService {
    operations: [
        GetWeather
    ]
}

@http(method: "GET", uri: "/weather/{city}")
@readonly
operation GetWeather {
    input := {
        @required
        @httpLabel
        city: String
    }

    output := {
        @required
        weather: Weather
    }
}

union Weather {
    good: GoodWeather
    decent: Unit
}

structure GoodWeather {
    reallyGood: Boolean
}
