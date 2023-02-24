package playground.smithyql

import playground.smithyql.Position
import playground.smithyql.SourceRange

// Mostly for testing.
object StringRangeUtils {

  implicit class StringRangeOps(
    source: String
  ) {

    def positionOf(
      text: String
    ): Position = Position(source.indexOf(text))

    def rangeOf(
      text: String
    ): SourceRange = {
      val pos = positionOf(text)
      SourceRange(pos, pos.moveRight(text.length()))
    }

    def lastPosition: Position = Position.lastInString(source)

  }

}
