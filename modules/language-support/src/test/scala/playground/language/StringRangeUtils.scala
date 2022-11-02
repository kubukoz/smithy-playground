package playground.language

import playground.smithyql.SourceRange
import playground.smithyql.Position

object StringRangeUtils {

  implicit class StringRangeOps(source: String) {
    def positionOf(text: String): Position = Position(source.indexOf(text))

    def rangeOf(text: String): SourceRange = {
      val pos = positionOf(text)
      SourceRange(pos, pos.moveRight(text.length()))
    }

  }

}
