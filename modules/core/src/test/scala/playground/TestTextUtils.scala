package playground

import playground.smithyql.Position

object TestTextUtils {
  val CURSOR = """<<HERE>>"""

  def extractCursor(
    s: String
  ): (
    remainingString: String,
    cursorPosition: Position,
  ) = {
    val cursor = Position(s.indexOf(CURSOR))

    (s.replace(CURSOR, ""), cursor)
  }

}
