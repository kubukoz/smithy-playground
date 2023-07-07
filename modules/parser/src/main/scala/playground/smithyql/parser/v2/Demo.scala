package playground.smithyql.parser.v2

import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer

object Demo extends App {

  val input =
    """use service demo.smithy#DemoService

CreateHero {
  empty: [],
  emptyObj: {},
  hero: {
    good: {
      howGood: 42,
    },
  },
  friends: [
    {
      bad: {
        evilName: "Vader",
        powerLevel: 9001,
      },
    },
  ],
  doc: [
    "this is a document, so you can do pretty much anything here",
    null,
    false,
    42,
    {
      nested: "key",
    },
    [ ],
  ],
  hasNewtypes: {
    anInstant: "2022-10-08T00:46:31.378493Z",
    anUUID: "cd4f93e0-fd11-41f0-8f13-44f66e1f0997",
    power: "FIRE",
    powerMap: {
      FIRE: {
        good: {
          howGood: 10,
        },
      },
    },
  },
}

  """.stripMargin

  val l = new Tokens(CharStreams.fromString(input))

  val p = new SmithyQL(new CommonTokenStream(l))

  // p.removeParseListeners()
  p.removeErrorListeners()

  p.addErrorListener(new BaseErrorListener {

    override def syntaxError(
      recognizer: Recognizer[_ <: Object, _ <: Object],
      offendingSymbol: Object,
      line: Int,
      charPositionInLine: Int,
      msg: String,
      e: RecognitionException,
    ): Unit = {

      val (beforeError, afterError) = input
        .linesIterator
        .toList(line - 1)
        .splitAt(charPositionInLine)

      val previousLinesRange: String =
        input.linesWithSeparators.toList.slice(line - 3, line - 1).mkString

      val nextLinesRange: String = input.linesWithSeparators.toList.slice(line, line + 2).mkString

      println(
        s"""ERROR $line:$charPositionInLine @ $msg
           |${previousLinesRange}${Console.GREEN}${beforeError}${Console.RED}${afterError}${Console.RESET}
           |${" " * charPositionInLine}^HERE${nextLinesRange}""".stripMargin
      )
    }

  })
  println(p.source_file())

}
