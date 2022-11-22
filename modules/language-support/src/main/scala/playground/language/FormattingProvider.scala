package playground.language

import cats.FlatMap
import cats.implicits._
import playground.smithyql.SourceFile
import playground.smithyql.SourceRange
import playground.smithyql.format.Formatter
import playground.smithyql.parser.SourceParser

object FormattingProvider {

  def provider[F[_]: TextDocumentProvider: FlatMap](
    getWidthSetting: F[Int]
  ): Uri => F[List[TextEdit]] =
    fileUri =>
      TextDocumentProvider[F]
        .get(fileUri)
        .flatMap { text =>
          getWidthSetting
            .map { maxWidth =>
              SourceParser[SourceFile]
                .parse(text)
                .map { parsed =>
                  // Adding a trailing newline because it's the right thing to do.
                  val formatted = Formatter[SourceFile].format(parsed, maxWidth).strip + "\n"

                  List(
                    TextEdit.Overwrite(
                      formatted,
                      SourceRange.forEntireString(text),
                    )
                  )
                }
                // doesn't parse, we won't format
                .getOrElse(Nil)
            }
        }

}
