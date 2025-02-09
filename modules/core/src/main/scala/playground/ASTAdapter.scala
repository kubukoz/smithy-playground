package playground

import cats.syntax.all.*
import playground.smithyql.QualifiedIdentifier

object ASTAdapter {

  def decodeQI(qi: playground.generated.nodes.QualifiedIdentifier): Option[QualifiedIdentifier] =
    (qi.namespace.map(_.source).toNel, qi.selection.map(_.source))
      .mapN(QualifiedIdentifier.apply)

}
