package playground

import cats.Id
import cats.data.IorNel
import cats.data.NonEmptyList
import cats.implicits._
import playground.CompilationErrorDetails._
import playground.smithyql._
import playground.smithyql.format.Formatter
import smithy.api.TimestampFormat
import smithy4s.ShapeId

// this is more like "Diagnostic".
final case class CompilationError(
  err: CompilationErrorDetails,
  range: SourceRange,
  severity: DiagnosticSeverity,
  tags: Set[DiagnosticTag],
) {
  def deprecated: CompilationError = copy(tags = tags + DiagnosticTag.Deprecated)

  def isError: Boolean = severity == DiagnosticSeverity.Error
  def isWarning: Boolean = severity == DiagnosticSeverity.Warning
}

object CompilationError {

  type InIorNel[+A] = IorNel[CompilationError, A]

  def error(
    err: CompilationErrorDetails,
    range: SourceRange,
  ): CompilationError = default(err, range, DiagnosticSeverity.Error)

  def info(
    err: CompilationErrorDetails,
    range: SourceRange,
  ): CompilationError = default(err, range, DiagnosticSeverity.Information)

  def warning(
    err: CompilationErrorDetails,
    range: SourceRange,
  ): CompilationError = default(err, range, DiagnosticSeverity.Warning)

  def deprecation(
    info: DeprecatedInfo,
    range: SourceRange,
  ): CompilationError =
    CompilationError
      .warning(CompilationErrorDetails.DeprecatedItem(info), range)
      .deprecated

  def default(
    err: CompilationErrorDetails,
    range: SourceRange,
    severity: DiagnosticSeverity,
  ): CompilationError = CompilationError(
    err = err,
    range = range,
    severity = severity,
    tags = Set.empty,
  )

}

sealed trait DiagnosticSeverity extends Product with Serializable

object DiagnosticSeverity {
  case object Warning extends DiagnosticSeverity
  case object Error extends DiagnosticSeverity
  case object Information extends DiagnosticSeverity
}

sealed trait DiagnosticTag extends Product with Serializable

object DiagnosticTag {
  case object Deprecated extends DiagnosticTag
  case object Unused extends DiagnosticTag
}

sealed trait CompilationErrorDetails extends Product with Serializable {

  def render: String =
    this match {
      case Message(text) => text
      case UnsupportedProtocols(supported, available) =>
        val supportedString = supported.map(_.show).mkString_(", ")

        val availableString =
          if (available.isEmpty)
            "<none>"
          else
            available.map(_.show).mkString(", ")

        s"""Service doesn't support any of the available protocols: $supportedString.
           |Found protocols: $availableString
           |Running queries will not be possible.""".stripMargin

      case ParseError(expectationString) => s"Parsing failure: expected ${expectationString}"
      case DeprecatedItem(info) => "Deprecated" + CompilationErrorDetails.deprecationString(info)
      case InvalidUUID          => "Invalid UUID"
      case InvalidBlob          => "Invalid blob, expected base64-encoded string"
      case ConflictingServiceReference(_) => "Conflicting service references"

      case NumberOutOfRange(value, expectedType) => s"Number out of range for $expectedType: $value"
      case EnumFallback(enumName) =>
        s"""Matching enums by value is deprecated and may be removed in the future. Use $enumName instead.""".stripMargin
      case DuplicateItem => "Duplicate item - some entries will be dropped to fit in a set shape."
      case AmbiguousService(workspaceServices) =>
        s"""Couldn't determine service for this operation. Add a use clause, or use an explicit reference to specify the service you want to use.
           |Available services:""".stripMargin + workspaceServices
          .sorted
          .map(UseClause[Id](_).mapK(WithSource.liftId))
          .map(Formatter.useClauseFormatter.format(_, Int.MaxValue))
          .mkString_("\n", "\n", ".")

      case UnknownService(known) =>
        s"Unknown service. Known services: ${known.map(_.render).mkString(", ")}."

      case RefinementFailure(msg) => s"Refinement failed: $msg."

      case TypeMismatch(expected, actual) => s"Type mismatch: expected $expected, got $actual."

      case OperationMissing(validOperations) =>
        s"Operation not found. Available operations: ${validOperations.map(_.text).mkString_(", ")}."

      case MissingField(label) => s"Missing field $label."

      case InvalidTimestampFormat(expected) => s"Invalid timestamp format, expected $expected."

      case MissingDiscriminator(labels) =>
        s"wrong shape, this union requires one of: ${labels.mkString_(", ")}."

      case EmptyStruct(possibleValues) =>
        s"found empty struct, expected one of: ${possibleValues.mkString_(", ")}."

      case UnknownEnumValue(name, possibleValues) =>
        s"Unknown enum value: $name. Available values: ${possibleValues.mkString(", ")}"

      case StructMismatch(keys, possibleValues) =>
        s"struct mismatch (keys: ${keys.mkString_(", ")}), you must choose exactly one of: ${possibleValues
            .mkString_(", ")}."

      case UnexpectedField(remainingFields) =>
        val expectedRemainingString =
          if (remainingFields.isEmpty)
            ""
          else if (remainingFields.size == 1)
            s" Expected: ${remainingFields.head}."
          else
            s" Expected: one of ${remainingFields.mkString(", ")}."

        s"Unexpected field.$expectedRemainingString"
    }

}

object CompilationErrorDetails {

  def deprecationString(
    info: DeprecatedInfo
  ): String = {
    val reasonString = info.message.foldMap(": " + _)
    val sinceString = info.since.foldMap(" (since " + _ + ")")

    sinceString ++ reasonString
  }

  final case class ParseError(
    expectationString: String
  ) extends CompilationErrorDetails

  // todo: remove
  final case class Message(
    text: String
  ) extends CompilationErrorDetails

  final case class UnknownService(
    knownServices: List[QualifiedIdentifier]
  ) extends CompilationErrorDetails

  final case class UnsupportedProtocols(
    supported: NonEmptyList[ShapeId],
    available: List[ShapeId],
  ) extends CompilationErrorDetails

  final case class ConflictingServiceReference(
    refs: List[QualifiedIdentifier]
  ) extends CompilationErrorDetails

  final case class AmbiguousService(
    workspaceServices: List[QualifiedIdentifier]
  ) extends CompilationErrorDetails

  final case class TypeMismatch(
    expected: NodeKind,
    actual: NodeKind,
  ) extends CompilationErrorDetails

  final case class OperationMissing(
    validOperations: List[OperationName[Id]]
  ) extends CompilationErrorDetails

  final case class MissingField(
    label: String
  ) extends CompilationErrorDetails

  case object InvalidUUID extends CompilationErrorDetails

  final case class NumberOutOfRange(
    numberValue: String,
    typeName: String,
  ) extends CompilationErrorDetails

  final case class InvalidTimestampFormat(
    expected: TimestampFormat
  ) extends CompilationErrorDetails

  final case class MissingDiscriminator(
    possibleValues: NonEmptyList[String]
  ) extends CompilationErrorDetails

  final case class EmptyStruct(
    possibleValues: NonEmptyList[String]
  ) extends CompilationErrorDetails

  final case class UnknownEnumValue(
    value: String,
    possibleValues: List[String],
  ) extends CompilationErrorDetails

  final case class StructMismatch(
    keys: List[String],
    possibleValues: NonEmptyList[String],
  ) extends CompilationErrorDetails

  final case class UnexpectedField(
    remainingFields: List[String]
  ) extends CompilationErrorDetails

  final case class RefinementFailure(
    msg: String
  ) extends CompilationErrorDetails

  case object DuplicateItem extends CompilationErrorDetails

  case object InvalidBlob extends CompilationErrorDetails

  case class DeprecatedItem(
    info: DeprecatedInfo
  ) extends CompilationErrorDetails

  final case class EnumFallback(
    enumName: String
  ) extends CompilationErrorDetails

}
