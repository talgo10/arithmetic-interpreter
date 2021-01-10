package talgo.ai

import scala.util.{Failure, Success, Try}

trait ParserLike {
  def parse(s: String): Either[Throwable, Expression]
}

object Parser extends ParserLike {
  type InternalParserLike[T] = PartialFunction[String, T]

  // Regex
  val numberRegex: String = "0|[-]?[1-9][0-9]*"

  val BinaryOperatorFirstPrecedenceRegex = """(.+)\s+([*])\s+(.+)""".r
  val BinaryOperatorSecondPrecedenceRegex = """(.+)\s+([+\-])\s+(.+)""".r
  val IntRegex = s"[\\s]*(${numberRegex})[\\s]*".r
  val VariableRefRegex = "[\\s]*([a-zA-Z]+)[\\s]*".r
  val AfterPlusPlusOperatorRegex = "[\\s]*([a-zA-Z]+)\\+\\+".r
  val BeforePlusPlusOperatorRegex = "[\\s]*\\+\\+([a-zA-Z]+)".r
  val NegativeOperatorRegex = "[\\s]*-([a-zA-Z]+)".r
  val AssignmentRegex = "[\\s]*([a-zA-Z]+)[\\s]*([\\/\\+\\-\\*]?)=[\\s]*(.+)".r

  val parseBinaryOperatorExpr: InternalParserLike[BinaryOperatorExpr] = {
    case "+" => Plus
    case "-" => Minus
    case "*" => Multiple
  }

  val parseBinaryOperationExpr: InternalParserLike[BinaryOperationExpr] = {
    case BinaryOperatorSecondPrecedenceRegex(left, op, right) =>
      BinaryOperationExpr(parseBinaryOperatorExpr(op), parseExpr(left), parseExpr(right))
    case BinaryOperatorFirstPrecedenceRegex(left, op, right) =>
      BinaryOperationExpr(parseBinaryOperatorExpr(op), parseExpr(left), parseExpr(right))
  }

  val parseVariableRef: InternalParserLike[VariableRefExpr] = {
    case VariableRefRegex(v) => VariableRefExpr(v)
  }

  val parseIntExpr: InternalParserLike[IntExpr] = {
    case IntRegex(l) => IntExpr(l.toInt)
  }

  val parseUnaryOperationExpr: InternalParserLike[UnaryOperationExpr] = {
    case AfterPlusPlusOperatorRegex(v) => UnaryOperationExpr(AfterPlusPlus, parseVariableRef(v))
    case BeforePlusPlusOperatorRegex(v) => UnaryOperationExpr(BeforePlusPlus, parseVariableRef(v))
    case NegativeOperatorRegex(v) => UnaryOperationExpr(Negative, parseVariableRef(v))
  }

  val parseAssignment: InternalParserLike[AssignmentExpr] = {
    case AssignmentRegex(v, compAss, right) =>
      val optCompoundOperator: Option[BinaryOperatorExpr] = if (compAss.isEmpty) None else Some(parseBinaryOperatorExpr(compAss))
      AssignmentExpr(parseVariableRef(v), parseExpr(right), optCompoundOperator)
  }

  def parseExpr: InternalParserLike[Expression] = {
    parseAssignment orElse
      parseBinaryOperationExpr orElse
      parseUnaryOperationExpr orElse
      parseIntExpr orElse
      parseVariableRef orElse {
      case unknownExp =>  throw UnknownParseExpression(unknownExp)
    }
  }

  def parse(s: String): Either[Throwable, Expression] = {
    Try(parseAssignment(s))
      .toEither
      .left.map { t => ParsingError(s"Failed to parse the following line: ${s}, cause: ${t.getMessage}", t) }
  }
}

case class ParsingError(msg: String, cause: Throwable) extends Exception(msg, cause)
case class UnknownParseExpression(input: String) extends RuntimeException(s"unknown expression `${input}`")