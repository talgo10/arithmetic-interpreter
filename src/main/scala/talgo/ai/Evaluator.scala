package talgo.ai

import scala.util.Try


trait EvaluatorLike {
  def eval(exp: Expression, ctx: Context): Either[EvaluationError, (IntExpr, Context)]
}

case class UnknownEvalExpression(exp: Expression) extends RuntimeException(s"Unknown expression `${exp}`")
case class EvaluationError(msg: String, cause: Throwable) extends Exception(msg, cause)

object Evaluator extends EvaluatorLike {
  private def evalExpr(exp: Expression, ctx: Context): (IntExpr, Context) = {
    exp match {
      case AssignmentExpr(variableRef, rightExpr, None) =>
        val (rightValue, rightCtx) = evalExpr(rightExpr, ctx)
        val updatedCtx = rightCtx.set(variableRef, rightValue)
        rightValue -> updatedCtx

      case AssignmentExpr(left, right, Some(optCompoundOperator)) =>
        evalExpr(AssignmentExpr(left, BinaryOperationExpr(optCompoundOperator, right, left) , None), ctx)

      case vr@VariableRefExpr(_) => ctx.tryGet(vr).get -> ctx

      case i@IntExpr(_) => i -> ctx

      case BinaryOperationExpr(op, leftOperand, rightOperand) =>
        // Eval left side first
        val (leftValue, leftCtx) = evalExpr(leftOperand, ctx)
        val (rightValue, rightCtx) = evalExpr(rightOperand, leftCtx)

        val evaluatedValue = op match {
          case Plus => IntExpr(leftValue.n + rightValue.n)
          case Minus => IntExpr(leftValue.n - rightValue.n)
          case Multiple => IntExpr(leftValue.n * rightValue.n)
        }

        evaluatedValue -> rightCtx

      case UnaryOperationExpr(BeforePlusPlus, varExp) =>
        val (updatedValue, updatedCtx) = evalExpr(AssignmentExpr(varExp, IntExpr(1), Some(Plus)), ctx)
        updatedValue -> updatedCtx

      case UnaryOperationExpr(AfterPlusPlus, varExp) =>
        val beforeUpdateValue = ctx.tryGet(varExp).get
        val (_, updatedCtx) = evalExpr(AssignmentExpr(varExp, IntExpr(1), Some(Plus)), ctx)
        beforeUpdateValue -> updatedCtx

      case UnaryOperationExpr(Negative, vr) => evalExpr(BinaryOperationExpr(Multiple, vr, IntExpr(-1)), ctx)

      case unknownExp => throw UnknownEvalExpression(unknownExp)
    }
  }

  override def eval(exp: Expression, ctx: Context): Either[EvaluationError, (IntExpr, Context)] = {
    Try(evalExpr(exp, ctx))
      .toEither
      .left.map { t => EvaluationError(s"Failed to eval expression, cause: ${t.getMessage}", t) }
  }
}
