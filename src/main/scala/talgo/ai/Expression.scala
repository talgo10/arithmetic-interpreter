package talgo.ai

sealed trait Expression
case class IntExpr(n: Int) extends Expression
case class VariableRefExpr(name: String) extends Expression
case class BinaryOperationExpr(op: BinaryOperatorExpr, leftOperand: Expression, rightOperand: Expression) extends Expression
case class UnaryOperationExpr(op: UnaryOperatorExpr, operand: VariableRefExpr) extends Expression


case class AssignmentExpr(left: VariableRefExpr, right: Expression, optCompoundOperator: Option[BinaryOperatorExpr] = None) extends Expression

sealed trait BinaryOperatorExpr extends Expression
case object Plus extends BinaryOperatorExpr
case object Minus extends BinaryOperatorExpr
case object Multiple extends BinaryOperatorExpr

sealed trait UnaryOperatorExpr extends Expression
case object BeforePlusPlus extends UnaryOperatorExpr
case object AfterPlusPlus extends UnaryOperatorExpr
case object Negative extends UnaryOperatorExpr