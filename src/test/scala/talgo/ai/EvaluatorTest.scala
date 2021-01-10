package talgo.ai

import org.scalatest.{Matchers, WordSpec}

class EvaluatorTest extends WordSpec with Matchers {

  val testEvalMethod: (Expression, Context) => Either[EvaluationError, (IntExpr, Context)] = Evaluator.eval _

  "evaluator eval() method" when {
    // Happy Paths
    "eval binary expression" in {
      val inputExpr = BinaryOperationExpr(Plus, BinaryOperationExpr(Multiple, IntExpr(2), IntExpr(2)), IntExpr(2))
      testEvalMethod(inputExpr, Context.empty) shouldBe Right((IntExpr(6), Context.empty))
    }

    "eval var reference" in {
      val inputContext = Context.empty.set(VariableRefExpr("x"), IntExpr(1))
      val inputExpr = VariableRefExpr("x")
      testEvalMethod(inputExpr, inputContext) shouldBe Right((IntExpr(1), inputContext))
    }

    "eval assignment" in {
      val inputExpr = AssignmentExpr(VariableRefExpr("x"), IntExpr(2))
      testEvalMethod(inputExpr, Context.empty) shouldBe Right {
        (IntExpr(2), Context.empty.set(VariableRefExpr("x"), IntExpr(2)))
      }
    }

    "eval compound assignment +=" in {
      val inputContext = Context.empty.set(VariableRefExpr("x"), IntExpr(1))
      val inputExpr = AssignmentExpr(VariableRefExpr("x"), IntExpr(2), Some(Plus))
      testEvalMethod(inputExpr, inputContext) shouldBe Right {
        (IntExpr(3), Context.empty.set(VariableRefExpr("x"), IntExpr(3)))
      }
    }

    "eval override value" in {
      val inputContext = Context.empty.set(VariableRefExpr("x"), IntExpr(1))
      val inputExpr = AssignmentExpr(VariableRefExpr("x"), IntExpr(2))
      testEvalMethod(inputExpr, inputContext) shouldBe Right {
        (IntExpr(2), Context.empty.set(VariableRefExpr("x"), IntExpr(2)))
      }
    }

    "eval negative var reference" in {
      val inputContext = Context.empty.set(VariableRefExpr("x"), IntExpr(1))
      val inputExpr = UnaryOperationExpr(Negative, VariableRefExpr("x"))
      testEvalMethod(inputExpr, inputContext) shouldBe Right((IntExpr(-1), inputContext))
    }

    "eval ++i" in {
      val inputContext = Context.empty.set(VariableRefExpr("x"), IntExpr(1))
      val inputExpr = UnaryOperationExpr(BeforePlusPlus, VariableRefExpr("x"))
      testEvalMethod(inputExpr, inputContext) shouldBe Right((IntExpr(2), Context.empty.set(VariableRefExpr("x"), IntExpr(2))))
    }

    "eval i++" in {
      val inputContext = Context.empty.set(VariableRefExpr("x"), IntExpr(1))
      val inputExpr = UnaryOperationExpr(AfterPlusPlus, VariableRefExpr("x"))
      testEvalMethod(inputExpr, inputContext) shouldBe Right((IntExpr(1), Context.empty.set(VariableRefExpr("x"), IntExpr(2))))
    }

    // Bad Paths
    "return error when has an unknown var reference" in {
      val inputExpr = VariableRefExpr("x")
      testEvalMethod(inputExpr, Context.empty).isLeft shouldBe true
    }
  }
}
