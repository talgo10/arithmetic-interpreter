package talgo.ai

import org.scalatest.{Matchers, WordSpec}

class ParserTest extends WordSpec with Matchers {

  // valid
  // - make sure ordering of building the tree (greedy)
  // - make sure about negative numbers
  // - using pattern matching, make sure any existing expression has a "valid" "happy" test to ensure it has parser for it

  // invalid inputs/ unsupported
  // - undefined operations
  // - undefined literals (not an int, double?, starting with 0 ?)
  // - operators without operands (left or right)
  // - assignment to nothing
  // - assignment like `7 + x = y` or `x = y = 7`
  // - make sure assignment returns from the public parse method

  val testParseMethod: String => Either[Throwable, Expression] = Parser.parse _

  "parser parse() method" when {
    // Happy paths
    "input has a good binary operation expression" should {
      "return parsed object tree from right to left (greedy)" in {
        testParseMethod("y = x + 7 + 9") shouldBe Right {
          AssignmentExpr(
            VariableRefExpr("y"),
            BinaryOperationExpr(
              Plus,
              BinaryOperationExpr(Plus, VariableRefExpr("x"), IntExpr(7)),
              IntExpr(9)),
            optCompoundOperator = None)
        }
      }

      "return parsed object for mixed operators precedences" in {
        testParseMethod("y = 2 + 7 * 9") shouldBe Right {
          AssignmentExpr(
            VariableRefExpr("y"),
            BinaryOperationExpr(
              Plus,
              IntExpr(2),
              BinaryOperationExpr(Multiple, IntExpr(7), IntExpr(9))),
            optCompoundOperator = None)
        }
      }

      "return parsed object for negative numbers" in {
        testParseMethod("y = -x * -7") shouldBe Right {
          AssignmentExpr(
            VariableRefExpr("y"),
            BinaryOperationExpr(
              Multiple,
              UnaryOperationExpr(Negative, VariableRefExpr("x")),
              IntExpr(-7)),
            optCompoundOperator = None)
        }
      }
    }

    "input has a good compound assignment" should {
      "return parsed object as expected" in {
        testParseMethod("y += 1") shouldBe Right {
          AssignmentExpr(
            VariableRefExpr("y"),
            IntExpr(1),
            optCompoundOperator = Some(Plus))
        }
      }
    }

    // Bad paths
    "input has an unknown operator" should {
      "return an error" in {
        testParseMethod("x = 6 ^ 6").toTry.isFailure shouldBe true
      }
    }

    "input has an unknown literal" should {
      "return an error for dotted number" in {
        testParseMethod("x = 6.0 + 2").toTry.isFailure shouldBe true
      }

      "return an error for zero leading number" in {
        testParseMethod("x = 7 + 06").toTry.isFailure shouldBe true
      }

      "return an error for var ref mixed with digits" in {
        testParseMethod("x = y7 + 6").toTry.isFailure shouldBe true
      }
    }

    "input has an operator without one of the operands" should {
      "return an error" in {
        testParseMethod("x = 7 + ").toTry.isFailure shouldBe true
      }
    }

    "input is not an assignment expression" should {
      "return an error for operator expression only" in {
        testParseMethod("x + 2").toTry.isFailure shouldBe true
      }

      "return an error when left side is not a var ref" in {
        testParseMethod("x + 2 = y").toTry.isFailure shouldBe true
      }
    }

    "input has missing spaces between operands" should {
      "return an error" in {
        testParseMethod("y = x+2 - 6").toTry.isFailure shouldBe true
      }
    }

  }
}
