package talgo.ai

import org.scalatest.{FunSuite, Matchers}

class InterpreterTest extends FunSuite with Matchers {

  test("preserve context state between runBlock method calls") {
    // Input
    val interpreter = Interpreter.apply()

    // Test
    // First block
    interpreter.runBlock(Iterator("x = 1"))
    // Second block
    val actualContext = interpreter.runBlock(Iterator("x += 1"))

    // Expected
    val expectedContext = Context.empty.set(VariableRefExpr("x"), IntExpr(2))

    // Validation
    actualContext shouldBe expectedContext
  }

  test("bad statements are skipped in runBlock method calls") {
    // Input
    val interpreter = Interpreter.apply()

    // Test
    val actualContext = interpreter.runBlock(Iterator("x = 1", "bad statement", "y = x + 1"))

    // Expected
    val expectedContext = Context.empty
      .set(VariableRefExpr("x"), IntExpr(1))
      .set(VariableRefExpr("y"), IntExpr(2))

    // Validation
    actualContext shouldBe expectedContext
  }
}
