package talgo.ai

trait InterpreterLike { self =>
  def runBlock(inputLines: Iterator[String]): Context
  def printlnStdout(s: String): Unit
  def printlnStderr(s: String): Unit
  def readLine(): String

  def start: Unit = {
    printlnStdout("Insert expressions separated by [ENTER] and press another [ENTER] to evaluate the block:")
    while(true) {
      val inputLines = Iterator.continually(readLine()).takeWhile(_.nonEmpty)
      self.runBlock(inputLines)
    }
  }

}

object Interpreter {
  def apply(): Interpreter = new Interpreter(parser = Parser ,evaluator = Evaluator)
}

class Interpreter(parser: ParserLike, evaluator: EvaluatorLike) extends InterpreterLike { self =>
  private var ctx = Context.empty

  override def runBlock(inputLines: Iterator[String]): Context = {
    ctx = inputLines.zipWithIndex.foldLeft(ctx) { case (previousCtx, (readLine, lineIdx)) =>

      val eitherSuccessOrFailure = for {
        parsedExp <- parser.parse(readLine)
        pair <- evaluator.eval(parsedExp, previousCtx)
        (result, updatedCtx) = pair
      } yield updatedCtx

      eitherSuccessOrFailure match {
        case Left(error) =>
          printlnStderr(s"Error in line ${lineIdx+1}: `${readLine}`; ${error.getMessage}")
          previousCtx
        case Right(updatedCtx) =>
          updatedCtx
      }
    }
    printlnStdout(ctx.pretty)
    ctx
  }

  override def printlnStdout(s: String): Unit = println(s)
  override def printlnStderr(s: String): Unit = Console.err.println(s)
  override def readLine(): String = scala.io.StdIn.readLine()
}
