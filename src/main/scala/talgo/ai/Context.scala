package talgo.ai

import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}

object Context {
  def empty: Context = Context(memory = ListMap.empty)
}

case class UnknownVariableRef(vr: VariableRefExpr) extends RuntimeException(s"Unknown variable reference `${vr.name}`")

case class Context(memory: ListMap[VariableRefExpr, IntExpr]) { self =>
  def set(v: VariableRefExpr, someValue: IntExpr): Context = self.copy(memory = self.memory.updated(v, someValue))
  def tryGet(v: VariableRefExpr): Try[IntExpr] = self.memory.get(v) match {
    case Some(v) => Success(v)
    case None => Failure(UnknownVariableRef(v))
  }

  def pretty: String = {
    self.memory.iterator.map { case (k,v) => s"${k.name}=${v.n}" }.mkString("{", ", ", "}")
  }
}