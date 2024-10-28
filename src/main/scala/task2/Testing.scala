package task2

import cats.syntax.all._

object Testing extends App {
  val tree    = TLExpr.parse("(a (a ()))").getOrElse(throw new Exception("Failed to parse"))
  val pattern = TLExpr.parse("(a ())").getOrElse(throw new Exception("Failed to parse"))
  val replace = TLExpr.parse("()").getOrElse(throw new Exception("Failed to parse"))

  val result = TLExpr.replace(tree)(pattern, replace)

  println(result.show)
}
