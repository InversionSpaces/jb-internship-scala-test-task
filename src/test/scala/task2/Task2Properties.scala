package task2

import org.scalacheck.{ Gen, Properties }
import org.scalacheck.Prop.forAll
import cats.syntax.all.*

import scala.annotation.tailrec

object Task2Properties extends Properties("Task2") {
  // Limit length of ids to 3 for tests
  val shortId: Gen[String] = Gen.choose(1, 3).flatMap(n => Gen.stringOfN(n, Gen.alphaNumChar))
  val tlId: Gen[TLExpr.Id] = shortId.map(TLExpr.Id.apply)
  val tlExpr: Gen[TLExpr] = Gen.recursive { recurse =>
    Gen.sized(n =>
      if (n <= 0) tlId
      else Gen.oneOf(tlId, Gen.listOf(Gen.resize(n / 2, recurse)).map(TLExpr.Node.apply))
    )
  }
  val tlNode: Gen[TLExpr.Node] = Gen.listOf(tlExpr).map(TLExpr.Node.apply)

  val exprSubExpr: Gen[(TLExpr, TLExpr)] =
    Gen.resize(
      40, // Resize to avoid big examples and long run times
      for {
        expr <- tlExpr
        subs = expr.bfs.toList.drop(1)
        if subs != Nil
        subExpr <- Gen.oneOf(subs)
      } yield (expr, subExpr)
    )

  val exprSubExprReplace: Gen[(TLExpr, TLExpr, TLExpr)] = Gen.resize(
    40, // Resize to avoid big examples and long run times
    for {
      (expr, subExpr) <- exprSubExpr
      replace         <- tlExpr
      if replace.bfs.forall(_ =!= subExpr) && expr.bfs.forall(_ =!= replace)
    } yield (expr, subExpr, replace)
  )

  property("TLExpr.parse(expr.show) == Right(expr)") = forAll(tlExpr) { expr =>
    TLExpr.parse(expr.show) == Right(expr)
  }

  property("subexpr.show is in expr.show") = forAll(tlExpr) { expr =>
    val repr = expr.show
    expr.bfs.forall { s =>
      repr.contains(s.show)
    }
  }

  property("TLExpr.parse(expr.show.remove(subexpr.show)) succeeds") = forAll(exprSubExpr) { (expr, subexpr) =>
    val exprStr    = expr.show
    val subexprStr = subexpr.show
    val newExprStr = exprStr.replace(subexprStr, "")
    TLExpr.parse(newExprStr).isRight
  }

  property("expr.replace(subexpr, subexpr) == expr") = forAll(exprSubExpr) { case (expr, subexpr) =>
    expr.replace(subexpr, subexpr) === expr
  }

  // Assuming that subexpr not in replace
  property("subexpr not in expr.replace(subexpr, replace)") = forAll(exprSubExprReplace) {
    case (expr, subexpr, replace) =>
      expr.replace(subexpr, replace).bfs.forall(_ =!= subexpr)
  }

  // Assuming that replace not in expr and subexpr not in replace
  property("count subexpr in expr == count replace in expr.replace(subexpr, replace)") = forAll(exprSubExprReplace) {
    case (expr, subexpr, replace) =>
      expr.bfs.count(_ === subexpr) == expr.replace(subexpr, replace).bfs.count(_ === replace)
  }

  property("expr.replace(subexpr, expr) has n^2 subexpr") = forAll(exprSubExpr) { case (expr, subexpr) =>
    val n        = expr.bfs.count(_ === subexpr)
    val replaced = expr.replace(subexpr, expr)
    replaced.bfs.count(_ === subexpr) === n * n
  }
}
