package task2

import cats.syntax.show._

object Testing extends App {
  val tree = TLExpr.Node(
    List(
      TLExpr.Id("a"),
      TLExpr.Node(
        List(
          TLExpr.Id("b"),
          TLExpr.Node(
            List(
              TLExpr.Id("c"),
              TLExpr.Id("d")
            )
          )
        )
      )
    )
  )

  println(TLExpr.bfs(tree).map(_.show).mkString("\n"))
//  val str = "aaabbb"
//
//  val parser = Parser.rep(Parser.char('a'))
//
//  val ctx = Parser.Ctx(str, 0)
//
//  val result = parser.parse(ctx)
//
//  println(result)
}
