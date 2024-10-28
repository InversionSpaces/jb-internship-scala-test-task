package task2

import cats.{ Eq, Show }

import scala.annotation.tailrec

enum TLExpr {
  case Id(name: String)
  case Node(children: List[TLExpr])
}

object TLExpr {
  def fold[B](expr: TLExpr)(
      idCase: Id => B,
      nodeCase: (Node, List[B]) => B
  ): B = {
    @tailrec
    def loop(stack: List[Either[Node, TLExpr]], acc: List[B]): B =
      stack match {
        case Right(id: Id) :: tail =>
          loop(tail, idCase(id) :: acc)
        case Right(n @ Node(children)) :: tail =>
          val newStack = children.map(Right(_)) ++ (Left(n) :: tail)
          loop(newStack, acc)
        case Left(n) :: tail =>
          val (children, rest) = acc.splitAt(n.children.length)
          loop(tail, nodeCase(n, children.reverse) :: rest)
        case Nil =>
          acc.head
      }

    loop(List(Right(expr)), Nil)
  }

  def bfs(expr: TLExpr): Iterator[TLExpr] =
    fold(expr)(
      Iterator.single[TLExpr],
      (n, children) => Iterator.single(n).concat(children.iterator.flatten)
    )

  given Eq[TLExpr] with {
    def eqv(left: TLExpr, right: TLExpr): Boolean =
      bfs(left).zip(bfs(right)).forall {
        case (Id(name1), Id(name2)) => name1 == name2
        case (Node(children1), Node(children2)) =>
          children1.length == children2.length
        case _ => false
      }
  }

  given Show[TLExpr] with {
    def show(expr: TLExpr): String =
      fold(expr)(
        id => id.name,
        (_, children) => children.mkString("(", " ", ")")
      )
  }
}
