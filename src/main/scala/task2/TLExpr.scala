package task2

import cats.{ Eq, Show }
import cats.syntax.all._

import scala.annotation.tailrec

enum TLExpr {
  case Id(name: String)
  case Node(children: List[TLExpr])

  def fold[B](idCase: Id => B, nodeCase: (Node, List[B]) => B): B =
    TLExpr.fold(this)(idCase, nodeCase)

  def bfs: Iterator[TLExpr] = TLExpr.bfs(this)

  def size: Int = TLExpr.size(this)

  def replace(search: TLExpr, replace: TLExpr): TLExpr =
    TLExpr.replace(this)(search, replace)
}

object TLExpr {
  def idParser: Parser[Id] =
    (Parser.whitespace *> Parser.alphanum <* Parser.whitespace).map(Id.apply)

  def nodeParser: Parser[Node] = (
    Parser.whitespace *> Parser.char('(') *> Parser.whitespace *>
      Parser.rep(parser) <* Parser.char(')') <* Parser.whitespace
  ).map(Node.apply)

  def parser: Parser[TLExpr] = Parser.defer(
    Parser.or(idParser, nodeParser)
  )

  def parse(input: String): Either[Parser.Error, TLExpr] =
    Parser.parseAll(parser, input)

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

  def size(expr: TLExpr): Int =
    fold(expr)(
      _ => 1,
      (_, children) => children.sum + 1
    )

  def replace(expr: TLExpr)(
      search: TLExpr,
      replace: TLExpr
  ): TLExpr = {
    val searchSize  = search.size
    val replaceSize = replace.size

    val (_, result) = fold(expr)(
      (id: TLExpr) => if (id === search) (replaceSize, replace) else (1, id),
      (_, childrenRes) => {
        val (sizes, children) = childrenRes.unzip
        val newSize           = sizes.sum + 1
        val newNode           = Node(children)

        if (newSize == searchSize && newNode === search) (replaceSize, replace)
        else (newSize, newNode)
      }
    )

    result
  }

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
