package task2

import cats.{ Eq, Show }
import cats.syntax.all._

import scala.annotation.tailrec

/** Represents an expression of TL language defined as follows:
  * {{{
  * TREE ::= NODE | ID
  * NODE ::= '(' TREE* ')'
  * ID   ::= [a-zA-Z]+
  * }}}
  */
enum TLExpr {
  case Id(name: String)
  case Node(children: List[TLExpr])

  /** Eliminator for `TLExpr`.
    * @param idCase
    *   Function to apply if the expression is an `Id`.
    * @param nodeCase
    *   Function to apply if the expression is a `Node`.
    * @return
    *   Result of eliminating with the given functions.
    */
  def fold[B](idCase: Id => B, nodeCase: (Node, List[B]) => B): B =
    TLExpr.fold(this)(idCase, nodeCase)

  /** Breadth-first traversal of the expression.
    * @return
    *   Iterator of subexpressions (including this) in breadth-first order.
    */
  def bfs: Iterator[TLExpr] = TLExpr.bfs(this)

  /** Size of the expression.
    * @return
    *   Number of nodes (both `Id` and `Node`) in the expression.
    */
  def size: Int = TLExpr.size(this)

  /** Replace all subexpressions equal to `search` with `replace`.
    * @param search
    *   Subexpression to search for.
    * @param replace
    *   Subexpression to replace with.
    * @return
    *   Expression with all occurrences of `search` replaced with `replace`.
    * @note
    *   If one replacement causes `search` to appear higher in the expression, replacement will be performed again.
    * @note
    *   If `replace` contains `search` as a subtree, replacement inside `replace` will not be performed.
    */
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

  /** Parser for `TLExpr` according to the following grammar:
    * {{{
    * TREE ::= NODE | ID
    * NODE ::= '(' TREE* ')'
    * ID   ::= [a-zA-Z]+
    * }}}
    * @return
    *   `Parser` for `TLExpr`.
    */
  def parser: Parser[TLExpr] =
    Parser.defer(Parser.or(idParser, nodeParser))

  /** Parse a string into a `TLExpr`.
    * @param input
    *   The string to parse.
    * @return
    *   Either a parsing error or the parsed `TLExpr`.
    * @note
    *   The parser should consume the whole input, otherwise it will fail.
    */
  def parse(input: String): Either[Parser.Error, TLExpr] =
    Parser.parseAll(parser, input)

  /** Eliminator for `TLExpr`.
    * @param expr
    *   Expression to eliminate.
    * @param idCase
    *   Function to apply if the expression is an `Id`.
    * @param nodeCase
    *   Function to apply if the expression is a `Node`.
    * @return
    *   Result of eliminating with the given functions.
    */
  def fold[B](expr: TLExpr)(
      idCase: Id => B,
      nodeCase: (Node, List[B]) => B
  ): B = {
    // Tail-recursive implementation of the fold.
    // Nodes are marked with `Left` to indicate that they were already visited.
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

  /** Breadth-first traversal of the expression.
    * @param expr
    *   Expression to traverse.
    * @return
    *   Iterator of subexpressions (including `expr`) in breadth-first order.
    */
  def bfs(expr: TLExpr): Iterator[TLExpr] =
    fold(expr)(
      Iterator.single[TLExpr],
      (n, children) => Iterator.single(n).concat(children.iterator.flatten)
    )

  /** Size of the expression.
    * @param expr
    *   Expression to calculate the size of.
    * @return
    *   Number of nodes (both `Id` and `Node`) in the expression.
    */
  def size(expr: TLExpr): Int =
    fold(expr)(
      _ => 1,
      (_, children) => children.sum + 1
    )

  /** Replace all subexpressions equal to `search` with `replace`.
    * @param expr
    *   Expression to replace in.
    * @param search
    *   Subexpression to search for.
    * @param replace
    *   Subexpression to replace with.
    * @return
    *   Expression with all occurrences of `search` in `expr` replaced with `replace`.
    * @note
    *   If one replacement causes `search` to appear higher in the expression, replacement will be performed again.
    * @note
    *   If `replace` contains `search` as a subtree, replacement inside `replace` will not be performed.
    */
  def replace(expr: TLExpr)(
      search: TLExpr,
      replace: TLExpr
  ): TLExpr = {
    val searchSize  = search.size
    val replaceSize = replace.size

    // Optimization: count size of the subtrees while folding
    // to avoid unnecessary comparisons with `search`.
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

  // Equality instance for `TLExpr`.
  given Eq[TLExpr] with {
    def eqv(left: TLExpr, right: TLExpr): Boolean =
      bfs(left).zip(bfs(right)).forall {
        case (Id(name1), Id(name2)) => name1 == name2
        case (Node(children1), Node(children2)) =>
          children1.length == children2.length
        case _ => false
      }
  }

  // Show instance for `TLExpr`.
  given Show[TLExpr] with {
    def show(expr: TLExpr): String =
      fold(expr)(
        id => id.name,
        (_, children) => children.mkString("(", " ", ")")
      )
  }
}
