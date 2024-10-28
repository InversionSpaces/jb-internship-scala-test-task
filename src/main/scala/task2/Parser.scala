package task2

import cats.MonadError
import cats.syntax.all._
import scala.annotation.tailrec

/** Simple parser combinator for recursive descent parsing.
  * @tparam A
  *   the type of the result.
  */
trait Parser[+A] {

  /** Method that performs parsing.
    * @param ctx
    *   Current parsing context.
    * @return
    *   Either parsing error or new context and result.
    */
  def parse(ctx: Parser.Ctx): Either[Parser.Error, (Parser.Ctx, A)]
}

object Parser {

  /** Parses the input string using the given parser.
    * @param parser
    *   The parser to use.
    * @param input
    *   The input string.
    * @return
    *   The result of parsing.
    * @note
    *   Parsing will fail if the input string is not fully consumed.
    */
  def parseAll[A](parser: Parser[A], input: String): Either[Error, A] =
    parser.parse(Ctx(input, 0)).flatMap { case (ctx, a) =>
      if (ctx.isEnd) Right(a)
      else Left(Error.expEOF(ctx.pos))
    }

  /** Represents a parsing error.
    * @param pos
    *   The position in the input string where the error occurred.
    * @param expectation
    *   The set of expected symbols at the error position.
    */
  final case class Error(
      pos: Int,
      expectation: Set[Error.Expectation]
  ) {

    /** Unites two errors into one. Takes the error with the largest position or combines expectations if positions are
      * the same.
      * @param that
      *   The other error.
      * @return
      *   The united error.
      */
    def unite(that: Error): Error =
      if (that.pos > pos) that
      else if (that.pos < pos) this
      else Error(pos, expectation ++ that.expectation)
  }

  object Error {

    /** Represents an expectation of a parser.
      */
    enum Expectation {

      /** Expectation of a specific symbol.
        * @param c
        *   The expected symbol.
        */
      case Symbol(c: Char)

      /** Expectation of a symbol class. Exists for convenience.
        * @param name
        *   The name of the expected symbol class.
        */
      case SymbolClass(name: String)

      /** Expectation of the end of the input.
        */
      case EOF
    }

    def expSymbol(pos: Int, c: Char): Error = Error(pos, Set(Expectation.Symbol(c)))

    def expClass(pos: Int, name: String): Error = Error(pos, Set(Expectation.SymbolClass(name)))

    def expEOF(pos: Int): Error = Error(pos, Set(Expectation.EOF))
  }

  /** Represents the parsing context.
    * @param input
    *   The input string.
    * @param pos
    *   The current position in the input string.
    */
  final case class Ctx(
      input: String,
      pos: Int
  ) {

    /** @return
      *   True if the end of the input is reached, false otherwise.
      */
    def isEnd: Boolean = pos >= input.length

    /** @return
      *   The current character or None if the end of the input is reached.
      */
    def cur: Option[Char] = if (isEnd) None else Some(input(pos))

    /** @param pos
      *   New position.
      * @return
      *   New context with the position moved to the specified value.
      */
    def moveTo(pos: Int): Ctx = Ctx(input, pos)

    /** @param n
      *   Number of characters to skip.
      * @return
      *   New context with the position moved forward by n characters.
      */
    def skip(n: Int = 1): Ctx = moveTo(pos + n)

    /** @return
      *   New context with the position moved to the end of the input.
      */
    def end: Ctx = moveTo(input.length)
  }

  /** @param c
    *   The character to parse.
    * @return
    *   The parser for the specified character.
    */
  def char(c: Char): Parser[Char] = (ctx: Ctx) =>
    ctx.cur
      .filter(_ == c)
      .fold(
        Left(Error.expSymbol(ctx.pos, c))
      )(_ => Right((ctx.skip(), c)))

  /** Parser for non-empty alphanumeric string. Can be represented with combinators, but implemented directly as an
    * optimization.
    */
  def alphanum: Parser[String] = (ctx: Ctx) => {
    val end = (ctx.pos until ctx.input.length)
      .find(i => !ctx.input(i).isLetterOrDigit)
      .getOrElse(ctx.input.length)

    Either.cond(
      end != ctx.pos,
      (ctx.moveTo(end), ctx.input.substring(ctx.pos, end)),
      Error.expClass(ctx.pos, "alphanum")
    )
  }

  /** Parser for any (including empty) amount of whitespace characters. Can be represented with combinators, but
    * implemented directly as an optimization.
    */
  def whitespace: Parser[Unit] = (ctx: Ctx) => {
    val end = (ctx.pos until ctx.input.length)
      .find(i => !ctx.input(i).isWhitespace)
      .getOrElse(ctx.input.length)

    Right((ctx.moveTo(end), ()))
  }

  /** `or` parser combinator.
    * @param left
    *   The parser to try first.
    * @param right
    *   The parser to try if `left` fails.
    * @return
    *   The parser that tries `left` and if it fails, tries `right`.
    */
  def or[A](left: Parser[A], right: Parser[A]): Parser[A] =
    left.recoverWith(error => right.adaptError(_.unite(error)))

  /** `product` parser combinator.
    *
    * @param left
    *   The parser to run first.
    * @param right
    *   The parser to run second.
    * @return
    *   Parser that runs `left` and then `right` and returns a tuple of their results.
    */
  def prod[A, B](left: Parser[A], right: Parser[B]): Parser[(A, B)] =
    left.product(right)

  /** `rep` parser combinator
    * @param p
    *   parser to repeat
    * @return
    *   parser that repeats `p` zero or more times
    */
  def rep[A](p: Parser[A]): Parser[List[A]] =
    (false, List.empty[A])
      .iterateUntilM { case (_, acc) =>
        p.map(a => (false, a :: acc)).handleError(_ => (true, acc))
      } { case (stop, _) => stop }
      .map { case (_, acc) => acc.reverse }

  /** `defer` for lazy parser construction. Useful for recursive parsers.
    * @param parser
    *   Lazy parser construction.
    * @return
    *   Parser that defers the construction of the actual parser.
    */
  def defer[A](parser: => Parser[A]): Parser[A] =
    (ctx: Ctx) => parser.parse(ctx)

  // MonadError instance for Parser
  given MonadError[Parser, Error] with {
    def pure[A](a: A): Parser[A] = (ctx: Ctx) => Right((ctx, a))

    def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = (ctx: Ctx) =>
      fa.parse(ctx) match {
        case Left(err)          => Left(err)
        case Right((newCtx, a)) => f(a).parse(newCtx)
      }

    def raiseError[A](e: Error): Parser[A] = (_: Ctx) => Left(e)

    def handleErrorWith[A](fa: Parser[A])(f: Error => Parser[A]): Parser[A] = (ctx: Ctx) =>
      fa.parse(ctx) match {
        case Left(err)        => f(err).parse(ctx)
        case right @ Right(_) => right
      }

    final def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] = {
      @tailrec
      def impl(a: A, ctx: Ctx): Either[Error, (Ctx, B)] =
        f(a).parse(ctx) match {
          case Left(err)                    => Left(err)
          case Right((newCtx, Left(nextA))) => impl(nextA, newCtx)
          case Right((newCtx, Right(b)))    => Right((newCtx, b))
        }

      (ctx: Ctx) => impl(a, ctx)
    }
  }

}
