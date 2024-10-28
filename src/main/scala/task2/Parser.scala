package task2

import cats.MonadError
import cats.syntax.all._
import Parser.*

import scala.annotation.tailrec

trait Parser[+A] {
  def parse(ctx: Ctx): Either[Error, (Ctx, A)]
}

object Parser {

  def parseAll[A](parser: Parser[A], input: String): Either[Error, A] =
    parser.parse(Ctx(input, 0)).flatMap { case (ctx, a) =>
      if (ctx.isEnd) Right(a)
      else Left(Error.expEOF(ctx.pos))
    }

  final case class Error(
      pos: Int,
      expectation: Set[Error.Expectation]
  ) {
    def unite(that: Error): Error =
      if (that.pos > pos) that
      else if (that.pos < pos) this
      else Error(pos, expectation ++ that.expectation)
  }

  object Error {
    enum Expectation {
      case Symbol(c: Char)
      case SymbolClass(name: String)
      case EOF
    }

    def expSymbol(pos: Int, c: Char): Error = Error(pos, Set(Expectation.Symbol(c)))

    def expClass(pos: Int, name: String): Error = Error(pos, Set(Expectation.SymbolClass(name)))

    def expEOF(pos: Int): Error = Error(pos, Set(Expectation.EOF))
  }

  final case class Ctx(
      input: String,
      pos: Int
  ) {
    def isEnd: Boolean = pos >= input.length

    def cur: Option[Char] = if (isEnd) None else Some(input(pos))

    def moveTo(pos: Int): Ctx = Ctx(input, pos)

    def skip(n: Int = 1): Ctx = moveTo(pos + n)

    def end: Ctx = moveTo(input.length)
  }

  def char(c: Char): Parser[Char] = (ctx: Ctx) =>
    ctx.cur
      .filter(_ == c)
      .fold(
        Left(Error.expSymbol(ctx.pos, c))
      )(_ => Right((ctx.skip(), c)))

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

  def whitespace: Parser[Unit] = (ctx: Ctx) => {
    val end = (ctx.pos until ctx.input.length)
      .find(i => !ctx.input(i).isWhitespace)
      .getOrElse(ctx.input.length)

    Right((ctx.moveTo(end), ()))
  }

  def or[A](left: Parser[A], right: Parser[A]): Parser[A] =
    left.recoverWith(error => right.adaptError(_.unite(error)))

  def prod[A, B](left: Parser[A], right: Parser[B]): Parser[(A, B)] =
    left.product(right)

  def rep[A](p: Parser[A]): Parser[List[A]] =
    (false, List.empty[A])
      .iterateUntilM { case (_, acc) =>
        p.map(a => (false, a :: acc)).handleError(_ => (true, acc))
      } { case (stop, _) => stop }
      .map { case (_, acc) => acc.reverse }

  def defer[A](parser: => Parser[A]): Parser[A] =
    (ctx: Ctx) => parser.parse(ctx)

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
