package task2

import cats.MonadError
import cats.syntax.all._
import Parser.*

import scala.annotation.tailrec

trait Parser[A] {
  def parse(ctx: Ctx): Either[Error, (Ctx, A)]
}

object Parser {
  final case class Error()

  final case class Ctx(
      private val input: String,
      private val pos: Int
  ) {
    def isEnd: Boolean = pos >= input.length

    def cur: Option[Char] = if (isEnd) None else Some(input(pos))

    def moveTo(pos: Int): Ctx = Ctx(input, pos)

    def skip(n: Int = 1): Ctx = moveTo(pos + n)
  }

  def char(c: Char): Parser[Char] = (ctx: Ctx) =>
    ctx.cur.fold(Left(Error()))(ch =>
      if (ch == c) Right((ctx.skip(), c))
      else Left(Error())
    )

  def rep[A](p: Parser[A]): Parser[List[A]] =
    (false, List.empty[A])
      .iterateUntilM { case (_, acc) =>
        p.map(a => (false, a :: acc)).handleError(_ => (true, acc))
      } { case (stop, _) => stop }
      .map { case (_, acc) => acc }

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
