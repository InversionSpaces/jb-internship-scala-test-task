import scala.annotation.tailrec

package object task1 {
  def f(x: Int): Int =
    if (x <= 0) 1
    else f(x - 1) + f(x - 1)

  def fOptimized(x: Int): Int = {
    @tailrec
    def fast_pow(base: Int, p: Int, acc: Int = 1): Int =
      if (p <= 0) acc
      else {
        val nAcc = if (p % 2 == 0) acc else base * acc

        fast_pow(base * base, p / 2, nAcc)
      }

    fast_pow(2, x)
  }
}
