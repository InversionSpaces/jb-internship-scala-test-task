import scala.annotation.tailrec

package object task1 {
  /**
   * Naive implementation of `f(x) = f(x) + f(x); f(0) = 1`
   * which computes `f(x)` in `O(2^x)` time.
   * @param x `x`
   * @return `f(x)`
   */
  def f(x: Int): BigInt =
    if (x <= 0) 1
    else f(x - 1) + f(x - 1)

  /**
   * Optimized implementation of `f(x) = f(x) + f(x); f(0) = 1`
   * which computes `f(x) = 2^x` in `O(log(x))` time.
   * @param x `x`
   * @return `f(x) = 2^x`
   */
  def fOptimized(x: Int): BigInt = {
    // Fast pow computing `acc * base^p`
    @tailrec
    def fast_pow(base: BigInt, p: BigInt, acc: BigInt = 1): BigInt =
      if (p <= 0) acc
      else {
        val nAcc = if (p % 2 == 0) acc else base * acc

        fast_pow(base * base, p / 2, nAcc)
      }

    fast_pow(2, x)
  }

  /**
   * Fastest implementation of `f(x) = f(x) + f(x); f(0) = 1`
   * which computes `f(x) = 2^x` in `O(1)` time.
   * @param x `x`
   * @return `f(x) = 2^x`
   */
  def fFastest(x: Int): BigInt = BigInt(1) << x
}
