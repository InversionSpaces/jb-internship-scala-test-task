package task1

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll

object Task1Properties extends Properties("Task1") {
  val smallInt: Gen[Int] = Gen.choose(0, 25)
  val nonNegInt: Gen[Int] = Gen.choose(0, 1000)

  property("f(x) = fOptimized(x)") = {
    forAll (smallInt) { x =>
      f(x) == fOptimized(x)
    }
  }

  property("fOptimized(x) = fFastest(x)") = {
    forAll(nonNegInt) { x =>
      fOptimized(x) == fFastest(x)
    }
  }
}
