package playground

import cats.data.Ior
import cats.syntax.all.*
import weaver.*

object IorUtilsTests extends FunSuite {

  case class Case(
    lhs: Ior[Int, String],
    rhs: Ior[Int, String],
    expected: Ior[Int, String],
  )

  List(
    Case(10.leftIor, 15.leftIor, 25.leftIor),
    Case(10.leftIor, "success".rightIor, Ior.both(10, "success")),
    Case(10.leftIor, Ior.both(15, "success"), Ior.both(25, "success")),
    Case(Ior.both(10, "success"), 15.leftIor, Ior.both(25, "success")),
    Case(Ior.both(10, "success"), "success 2".rightIor, Ior.both(10, "success")),
    Case(Ior.both(10, "success"), Ior.both(15, "success 2"), Ior.both(25, "success")),
    Case("success".rightIor, 15.leftIor, Ior.both(15, "success")),
    Case("success".rightIor, "success 2".rightIor, "success".rightIor),
    Case("success".rightIor, Ior.both(15, "success 2"), Ior.both(15, "success")),
  )
    .foreach { testCase =>
      test(s"orElseCombine(${testCase.lhs}, ${testCase.rhs})") {
        val result = IorUtils.orElseCombine(testCase.lhs, testCase.rhs)
        assert(result === testCase.expected)
      }
    }
}
