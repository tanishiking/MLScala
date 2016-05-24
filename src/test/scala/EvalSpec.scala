import mlscala.Ast._
import mlscala.Eval._
import mlscala.EvalResult._
import org.scalatest.{Matchers, FlatSpec}

class EvalSpec extends FlatSpec with Matchers {

  behavior of "Eval.scala"

  it should "IntV(_)" in {
    assert(applyPrim(Plus, IntV(1), IntV(3)) === Right(IntV(4)))
    assert(applyPrim(Mult, IntV(3), IntV(3)) === Right(IntV(9)))
    assert(applyPrim(Minus, IntV(3), IntV(1)) === Right(IntV(2)))
  }

  it should "BoolV(_)" in {
    assert(applyPrim(Lt, IntV(1), IntV(5)) === Right(BoolV(true)))
    assert(applyPrim(Lt, IntV(10), IntV(5)) === Right(BoolV(false)))
  }

  it should "throw RuntimeException" in {
    assert(applyPrim(And, IntV(1), BoolV(true)).left.get.isInstanceOf[RuntimeException])
    assert(applyPrim(Or, IntV(1), BoolV(false)).left.get.isInstanceOf[RuntimeException])
    assert(applyPrim(Plus, BoolV(true), BoolV(false)).left.get.isInstanceOf[RuntimeException])
    assert(applyPrim(Mult, BoolV(true), IntV(2)).left.get.isInstanceOf[RuntimeException])
    assert(applyPrim(Lt, BoolV(true), IntV(2)).left.get.isInstanceOf[RuntimeException])
  }
}
