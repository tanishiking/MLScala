import org.scalatest.{Matchers, FlatSpec}
import mlscala.Parser._
import mlscala.Ast._


class ParserSpec extends FlatSpec with Matchers {

  behavior of "Parser.scala"

  it should "error" in {
    assert(parse("without semisemi").getOrElse("error") == "error")
  }

  it should "Var(a)" in {
    assert(parse("a;;").get == Exp(Var("a")))
    assert(parse("test;;").get == Exp(Var("test")))
  }

  it should "BLit(true) or BLit(false)" in {
    assert(parse("true;;").get == Exp(BLit(true)))
    assert(parse("false;;").get == Exp(BLit(false)))
  }

  it should "ILit(num)" in {
    assert(parse("1;;").get == Exp(ILit(1)))
    assert(parse("100;;").get == Exp(ILit(100)))
  }

  it should "LessThan" in {
    assert(parse("1 < 2;;").get == Exp(BinOp(Lt, ILit(1), ILit(2))))
    assert(parse("1<2;;").get == Exp(BinOp(Lt, ILit(1), ILit(2))))
  }

  it should "plus" in {
    assert(parse("1 + 2;;").get == Exp(BinOp(Plus, ILit(1), ILit(2))))
  }

  it should "mult" in {
    assert(parse("1 * 2;;").get == Exp(BinOp(Mult, ILit(1), ILit(2))))
  }

  it should "nested binop" in {
    assert(parse("(1 + 1) + 1;;").get == Exp(BinOp(Plus,BinOp(Plus,ILit(1),ILit(1)),ILit(1))))
  }

  it should "if" in {
    assert(parse("if true then 1 else 0;;").get == Exp(IfExp(BLit(true),ILit(1),ILit(0))))
  }

}
