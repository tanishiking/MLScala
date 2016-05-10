import org.scalatest.{Matchers, FlatSpec}
import mlscala.Parser._
import mlscala.Ast._


class ParserSpec extends FlatSpec with Matchers {

  behavior of "Parser.scala"

  it should "error" in {
    assert(parse("without semisemi").getOrElse("error") == "error")
  }

  it should "Decl(x, ILit(1))" in {
    assert(parse("let x = 1;;").get == MultiDecl(List(Decl("x", ILit(1)))))
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

  it should "parseOK" in {
    assert(parse("1 && false;;").get == Exp(BinOp(And, ILit(1), BLit(false))))
  }

  it should "AndOp" in {
    assert(parse("true && false;;").get == Exp(BinOp(And, BLit(true), BLit(false))))
  }

  it should "BinOp Or" in {
    assert(parse("1 < 2 || 2 < 1;;").get == Exp(BinOp(Or, BinOp(Lt, ILit(1), ILit(2)), BinOp(Lt, ILit(2), ILit(1)))))
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

  it should "LetExp" in {
    assert(parse("let x = 1 in x;;").get == Exp(LetExp("x", ILit(1), Var("x"))))
  }

  it should "Let funExp" in {
    assert(parse("let addx = fun x -> x + 1;;").get == MultiDecl(List(Decl("addx", FunExp("x", BinOp(Plus, Var("x"), ILit(1)))))))
  }

  it should "return AppExp" in {
    assert(parse("addx 1;;").get == Exp(AppExp(Var("addx"), ILit(1))))
  }

  it should "equal multi arg funtion" in {
    assert(parse("fun x -> fun y -> x + y;;").get == parse("fun x y -> x + y;;").get)
    assert(parse("fun x -> fun y -> fun z -> (x + y) + z;;").get == parse("fun x y z -> (x + y) + z;;").get)
  }

}
