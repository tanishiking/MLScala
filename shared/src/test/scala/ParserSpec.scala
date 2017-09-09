package mlscala

import org.scalatest.{FunSpec, Matchers}
import Ast._

class ParserSpec extends FunSpec with Matchers {
  describe("mlscala.Parser") {
    describe("topExpr") {
      describe("aExpr") {
        it ("can parse variable name") {
          Parser.statement.parse("a;;").get.value shouldBe TopExpr(Var("a"))
          Parser.statement.parse("test;;").get.value shouldBe TopExpr(Var("test"))
          Parser.statement.parse("test_piyo;;").get.value shouldBe TopExpr(Var("test_piyo"))
          Parser.statement.parse("Test_piyo;;").get.value shouldBe TopExpr(Var("Test_piyo"))
          Parser.statement.parse("t100;;").get.value shouldBe TopExpr(Var("t100"))
        }

        it("can parse variable name that contains reserved") {
          Parser.statement.parse("inc;;").get.value shouldBe TopExpr(Var("inc"))
          Parser.statement.parse("recv;;").get.value shouldBe TopExpr(Var("recv"))
          Parser.statement.parse("lets;;").get.value shouldBe TopExpr(Var("lets"))
        }

        it("can parse boolean expr") {
          Parser.statement.parse("true;;").get.value shouldBe TopExpr(BLit(true))
          Parser.statement.parse("false;;").get.value shouldBe TopExpr(BLit(false))
        }

        it("can parse number") {
          Parser.statement.parse("1;;").get.value shouldBe TopExpr(ILit(1))
          Parser.statement.parse("100;;").get.value shouldBe TopExpr(ILit(100))
          // TODO: test fail for input like 01
        }

        it("can parse parenthesis") {
          Parser.statement.parse("(1);;").get.value shouldBe TopExpr(ILit(1))
          Parser.statement.parse("(true);;").get.value shouldBe TopExpr(BLit(true))
          Parser.statement.parse("(a);;").get.value shouldBe TopExpr(Var("a"))
        }
      }

      describe("application") {
        it ("should return AppExp for argument with/without parenthesis") {
          Parser.statement.parse("print x;;").get.value shouldBe TopExpr(AppExp(Var("print"), Var("x")))
          Parser.statement.parse("print(x);;").get.value shouldBe TopExpr(AppExp(Var("print"), Var("x")))
        }
        it ("should return nested AppExp for multiple args") {
          Parser.statement.parse("add 1 2;;").get.value shouldBe TopExpr(AppExp(AppExp(Var("add"), ILit(1)), ILit(2)))
          Parser.statement.parse("add 1 2 3;;").get.value shouldBe TopExpr(AppExp(AppExp(AppExp(Var("add"), ILit(1)), ILit(2)), ILit(3)))
        }
      }

      describe("binary expr") {
        describe("multExpr") {
          it ("should return BinOp(Mult, ...)") {
            Parser.statement.parse("1 * 2;;").get.value shouldBe TopExpr(BinOp(Mult, ILit(1), ILit(2)))
            Parser.statement.parse("(1 * 1) * 1;;").get.value shouldBe TopExpr(BinOp(Mult, BinOp(Mult, ILit(1), ILit(1)), ILit(1)))
            Parser.statement.parse("1*2;;").get.value shouldBe TopExpr(BinOp(Mult, ILit(1), ILit(2)))
          }

          it ("can contain appExpr for lhs or/and rhs") {
            Parser.statement.parse("add 1 1 * add 1 1;;").get.value shouldBe
              TopExpr(BinOp(
                Mult,
                AppExp(AppExp(Var("add"), ILit(1)), ILit(1)),
                AppExp(AppExp(Var("add"), ILit(1)), ILit(1))
              ))

            Parser.statement.parse("(add 1 1) * (id 1);;").get.value shouldBe
              TopExpr(BinOp(
                Mult,
                AppExp(AppExp(Var("add"), ILit(1)), ILit(1)),
                AppExp(Var("id"), ILit(1))
              ))

            Parser.statement.parse("id (1 * 1) * (id 1);;").get.value shouldBe
              TopExpr(BinOp(
                Mult,
                AppExp(Var("id"), BinOp(Mult, ILit(1), ILit(1))),
                AppExp(Var("id"), ILit(1))
              ))
          }
        }

        describe("arithmeticExpr") {
          it ("should return BinOp(Plus ...") {
            Parser.statement.parse("1 + 2;;").get.value shouldBe TopExpr(BinOp(Plus, ILit(1), ILit(2)))
            Parser.statement.parse("1 + (2 * 2);;").get.value shouldBe TopExpr(BinOp(Plus, ILit(1), BinOp(Mult, ILit(2), ILit(2))))
            Parser.statement.parse("(1 * 2) + 2;;").get.value shouldBe TopExpr(BinOp(Plus, BinOp(Mult, ILit(1), ILit(2)), ILit(2)))
          }
          it ("should return BinOp(Minus ...") {
            Parser.statement.parse("1 - 2;;").get.value shouldBe TopExpr(BinOp(Minus, ILit(1), ILit(2)))
            Parser.statement.parse("(1 * 2) - 2;;").get.value shouldBe TopExpr(BinOp(Minus, BinOp(Mult, ILit(1), ILit(2)), ILit(2)))
          }
        }

        describe("lessthan") {
          it ("should return BinOp(Lt ...") {
            Parser.statement.parse("1 < 2;;").get.value shouldBe TopExpr(BinOp(Lt, ILit(1), ILit(2)))
            Parser.statement.parse("1 + 1 < 1;;").get.value shouldBe TopExpr(BinOp(Lt, BinOp(Plus, ILit(1), ILit(1)), ILit(1)))
          }
        }

        describe("or/and") {
          it ("should return BinOp(Or.. ") {
            Parser.statement.parse("true || false;;").get.value shouldBe TopExpr(BinOp(Or, BLit(true), BLit(false)))
            Parser.statement.parse("1 < 1 || 1 < 1;;").get.value shouldBe
              TopExpr(BinOp(Or, BinOp(Lt, ILit(1), ILit(1)), BinOp(Lt, ILit(1), ILit(1))))
          }

          it ("should return BinOp(And ...") {
            Parser.statement.parse("1 && false;;").get.value shouldBe TopExpr(BinOp(And, ILit(1), BLit(false)))
            Parser.statement.parse("true && (false);;").get.value shouldBe TopExpr(BinOp(And, BLit(true), BLit(false)))
            Parser.statement.parse("true&&false;;").get.value shouldBe TopExpr(BinOp(And, BLit(true), BLit(false)))
            Parser.statement.parse("true || false && true || false;;").get.value shouldBe
              TopExpr(BinOp(And, BinOp(Or, BLit(true), BLit(false)), BinOp(Or, BLit(true), BLit(false))))
          }
        }
      }

      describe("dfunExpr") {
        it ("should parse dynamic function") {
          Parser.statement.parse("dfun x -> x + 1;;").get.value shouldBe TopExpr(DFunExp("x", BinOp(Plus, Var("x"), ILit(1))))
          Parser.statement.parse("dfun x y -> x + y;;").get.value shouldBe TopExpr(DFunExp("x", DFunExp("y", BinOp(Plus, Var("x"), Var("y")))))
        }
      }

      describe("funExpr") {
        it("should parse function") {
          Parser.statement.parse("fun x -> x + 1;;").get.value shouldBe TopExpr(FunExp("x", BinOp(Plus, Var("x"), ILit(1))))
          Parser.statement.parse("fun x y -> x + y;;").get.value shouldBe TopExpr(FunExp("x", FunExp("y", BinOp(Plus, Var("x"), Var("y")))))
        }
      }

      describe("letExpr") {
        it("should parse let expr") {
          Parser.statement.parse("let x = 1 in x;;").get.value shouldBe
            TopExpr(LetExp("x", ILit(1), Var("x")))
          Parser.statement.parse("let x = 1 in let y = x in x + y;;").get.value shouldBe
            TopExpr(LetExp("x", ILit(1), LetExp("y", Var("x"), BinOp(Plus, Var("x"), Var("y")))))
          Parser.statement.parse("let plus = fun x -> x + 1 in plus 3;;").get.value shouldBe
            TopExpr(LetExp("plus", FunExp("x", BinOp(Plus, Var("x"), ILit(1))), AppExp(Var("plus"), ILit(3))))
        }
      }

      describe("letRecExpr") {
        it("should parse let rec expr") {
          Parser.statement.parse("let rec fact = fun x -> if x < 1 then 1 else x * fact(x - 1) in fact 6;;").get.value shouldBe
            TopExpr(LetRecExp(
              "fact",
              FunExp("x", IfExp(BinOp(Lt ,Var("x"),ILit(1)), ILit(1), BinOp(Mult, Var("x"), AppExp(Var("fact"), BinOp(Minus, Var("x"), ILit(1)))))),
              AppExp(Var("fact"), ILit(6))
            ))
        }
      }

      describe("ifExpr") {
        it("should parse if expr") {
          Parser.statement.parse("if true then 1 else 1;;").get.value shouldBe
            TopExpr(IfExp(BLit(true), ILit(1), ILit(1)))
          Parser.statement.parse("if x < 0 then x else (0 - x);;").get.value shouldBe
            TopExpr(IfExp(BinOp(Lt, Var("x"), ILit(0)), Var("x"), BinOp(Minus, ILit(0), Var("x"))))
        }
      }
    }

    describe("topLet") {
      it("should parse top of let") {
        Parser.statement.parse("let x = 1;;").get.value shouldBe
          MultiDecl(List(Decl("x", ILit(1))))
        Parser.statement.parse("let addx = fun x -> x + 1;;").get.value shouldBe
          MultiDecl(List(
            Decl("addx", FunExp("x", BinOp(Plus, Var("x"), ILit(1))))
          ))
      }
    }

    describe("topDecl") {
      it("should parse top declaration") {
        Parser.statement.parse("let add x = x + 1;;").get.value shouldBe
          MultiDecl(List(Decl("add", FunExp("x", BinOp(Plus, Var("x"), ILit(1))))))
        Parser.statement.parse("let add x y = x + y;;").get.value shouldBe
          MultiDecl(List(
            Decl("add", FunExp("x", FunExp("y", BinOp(Plus, Var("x"), Var("y")))))
          ))
      }
    }

    describe("recDecl") {
      it("should parse recursive declaration") {
        Parser.statement.parse("let rec test = fun x -> x + 1;;").get.value shouldBe
          RecDecl("test", "x", BinOp(Plus, Var("x"), ILit(1)))
        Parser.statement.parse("let rec fact = fun x -> if x < 1 then 1 else x * fact(x - 1);;").get.value shouldBe
          RecDecl(
            "fact",
            "x",
            IfExp(
              BinOp(Lt, Var("x"), ILit(1)),
              ILit(1),
              BinOp(Mult, Var("x"), AppExp(Var("fact"), BinOp(Minus, Var("x"), ILit(1))))
            )
          )
      }
    }

    describe("program") {
      it("can parse program contains multiple statements") {
        Parser.program.parse("let x = 1;; let y = 2;;").get.value shouldBe
          List(MultiDecl(List(Decl("x", ILit(1)))), MultiDecl(List(Decl("y", ILit(2)))))
        Parser.program.parse("let x = 1;; print x;;").get.value shouldBe
          List(MultiDecl(List(Decl("x", ILit(1)))), TopExpr(AppExp(Var("print"), Var("x"))))
      }
    }
  }
}
