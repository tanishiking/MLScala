import org.scalatest.FlatSpec
import mlscala.Parser._
import mlscala.Ast._

class ParseProgramSpec extends FlatSpec {

  behavior of "parseProgram parser"

  it should "return list of multidecl" in {
    assert(parseProgram("let x = 1;; let y = 2;;").get == List(MultiDecl(List(Decl("x", ILit(1)))), MultiDecl(List(Decl("y", ILit(2))))))
  }

  it should "return list of multidecl and exp" in {
    assert(parseProgram("let x = 1;; print x;;").get == List(MultiDecl(List(Decl("x", ILit(1)))), Exp(AppExp(Var("print"), Var("x")))))
  }

}
