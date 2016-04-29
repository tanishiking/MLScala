package mlscala

import mlscala.Ast.{Exp, getPretyExpr}
import mlscala.Parser.parse
import mlscala.Eval.evalDecl
import mlscala.Environment._

import scala.io.StdIn.readLine

object Main {

  def readEvalPrint(env: Env): Unit = {
    val input: String = readLine("# ")
    val decl: Exp = parse(input).get
    val (id, newEnv, exp) = evalDecl(env, decl)
    printf("val %s = ", id)
    print(getPretyExpr(exp) + "\n")
    readEvalPrint(newEnv)
  }

  def main(args: Array[String]) {
    readEvalPrint(getEmptyEnv)
  }

}
