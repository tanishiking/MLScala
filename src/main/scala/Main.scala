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
    evalDecl(env, decl) match {
      case Right((id, newEnv, exp)) => {
        printf("val %s = %s\n", id, getPretyExpr(exp))
        readEvalPrint(newEnv)
      }
      case Left(e: Exception) => {
        printf(e.getMessage + "\n")
        readEvalPrint(env)
      }
    }
  }

  def main(args: Array[String]) {
    readEvalPrint(getEmptyEnv)
  }

}
