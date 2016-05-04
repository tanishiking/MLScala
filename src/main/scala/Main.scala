package mlscala

import mlscala.Ast.{Exp, getPretyExpr}
import mlscala.EvalResult.{MultiEvalResult, SingleEvalResult}
import mlscala.Parser.parse
import mlscala.Eval.evalDecl
import mlscala.Environment._

import scala.io.StdIn.readLine

object Main {

  private val EXITCOMMAND = "exit"
  private val EXITMESSAGE = "bye"

  def readEvalPrint(env: Env): Unit = {
    def returnToREPL(msg: String, env: Env): Unit = {
      println(msg)
      readEvalPrint(env)
    }
    val input: String = readLine("# ")
    if (input == EXITCOMMAND) {
      println(EXITMESSAGE)
      sys.exit(0)
    }

    parse(input) match {
      case Parser.NoSuccess(msg, _) => returnToREPL(msg, env)
      case Parser.Success(decl, _) =>evalDecl(env, decl) match {
          case Left(e: Exception) => returnToREPL(e.getMessage, env)
          case Right(evalResult) => {
            evalResult match {
              case SingleEvalResult(id, newEnv, e) =>
                printf("val %s = %s\n", id, getPretyExpr(e))
                readEvalPrint(newEnv)
              case MultiEvalResult(ids, newEnv, es) =>
                ids.zip(es).map(t => String.format("val %s = %s", t._1, getPretyExpr(t._2))).foreach(println)
                readEvalPrint(newEnv)
            }
          }
        }
    }
  }


  def main(args: Array[String]) {
    readEvalPrint(getEmptyEnv)
  }

}
