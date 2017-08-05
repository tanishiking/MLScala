package mlscala

import mlscala.EvalResult.{MultiEvalResult, SingleEvalResult, getPrettyVal, getPrettyTy}
import mlscala.Parser.parse
import mlscala.Eval.evalDecl
import mlscala.Environment._
import mlscala.Typing.tyDecl

object Main {

  private val EXITCOMMAND = "exit"
  private val EXITMESSAGE = "bye"

  def readEvalPrint(env: Env, tyenv: TyEnv): Unit = {
    def returnToREPL(msg: String, env: Env, tyenv: TyEnv): Unit = {
      println(msg)
      readEvalPrint(env, tyenv)
    }
    val input: String = readLine("# ")
    if (input == EXITCOMMAND) {
      println(EXITMESSAGE)
      sys.exit(0)
    }

    parse(input) match {
      case Parser.NoSuccess(msg, _) => returnToREPL(msg, env, tyenv)
      case Parser.Success(decl, _) =>
        val eitherTyEnvTy = tyDecl(tyenv, decl)
        val eitherEvalRes = evalDecl(env, decl)
        (eitherTyEnvTy, eitherEvalRes) match {
          case (Left(e: Exception), _)     => returnToREPL(e.getMessage, env, tyenv)
          case (_, Left(e: Exception))     => returnToREPL(e.getMessage, env, tyenv)
          case (Right((newtyenv, ty)), Right(evalRes)) =>
            evalRes match {
              case SingleEvalResult(id, newEnv, v) =>
                printf("val %s: %s = %s\n", id, getPrettyTy(ty), getPrettyVal(v))
                readEvalPrint(newEnv, newtyenv)
              case MultiEvalResult(ids, newEnv, vs) =>
                ids.zip(vs).map(t => String.format("val %s: %s = %s", t._1, getPrettyTy(ty), getPrettyVal(t._2))).foreach(println)
                readEvalPrint(newEnv, newtyenv)
            }
        }
    }
  }

  def main(args: Array[String]) {
    readEvalPrint(initialEnv, getEmptyTyEnv)
  }
}
