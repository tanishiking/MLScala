package mlscala

import fastparse.core.Parsed
import Environment.Env
import EvalResult.{MultiEvalResult, SingleEvalResult}

object Repl {
  private val ExitCommand = "exit"
  private val ExitMessage = "bye"

  private def loop(env: Env, tyenv: Type.TyEnv): Unit = {
    def returnToREPL(msg: String, env: Env, tyenv: Type.TyEnv): Unit = {
      println(msg)
      loop(env, tyenv)
    }
    val input: String = scala.io.StdIn.readLine("# ")
    if (input == ExitCommand) {
      println(ExitMessage)
      sys.exit(0)
    }

    Parser.statement.parse(input) match {
      case Parsed.Failure(expected, index, extra) => returnToREPL(expected.toString + index.toString + extra.toString, env, tyenv)
      case Parsed.Success(stmt, _) =>
        val eitherTyEnvTy = Typer.typeStmt(tyenv, stmt)
        val eitherEvalRes = Eval.evalStmt(env, stmt)
        (eitherTyEnvTy, eitherEvalRes) match {
          case (Left(e: Exception), _)     => returnToREPL(e.getMessage, env, tyenv)
          case (_, Left(e: Exception))     => returnToREPL(e.getMessage, env, tyenv)
          case (Right((newtyenv, ty)), Right(evalRes)) =>
            evalRes match {
              case SingleEvalResult(id, newEnv, v) =>
                printf("val %s: %s = %s\n", id, ty, v)
                loop(newEnv, newtyenv)
              case MultiEvalResult(ids, newEnv, vs) =>
                ids.zip(vs).map(t => String.format("val %s: %s = %s", t._1, ty, t._2)).foreach(println)
                loop(newEnv, newtyenv)
            }
        }
    }
  }

  def main(args: Array[String]): Unit = {
    loop(Environment.initialEnv, Type.TyEnv.initialEnv)
  }
}
