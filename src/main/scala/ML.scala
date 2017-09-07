package mlscala

import scalajs.js.annotation._
import EvalResult.{MultiEvalResult, SingleEvalResult}
import fastparse.core.Parsed
import Environment.Env
import Ast.Stmt
import Type.TyEnv

@JSExportTopLevel("ML")
object ML {
  private val ExitCommand = "exit"
  private val ExitMessage = "bye"

  /*
  def loop(env: Env, tyenv: Type.TyEnv): Unit = {
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
  */

  @JSExport
  def interpret(input: String): String = {
    var buffer = ""
    val res: Either[Exception, (TyEnv, Env)] = Parser.program.parse(input) match {
      case Parsed.Failure(expected, _, extra) => Left(new RuntimeException(
        s"ParseError: ${Parser.errorMessage(expected, input)}"
      ))
      case Parsed.Success(program, _) =>
        program.foldLeft(Right((TyEnv.initialEnv, Environment.initialEnv)): Either[Exception, (TyEnv, Env)]) {
          (acc, stmt) => acc match {
            case Left(exception) => Left(exception)
            case Right((tyenv, env)) =>
              (Typer.typeStmt(tyenv, stmt), Eval.evalStmt(env, stmt)) match {
                case (Right(typeResult), Right(evalResult)) => Right((typeResult._1, evalResult.getEnv))
                case (Left(exception), _) => Left(exception)
                case (_, Left(exception)) => Left(exception)
          }
        }
      }
    }
    buffer += Eval.getOutput
    res.left.foreach { error => buffer += error.getMessage }
    Eval.flushBuffer
    buffer
  }


  def main(args: Array[String]): Unit = {
    // loop(Environment.initialEnv, Type.TyEnv.initialEnv)
    interpret(args.head)
  }
}
