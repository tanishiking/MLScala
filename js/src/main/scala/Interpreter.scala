package mlscala

import Type.TyEnv
import Environment.Env

import fastparse.core.Parsed
import scalajs.js.annotation._

@JSExportTopLevel("ML")
object Interpreter {
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
}
