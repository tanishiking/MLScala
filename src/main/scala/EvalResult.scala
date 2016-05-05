package mlscala

import mlscala.Ast.Expr
import mlscala.Environment._

object EvalResult {

  abstract sealed class EvalResult
  case class SingleEvalResult(name: String, newEnv: Env, v: EvalV) extends EvalResult
  case class MultiEvalResult(names: List[String], newEnv: Env, vs: List[EvalV]) extends EvalResult

  abstract sealed class EvalV
  case class IntV(i: Int) extends EvalV
  case class BoolV(b: Boolean) extends EvalV
  case class ProcV(arg: String, env: Env, body: Expr) extends EvalV

  def getPrettyVal(v: EvalV): String = {
    v match {
      case IntV(i)          => i.toString
      case BoolV(b)         => b.toString
      case ProcV(arg, _, _) => arg
      case _                => "-"
    }
  }


}
