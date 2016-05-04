package mlscala

import mlscala.Ast.Expr
import mlscala.Environment._

object EvalResult {

  abstract sealed class EvalResult
  case class SingleEvalResult(name: String, newEnv: Env, expr: Expr) extends EvalResult
  case class MultiEvalResult(names: List[String], newEnv: Env, exprs: List[Expr]) extends EvalResult

}
