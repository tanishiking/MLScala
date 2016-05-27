package mlscala

import mlscala.Ast.{TyBool, TyInt, Expr, Type}
import mlscala.Environment._

object EvalResult {

  abstract sealed class EvalResult {
    def getEnv: Env
  }
  case class SingleEvalResult(name: String, newEnv: Env, v: EvalV) extends EvalResult {
    override def getEnv: Env = newEnv
  }
  case class MultiEvalResult(names: List[String], newEnv: Env, vs: List[EvalV]) extends EvalResult {
    override def getEnv: Env = newEnv
  }

  abstract sealed class EvalV
  case class IntV(i: Int) extends EvalV
  case class BoolV(b: Boolean) extends EvalV
  case class ProcV(arg: String, env: Env, body: Expr) extends EvalV
  case class DProcV(arg: String, body: Expr) extends EvalV // 動的束縛
  case class PrintV() extends EvalV

  def getPrettyVal(v: EvalV): String = {
    v match {
      case IntV(i)          => i.toString
      case BoolV(b)         => b.toString
      case ProcV(arg, _, _) => arg
      case DProcV(arg, _)   => arg
      case _                => "-"
    }
  }

  def getPrettyTy(t: Type): String = {
    t match {
      case TyInt  => "int"
      case TyBool => "boolean"
    }
  }


}
