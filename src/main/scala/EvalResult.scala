package mlscala

import Ast.Expr
import Environment.Env

object EvalResult {

  abstract sealed class EvalResult {
    def getEnv: Env
  }
  case class SingleEvalResult(name: String, newEnv: Env, v: EvalV) extends EvalResult {
    override def getEnv: Env = newEnv
  }
  case class MultiEvalResult(names: Seq[String], newEnv: Env, vs: Seq[EvalV]) extends EvalResult {
    override def getEnv: Env = newEnv
  }

  abstract sealed class EvalV
  case class IntV(i: Int) extends EvalV {
    override def toString: String = i.toString
  }
  case class BoolV(b: Boolean) extends EvalV {
    override def toString: String = b.toString
  }
  case class ProcV(arg: String, env: Env, body: Expr) extends EvalV {
    override def toString: String = arg
  }
  case class DProcV(arg: String, body: Expr) extends EvalV {
    override def toString: String = arg
  }
  case class PrintV() extends EvalV {
    override def toString: String = "-"
  }
}
