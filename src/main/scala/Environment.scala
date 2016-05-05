package mlscala

import Ast._
import mlscala.EvalResult.EvalV

object Environment {
  type Env = Map[Var, EvalV]

  def getEmptyEnv: Env = Map.empty[Var, EvalV]
  def extendEnv(x: Var, v: EvalV, env: Map[Var, EvalV]): Env = env.updated(x, v)

  def lookup(x: Var, env: Env): Option[EvalV] = env.get(x)
}
