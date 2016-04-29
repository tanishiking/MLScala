package mlscala

import Ast._

object Environment {
  type Env = Map[Var, Expr]

  def getEmptyEnv: Env = Map.empty[Var, Expr]
  def extendEnv(x: Var, v: Expr, env: Map[Var, Expr]): Env = env.updated(x, v)

  def lookup(x: Var, env: Map[Var, Expr]): Option[Expr] = env.get(x)
}
