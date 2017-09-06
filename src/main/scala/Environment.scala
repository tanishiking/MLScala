package mlscala

import Ast.Var
import EvalResult.{EvalV, PrintV}

object Environment {
  type Env = Map[Var, EvalV]

  val initialEnv: Env = Map(Var("print") -> PrintV())

  def empty: Env = Map.empty[Var, EvalV]
}
