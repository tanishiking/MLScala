package mlscala

import mlscala.EvalResult.{MultiEvalResult, SingleEvalResult, getPrettyVal, getPrettyTy}
import mlscala.Ast.{Program, Type}
import mlscala.Parser.{parse, parseProgram}
import mlscala.Eval.evalDecl
import mlscala.Environment._
import mlscala.Typing.{Substs, tyDecl}

import scala.io.StdIn.readLine
import scala.io.Source.fromFile
import java.io.File

object Main {

  private val EXITCOMMAND = "exit"
  private val EXITMESSAGE = "bye"

  def readEvalPrint(env: Env, tyenv: TyEnv): Unit = {
    def returnToREPL(msg: String, env: Env, tyenv: TyEnv): Unit = {
      println(msg)
      readEvalPrint(env, tyenv)
    }
    val input: String = readLine("# ")
    if (input == EXITCOMMAND) {
      println(EXITMESSAGE)
      sys.exit(0)
    }

    parse(input) match {
      case Parser.NoSuccess(msg, _) => returnToREPL(msg, env, tyenv)
      case Parser.Success(decl, _) =>
        val eitherSubTy = tyDecl(tyenv, decl)
        val eitherEvalRes = evalDecl(env, decl)
        (eitherSubTy, eitherEvalRes) match {
          case (Left(e: Exception), _)     => returnToREPL(e.getMessage, env, tyenv)
          case (_, Left(e: Exception))     => returnToREPL(e.getMessage, env, tyenv)
          case (Right((_, ty)), Right(evalRes)) =>
            evalRes match {
              case SingleEvalResult(id, newEnv, v) =>
                printf("val %s: %s = %s\n", id, getPrettyTy(ty), getPrettyVal(v))
                readEvalPrint(newEnv, tyenv)
              case MultiEvalResult(ids, newEnv, vs) =>
                ids.zip(vs).map(t => String.format("val %s: %s = %s", t._1, getPrettyTy(ty), getPrettyVal(t._2))).foreach(println)
                readEvalPrint(newEnv, tyenv)
            }
        }
    }
  }

  private def interpret(file: File) = {
    val input: String = fromFile(file).mkString
    parseProgram(input) match {
      case Parser.NoSuccess(msg, _)    => sys.error(msg)
      case Parser.Success(programs, _) => programs.foldLeft(initialEnv){
        (accEnv: Env, program: Program) => evalDecl(accEnv, program) match {
          case Left(e: Exception) => sys.error(e.getMessage)
          case Right(evalResult)  => evalResult.getEnv
        }
      }
    }
  }

  private def findFile (filename:String): Option[File] = {
    val file = new File(filename)
    if (file.exists) Some(file)
    else None
  }


  def main(args: Array[String]) {
    if (args.isEmpty) readEvalPrint(initialEnv, getEmptyTyEnv)
    else findFile(args.head) match {
      case Some(file) => interpret(file)
      case None       => sys.error("File not found: " + args.head)
    }
  }

}
