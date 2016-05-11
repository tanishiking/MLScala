package mlscala

import mlscala.EvalResult.{MultiEvalResult, SingleEvalResult, getPrettyVal}
import mlscala.Ast.Program
import mlscala.Parser.{parse, parseProgramFile}
import mlscala.Eval.evalDecl
import mlscala.Environment._

import scala.io.StdIn.readLine
import scala.io.Source.fromFile
import java.io.File

object Main {

  private val EXITCOMMAND = "exit"
  private val EXITMESSAGE = "bye"

  def readEvalPrint(env: Env): Unit = {
    def returnToREPL(msg: String, env: Env): Unit = {
      println(msg)
      readEvalPrint(env)
    }
    val input: String = readLine("# ")
    if (input == EXITCOMMAND) {
      println(EXITMESSAGE)
      sys.exit(0)
    }

    parse(input) match {
      case Parser.NoSuccess(msg, _) => returnToREPL(msg, env)
      case Parser.Success(decl, _) =>evalDecl(env, decl) match {
          case Left(e: Exception) => returnToREPL(e.getMessage, env)
          case Right(evalResult) => {
            evalResult match {
              case SingleEvalResult(id, newEnv, v) =>
                printf("val %s = %s\n", id, getPrettyVal(v))
                readEvalPrint(newEnv)
              case MultiEvalResult(ids, newEnv, vs) =>
                ids.zip(vs).map(t => String.format("val %s = %s", t._1, getPrettyVal(t._2))).foreach(println)
                readEvalPrint(newEnv)
            }
          }
        }
    }
  }

  private def interpret(file: File) = {
    val input: String = fromFile(file).mkString
    parseProgramFile(input) match {
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
    if (args.isEmpty) readEvalPrint(initialEnv)
    else findFile(args.head) match {
      case Some(file) => interpret(file)
      case None       => sys.error("File not found: " + args.head)
    }
  }

}
