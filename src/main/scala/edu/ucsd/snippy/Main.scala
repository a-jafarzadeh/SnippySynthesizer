package edu.ucsd.snippy
import java.io.{BufferedWriter, FileWriter}
import edu.ucsd.snippy.ast.ASTNode
import edu.ucsd.snippy.enumeration.InputsValuesManager
import trace.DebugPrints.dprintln
import scala.concurrent.duration._
import scala.io.Source.fromFile
import scala.util.control.Breaks._


object Main extends App {
  case class RankedProgram(program: ASTNode, rank: Double) extends Ordered[RankedProgram] {
    override def compare(that: RankedProgram): Int = this.rank.compare(that.rank)
  }

	def synthesize(filename: String) : Option[(String, Int)] =
	{
		val task: SynthesisTask = PythonPBETask.fromString(fromFile(filename).mkString)
    synthesizeFromTask(task)
	}

  private def avgSynthesizeTime(filename: String): Unit = {
    val reps = 10;
    var totalTime = 0;
    for (w <- 0 to reps) {
      val task: SynthesisTask = PythonPBETask.fromString(fromFile(filename).mkString)
      val (program, time) = synthesizeFromTask(task) match {
        case None => ("None", -1)
        case Some((program: String, time: Int)) => (program, time)
      }
      totalTime = time + totalTime;
      println(f"[${time / 1000.0}%1.3f] $program")
    }
    println (f"Avg time after $reps : [${totalTime / (reps * 1000.0)}%1.3f]")
  }

  def synthesizeFromTask(task: SynthesisTask, timeout: Int = 7): Option[(String, Int)] = {
    var rs: Option[(String, Int)] = None
    val oeManager = new InputsValuesManager()
    val enumerator = new enumeration.Enumerator(
      task.vocab,
      oeManager,
      task.examples.map(_.input)
    )
    val deadline = timeout.seconds.fromNow
    breakable {
      for ((program, i) <- enumerator.zipWithIndex) {
        if (!deadline.hasTimeLeft || program.height > 3) {
          rs = Some(("None", timeout * 1000 - deadline.timeLeft.toMillis.toInt))
          break
        }
        if (program.nodeType == task.returnType) {
          val results = task.examples.zip(program.values)
            .map(pair => pair._1.output == pair._2)
          if (results.forall(identity)) {
            if (program.usesVariables) {
              rs = Some(
                (task.asInstanceOf[PythonPBETask].outputVar + " = " + PostProcessor.clean(program).code, timeout * 1000 - deadline.timeLeft.toMillis.toInt))
              break
            }
            else {
              oeManager.classValues.remove(program.values)
            }
          }
        }
        if (trace.DebugPrints.debug) {
          val p = PostProcessor.clean(program)
          dprintln(s"[$i] (${program.height}) ${p.code}")
        }
      }
    }
    rs
  }

  case class ExpectedEOFException() extends Exception

//  avgSynthesizeTime(args.head)
  val (program, time) = synthesize(args.head) match {
    case None => ("None", -1)
    case Some((program: String, time: Int)) => (program, time)
  }//  }
  println(f"[${time / 1000.0}%1.3f] $program")
}
//  // trace.DebugPrints.setDebug()
//  val (program, time) = synthesize(args.head) match {//   case None => ("None", -1)
//   case Some((program: String, time: Int)) => (program, time)//  }
////  val writer = new BufferedWriter(new FileWriter(args.head + ".out"))
//  writer.write(program)//  println(f"[${time / 1000.0}%1.3f] $program")
//  writer.close()}