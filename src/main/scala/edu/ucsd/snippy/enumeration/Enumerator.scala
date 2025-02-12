package edu.ucsd.snippy.enumeration

import edu.ucsd.snippy.ast.ASTNode
import edu.ucsd.snippy.vocab.{VocabFactory, VocabMaker}

import scala.collection.mutable
import trace.DebugPrints.dprintln

class Enumerator(
  val vocab    : VocabFactory,
  val oeManager: OEValuesManager,
  val contexts : List[Map[String, Any]])
  extends Iterator[ASTNode]
{
	override def toString(): String = "enumeration.Enumerator"

	var currIter: Iterator[VocabMaker] = vocab.leaves()
	// TODO Bug: This is not init()-ed
	var prevLevelProgs: mutable.ListBuffer[ASTNode] = mutable.ListBuffer()
	var currLevelProgs: mutable.ListBuffer[ASTNode] = mutable.ListBuffer()
	var nextProgram: Option[ASTNode] = None
	var height = 0

	var rootMaker: Iterator[ASTNode] =
		currIter.next().init(currLevelProgs.toList, contexts, vocab, height)

	override def hasNext: Boolean =
		if (nextProgram.isDefined) {
			true
		} else {
			nextProgram = getNextProgram
			nextProgram.isDefined
		}
private def isIncluded(node: ASTNode): Boolean = {
		var res: Boolean = node.score == 1
		if (!res) {
			for (child <- node.children) {
				res = isIncluded(child) || res
			}
		}
		res
	}

	override def next(): ASTNode = {
		if (nextProgram.isEmpty) nextProgram = getNextProgram
		val res = nextProgram.get
//		while (res.score == 0 && !res.children.exists(_.score > 0) && res.height <= 3){
//			nextProgram = getNextProgram
//			res = nextProgram.get
//			dprintln("==========")
//			dprintln(res.children.toList)
//			dprintln(res.children.exists(_.score > 0))
//		}
		nextProgram = None
		res
	}

	/**
	 * This method moves the rootMaker to the next possible non-leaf. Note that this does not
	 * change the level/height of generated programs.
	 * @return False if we have exhausted all non-leaf AST nodes.
	 */
	def advanceRoot(): Boolean =
	{
		// We may not have any children for a root
		rootMaker = null
		while (rootMaker == null || !rootMaker.hasNext) {
			// We are out of programs!
			if (!currIter.hasNext) return false
			val next = currIter.next()
			rootMaker = next.init(prevLevelProgs.toList, contexts, this.vocab, height)
		}

		true
	}

	/**
	 * This method resets the variables to begin enumerating the next level (taller) trees.
	 * @return False if the current level failed to generate any new programs.
	 */
	def changeLevel(): Boolean =
	{
		// dprintln(currLevelProgs.length)
		if (currLevelProgs.isEmpty) return false

		currIter = vocab.nonLeaves()
		height += 1
		prevLevelProgs ++= currLevelProgs
		currLevelProgs.clear()
		advanceRoot()
	}

	def getNextProgram: Option[ASTNode] =
	{
		var res: Option[ASTNode] = None

		// Iterate while no non-equivalent program is found
		while (res.isEmpty) {
			if (rootMaker.hasNext) {
				val prog = rootMaker.next

				if (prog.values.nonEmpty && oeManager.isRepresentative(prog)) {
					res = Some(prog)
				}
			}
			else if (currIter.hasNext) {
				if (!advanceRoot()) {
					if (!changeLevel()) return None
				}
			}
			else if (!changeLevel()) {
				return None
			}
		}
		currLevelProgs += res.get
		res
	}
}
