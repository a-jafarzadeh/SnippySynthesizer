package edu.ucsd.snippy.ast

import edu.ucsd.snippy.ast.Types.Types

trait ASTNode
{
	val nodeType: Types.Types
	val values  : List[Any]
	val code    : String
	val height  : Int
	val terms   : Int
	val score   : Int = 0
	val children: Iterable[ASTNode]
	protected val parenless: Boolean
	def includes(varName: String): Boolean
	def parensIfNeeded: String = if (height > 0 && !parenless) "(" + code + ")" else code
	val usesVariables: Boolean
}

trait IterableNode extends ASTNode {

	def splitByIterable[A](values: Iterable[Iterable[_]], listToSplit: Iterable[A]): List[Iterable[A]] =
	{
		var rs: List[Iterable[A]] = Nil
		var start = 0
		for (delta <- values.map(_.size)) {
			rs = rs :+ listToSplit.slice(start, start + delta)
			start += delta
		}
		rs
	}
}

trait StringNode extends IterableNode
{
	override val values: List[String]
	override val nodeType: Types = Types.String
}

trait IntNode extends ASTNode
{
	override val values: List[Int]
	override val nodeType: Types = Types.Int
}

trait BoolNode extends ASTNode
{
	override val values: List[Boolean]
	override val nodeType: Types = Types.Bool
}

trait ListNode[T] extends IterableNode {
	val childType: Types
	override val values: List[Iterable[T]]
	override lazy val nodeType: Types = Types.List(childType)
}

trait StringListNode extends ListNode[String] { override val childType: Types = Types.String }
trait IntListNode extends ListNode[Int] { override val childType: Types = Types.Int}
trait BoolListNode extends ListNode[Boolean]  { override val childType: Types = Types.Bool}

trait MapNode[K,V] extends IterableNode
{
	val keyType: Types
	val valType: Types

	override val values: List[Map[K,V]]
	override lazy val nodeType: Types = Types.Map(keyType, valType)
}

trait StringStringMapNode extends MapNode[String,String]
{
	override val keyType: Types = Types.String
	override val valType: Types = Types.String
}

trait StringIntMapNode extends MapNode[String,Int]
{
	override val keyType: Types = Types.String
	override val valType: Types = Types.Int
}

trait IntStringMapNode extends MapNode[Int,String]
{
	override val keyType: Types = Types.Int
	override val valType: Types = Types.String
}

trait IntIntMapNode extends MapNode[Int,Int]
{
	override val keyType: Types = Types.Int
	override val valType: Types = Types.Int
}