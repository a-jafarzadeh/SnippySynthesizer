package ast

class Lambda(val vars: List[Variable], val expr: ASTNode) extends ASTNode{
  override val name: String = vars.map(_.name).mkString("(",",",")")
  override val arity: Int = 1
  override val height: Int = -1

  override def toCodeInner(stringBuilder: StringBuilder): Unit = {
    stringBuilder ++= "lambda " ++= vars.map(_.name).mkString(",") ++= ": "
    expr.toCodeInner(stringBuilder)
  }
}
