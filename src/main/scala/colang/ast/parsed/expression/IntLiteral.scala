package colang.ast.parsed.expression

import colang.Issue
import colang.ast.parsed.Scope
import colang.ast.raw.{expression => raw}

/**
  * Represents a literal integer value of type 'int'.
  * @param value literal value
  * @param scope enclosing scope
  */
case class IntLiteral(value: Int,
                      rawNode: Option[raw.IntLiteral])(implicit scope: Scope) extends Expression {

  val type_ = scope.root.intType
}

object IntLiteral {
  def analyze(rawExpr: raw.IntLiteral)(implicit scope: Scope): (Expression, Seq[Issue]) = {
    (IntLiteral(rawExpr.value.value, Some(rawExpr)), Seq.empty)
  }
}