package colang.ast.parsed.expression

import colang.Issue
import colang.ast.parsed.Scope
import colang.ast.raw.{expression => raw}

/**
  * Represents a literal fractional value of type 'double'.
  * @param value literal value
  * @param scope enclosing scope
  */
case class DoubleLiteral(value: Double,
                         rawNode: Option[raw.DoubleLiteral])(implicit scope: Scope) extends Expression {

  val type_ = scope.root.doubleType
}

object DoubleLiteral {
  def analyze(rawExpr: raw.DoubleLiteral)(implicit scope: Scope): (Expression, Seq[Issue]) = {
    (DoubleLiteral(rawExpr.value.value, Some(rawExpr)), Seq.empty)
  }
}