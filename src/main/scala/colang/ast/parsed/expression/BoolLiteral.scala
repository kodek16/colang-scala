package colang.ast.parsed.expression

import colang.ast.parsed.Scope
import colang.ast.raw.{expression => raw}
import colang.issues.Issue

/**
  * Represents a literal logical value of type 'bool'.
  * @param value literal value
  * @param scope enclosing scope
  */
case class BoolLiteral(value: Boolean,
                       rawNode: Option[raw.BoolLiteral])(implicit scope: Scope) extends Expression {

  val type_ = scope.root.boolType
}

object BoolLiteral {
  def analyze(rawExpr: raw.BoolLiteral)(implicit scope: Scope): (Expression, Seq[Issue]) = {
    (BoolLiteral(rawExpr.value.value, Some(rawExpr)), Seq.empty)
  }
}
