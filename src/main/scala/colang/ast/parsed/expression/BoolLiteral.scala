package colang.ast.parsed.expression

import colang.Issue
import colang.ast.parsed.{Scope, Type}
import colang.ast.raw.{expression => raw}

/**
  * Represents a literal logical value of type 'bool'.
  * @param value literal value
  * @param scope enclosing scope
  */
case class BoolLiteral(value: Boolean)(implicit scope: Scope) extends Expression {
  val type_ = scope.root.resolve("bool").get.asInstanceOf[Type]
}

object BoolLiteral {
  def analyze(rawExpr: raw.BoolLiteral)(implicit scope: Scope): (Expression, Seq[Issue]) = {
    (BoolLiteral(rawExpr.value.value), Seq.empty)
  }
}