package colang.ast.parsed.expression

import colang.Issue
import colang.ast.parsed.{Scope, Type}
import colang.ast.raw.{expression => raw}

/**
  * Represents a literal fractional value of type 'double'.
  * @param value literal value
  * @param scope enclosing scope
  */
case class DoubleLiteral(value: Double)(implicit scope: Scope) extends Expression {
  val type_ = scope.root.resolve("double").get.asInstanceOf[Type]
}

object DoubleLiteral {
  def analyze(rawExpr: raw.DoubleLiteral)(implicit scope: Scope): (Expression, Seq[Issue]) = {
    (DoubleLiteral(rawExpr.value.value), Seq.empty)
  }
}