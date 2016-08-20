package colang.ast.parsed.expression

import colang.Issue
import colang.ast.parsed.{Scope, Type}
import colang.ast.raw.{expression => raw}

/**
  * Represents a literal integer value of type 'int'.
  * @param value literal value
  * @param scope enclosing scope
  */
case class IntLiteral(value: Int)(implicit scope: Scope) extends Expression {
  val type_ = scope.root.resolve("int").get.asInstanceOf[Type]
}

object IntLiteral {
  def analyze(rawExpr: raw.IntLiteral)(implicit scope: Scope): (Expression, Seq[Issue]) = {
    (IntLiteral(rawExpr.value.value), Seq.empty)
  }
}