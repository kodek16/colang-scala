package colang.ast.parsed.expression

import colang.ast.parsed.{LocalContext, Scope, Type}
import colang.ast.raw.{expression => raw}
import colang.issues.{Issue, Issues}

/**
  * Represents a 'this' expression.
  * @param type_ actual type of 'this'
  */
case class ThisReference(type_ : Type, rawNode: Option[raw.ThisReference]) extends Expression

object ThisReference {
  def analyze(rawExpr: raw.ThisReference)(implicit scope: Scope, localContext: LocalContext): (Expression, Seq[Issue]) = {
    localContext.contextualObjectType match {
      case Some(thisType) => (ThisReference(thisType, Some(rawExpr)), Seq.empty)
      case None =>
        val issue = Issues.ThisReferenceOutsideMethod(rawExpr.source, ())
        (InvalidExpression(), Seq(issue))
    }
  }
}
