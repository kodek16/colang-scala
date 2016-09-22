package colang.ast.parsed.expression

import colang.ast.parsed._
import colang.ast.raw.{expression => raw}
import colang.issues.{Issue, Issues}

/**
  * Represents a reference to an object field.
  * Note that for a field of type 'T', instance may be either of type 'T' or of type 'T&'. This means that
  * 'instance.type_' is not always equal to 'field.container'.
  * @param instance object the field is bound to
  * @param field object field
  */
case class FieldAccess(instance: Expression,
                       field: Field,
                       rawNode: Option[raw.Expression]) extends Expression {

  val type_ = (instance.type_, field.type_) match {
    case (it: NonReferenceType, ft: NonReferenceType) => ft
    case (it: NonReferenceType, ft: ReferenceType) => ft.referenced
    case (it: ReferenceType, ft: NonReferenceType) => ft.reference
    case (it: ReferenceType, ft: ReferenceType) => ft
  }
}

/**
  * Represents a reference to an object method.
  * @param instance object the method is bound to
  * @param method object method
  */
case class MethodAccess(instance: Expression,
                        method: Method,
                        rawNode: Option[raw.Expression]) extends Expression {

  val type_ = method.container.scope.get.root.boundMethodType
}


/**
  * Represents a reference to an overloaded object method.
  * @param instance object the method is bound to
  * @param overloadedMethod overloaded method
  */
case class OverloadedMethodAccess(instance: Expression,
                                  overloadedMethod: OverloadedMethod,
                                  rawNode: Option[raw.Expression]) extends Expression {

  val type_ = overloadedMethod.container.scope.get.root.boundMethodType
}

object MemberAccess {

  /**
    * A "safer" function for accessing object members: only returns Some(expr, issues) if the member actually existed.
    * This is very useful for analyzing short member access expressions in SymbolReference.
    * @param instance object that may contain the member
    * @param memberName member name
    * @return Some(expression, issues) if the member actually exists
    */
  def tryAnalyze(instance: Expression, memberName: String, rawNode: raw.Expression)(implicit scope: Scope): Option[(Expression, Seq[Issue])] = {

    def asFieldAccess: Option[(Expression, Seq[Issue])] = {
      val nonRefType = instance.type_ match {
        case t: NonReferenceType => t
        case rt: ReferenceType => rt.referenced
      }

      nonRefType.resolveObjectMember(memberName) match {
        case Some(f: Field) => Some((FieldAccess(instance, f, Some(rawNode)), Seq.empty))
        case _ => None
      }
    }

    def asMethodAccess: Option[(Expression, Seq[Issue])] = {
      instance.type_.resolveObjectMember(memberName) match {
        case Some(m: Method) => Some((MethodAccess(instance, m, Some(rawNode)), Seq.empty))
        case Some(om: OverloadedMethod) => Some((OverloadedMethodAccess(instance, om, Some(rawNode)), Seq.empty))

        case None =>
          instance.type_ match {
            // Try dereferencing
            case rt: ReferenceType =>
              val dereferenced = rt.referenced
              dereferenced.resolveObjectMember(memberName) match {
                case Some(m: Method) =>
                  Some((MethodAccess(ImplicitDereferencing(instance), m, Some(rawNode)), Seq.empty))
                case Some(om: OverloadedMethod) =>
                  Some((OverloadedMethodAccess(ImplicitDereferencing(instance), om, Some(rawNode)), Seq.empty))

                case _ => None
              }

            // Check if the method exists in the reference type.
            case t: NonReferenceType =>
              val reference = t.reference
              reference.resolveObjectMember(memberName) match {
                case Some(m: Method) =>
                  val issue = Issues.ReferenceMethodAccessFromNonReference(rawNode.source, (m.name, t.qualifiedName))
                  Some((InvalidExpression(), Seq(issue)))
                case Some(om: OverloadedMethod) =>
                  val issue = Issues.ReferenceMethodAccessFromNonReference(rawNode.source, (om.name, t.qualifiedName))
                  Some((InvalidExpression(), Seq(issue)))

                case _ => None
              }
          }

        case _ => None
      }
    }

    asFieldAccess.orElse(asMethodAccess)
  }

  def analyze(rawExpr: raw.MemberAccess)(implicit scope: Scope, localContext: LocalContext): (Expression, Seq[Issue]) = {
    val (instance, instanceIssues) = Expression.analyze(rawExpr.instance)

    tryAnalyze(instance, rawExpr.memberName.value, rawExpr) match {
      case Some((result, issues)) => (result, instanceIssues ++ issues)
      case None =>
        val issue = Issues.UnknownObjectMember(rawExpr.memberName.source, instance.type_.qualifiedName)
        (InvalidExpression(), instanceIssues :+ issue)
    }
  }
}
