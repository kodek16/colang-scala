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
                       rawNode: Option[raw.MemberAccess]) extends Expression {

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
                        rawNode: Option[raw.MemberAccess]) extends Expression {

  val type_ = method.container.scope.get.root.boundMethodType
}


/**
  * Represents a reference to an overloaded object method.
  * @param instance object the method is bound to
  * @param overloadedMethod overloaded method
  */
case class OverloadedMethodAccess(instance: Expression,
                                  overloadedMethod: OverloadedMethod,
                                  rawNode: Option[raw.MemberAccess]) extends Expression {

  val type_ = overloadedMethod.container.scope.get.root.boundMethodType
}

object MemberAccess {
  def analyze(rawExpr: raw.MemberAccess)(implicit scope: Scope, localContext: LocalContext): (Expression, Seq[Issue]) = {
    val (instance, instanceIssues) = Expression.analyze(rawExpr.instance)

    def asFieldAccess: Option[(Expression, Seq[Issue])] = {
      val nonRefType = instance.type_ match {
        case t: NonReferenceType => t
        case rt: ReferenceType => rt.referenced
      }

      nonRefType.resolveObjectMember(rawExpr.memberName.value) match {
        case Some(f: Field) => Some((FieldAccess(instance, f, Some(rawExpr)), instanceIssues))
        case _ => None
      }
    }

    def asMethodAccess: Option[(Expression, Seq[Issue])] = {
      instance.type_.resolveObjectMember(rawExpr.memberName.value) match {
        case Some(m: Method) => Some((MethodAccess(instance, m, Some(rawExpr)), instanceIssues))
        case Some(om: OverloadedMethod) => Some((OverloadedMethodAccess(instance, om, Some(rawExpr)), instanceIssues))

        case None =>
          instance.type_ match {
            // Try dereferencing
            case rt: ReferenceType =>
              val dereferenced = rt.referenced
              dereferenced.resolveObjectMember(rawExpr.memberName.value) match {
                case Some(m: Method) =>
                  Some((MethodAccess(ImplicitDereferencing(instance), m, Some(rawExpr)), instanceIssues))
                case Some(om: OverloadedMethod) =>
                  Some((OverloadedMethodAccess(ImplicitDereferencing(instance), om, Some(rawExpr)), instanceIssues))

                case _ => None
              }

            // Check if the method exists in the reference type.
            case t: NonReferenceType =>
              val reference = t.reference
              reference.resolveObjectMember(rawExpr.memberName.value) match {
                case Some(m: Method) =>
                  val issue = Issues.ReferenceMethodAccessFromNonReference(rawExpr.source, (m.name, t.qualifiedName))
                  Some((InvalidExpression()), instanceIssues :+ issue)
                case Some(om: OverloadedMethod) =>
                  val issue = Issues.ReferenceMethodAccessFromNonReference(rawExpr.source, (om.name, t.qualifiedName))
                  Some((InvalidExpression()), instanceIssues :+ issue)

                case _ => None
              }
          }

        case _ => None
      }
    }

    def asUnknownMemberAccess: (Expression, Seq[Issue]) = {
      val issue = Issues.UnknownObjectMember(rawExpr.memberName.source, instance.type_.qualifiedName)
      (InvalidExpression(), instanceIssues :+ issue)
    }

    asFieldAccess.orElse(asMethodAccess).getOrElse(asUnknownMemberAccess)
  }
}
