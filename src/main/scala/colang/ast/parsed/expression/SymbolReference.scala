package colang.ast.parsed.expression

import colang.ast.parsed.{Function, LocalContext, NonReferenceType, OverloadedFunction, ReferenceType, ReferenceVariable, Scope, Symbol, Type, Variable}
import colang.ast.raw.{expression => raw}
import colang.issues.{Issue, Issues}

import scala.annotation.tailrec

/**
  * Represents a type name, which is technically an expression.
  * @param value referenced type
  */
case class TypeReference(value: Type, rawNode: Option[raw.Expression]) extends Expression {
  val type_ = value.scope.get.root.typeType
}

/**
  * Represents a function reference.
  * @param function referenced function
  */
case class FunctionReference(function: Function, rawNode: Option[raw.SymbolReference]) extends Expression {
  val type_ = function.functionType
}

/**
  * Represents an overloaded function reference.
  * @param overloadedFunction referenced overloaded function
  */
case class OverloadedFunctionReference(overloadedFunction: OverloadedFunction,
                                       rawNode: Option[raw.SymbolReference]) extends Expression {

  val type_ = overloadedFunction.scope.get.root.overloadedFunctionType
}

object VariableReference {
  def apply(variable: Variable, rawNode: Option[raw.SymbolReference]) = new VariableReference(variable, rawNode)
  def unapply(arg: VariableReference): Option[Variable] = Some(arg.variable)
}

object ReferenceVariableReference {

  /**
    * A special extractor for "reference variable" references
    */
  def unapply(arg: VariableReference): Option[(Variable, ReferenceType)] = arg match {
    case VariableReference(rv @ ReferenceVariable(_, rt)) => Some(rv, rt)
    case _ => None
  }
}

/**
  * Represents a variable reference.
  * Note that there is an additional ReferenceVariableReference extractor for cases when you care whether the referenced
  * variable has reference type.
  * @param variable referenced variable
  */
class VariableReference private (val variable: Variable,
                                 val rawNode: Option[raw.SymbolReference]) extends Expression {
  val type_ = variable.type_ match {
    case t: NonReferenceType => t.reference
    case rt: ReferenceType => rt
  }
}

object SymbolReference {

  // Note that this function supports "short member access" expressions: object member names without "this." prefix
  // inside methods, and handles them correctly.
  def analyze(rawExpr: raw.SymbolReference)(implicit scope: Scope, localContext: LocalContext): (Expression, Seq[Issue]) = {

    // Returns true if the symbol is more "local" than an object member. Should only return true when the symbol
    // is local.
    def symbolIsMoreSpecificThanObjectMember(symbol: Symbol): Boolean = {
      localContext.contextualObjectType match {
        case Some(contextualObjectType) =>
          val objectTypeScope = contextualObjectType match {
            case t: NonReferenceType => t
            case rt: ReferenceType => rt.referenced
          }

          @tailrec
          def checkScope(scope: Scope): Boolean = {
            if (scope == objectTypeScope) {
              true
            } else {
              scope.parent match {
                case Some(parent) => checkScope(parent)
                case None => false
              }
            }
          }

          checkScope(symbol.scope.get)  // scope is always defined: root namespace can't appear here.

        case None => true
      }
    }

    def resolveSymbol(symbol: Symbol): (Expression, Seq[Issue]) = {
      symbol match {
        case t: Type => (TypeReference(t, Some(rawExpr)), Seq.empty)
        case v: Variable => (VariableReference(v, Some(rawExpr)), Seq.empty)
        case f: Function => (FunctionReference(f, Some(rawExpr)), Seq.empty)
        case of: OverloadedFunction => (OverloadedFunctionReference(of, Some(rawExpr)), Seq.empty)
        case _ =>
          val issue = Issues.InvalidReferenceAsExpression(rawExpr.source, symbol.description)
          (InvalidExpression(), Seq(issue))
      }
    }

    val asShortMemberAccess = localContext.contextualObjectType match {
      case Some(contextualObjectType) =>
        MemberAccess.tryAnalyze(ThisReference(contextualObjectType, None), rawExpr.name.value, rawExpr)
      case None => None
    }

    (asShortMemberAccess, scope.resolve(rawExpr.name.value)) match {
      case (Some(resultAsShortMemberAccess), Some(s: Symbol)) if symbolIsMoreSpecificThanObjectMember(s) =>
        resolveSymbol(s)
      case (Some(resultAsShortMemberAccess), _) =>
        resultAsShortMemberAccess
      case (None, Some(s: Symbol)) =>
        resolveSymbol(s)
      case (None, None) =>
        val issue = Issues.UnknownName(rawExpr.source, ())
        (InvalidExpression(), Seq(issue))
    }
  }
}
