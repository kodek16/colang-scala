package colang.ast.parsed

import colang.SourceCode
import colang.issues.{Adjectives, Issue, Issues, Terms}

import scala.collection.mutable.ListBuffer

/**
  * Represents an overloaded function: a set of functions ("overloads") with the same name in the same scope
  * that have different parameter types.
  * @param name function name
  * @param scope enclosing scope
  */
class OverloadedFunction(val name: String,
                         val scope: Some[Scope]) extends Symbol with OverloadedApplicable[Function] {

  val description = Adjectives.Overloaded applyTo Terms.Function
  val definitionSite = None

  protected val overloads = ListBuffer.empty[Function]

  protected def overloadIsSuitable(overload: Function): Boolean = {
    overload.name == name && overload.scope == scope
  }

  protected def duplicateIssue(newOverload: Function, originalOverload: Function): Issue = {
    Issues.DuplicateFunctionDefinition(newOverload.definition.get.prototypeSource, originalOverload.definitionSite)
  }

  protected def ambiguousCallIssue(callSource: SourceCode, candidates: Seq[Function], argumentTypes: Seq[Type]): Issue = {
    val notes = candidates map { candidate =>
      Issues.AmbiguousOverloadedCall.note(candidate.signatureString, candidate.definitionSite)
    }

    Issues.AmbiguousOverloadedCall(callSource, (Terms.Function, notes))
  }

  protected def invalidArgumentsIssue(callSource: SourceCode, argumentTypes: Seq[Type]): Issue = {
    val argTypeNames = argumentTypes map { _.qualifiedName }
    Issues.InvalidCallArguments(callSource, (Terms.Function, argTypeNames))
  }
}
