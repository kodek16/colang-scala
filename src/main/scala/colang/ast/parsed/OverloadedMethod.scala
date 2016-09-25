package colang.ast.parsed

import colang.SourceCode
import colang.issues.{Adjectives, Issue, Issues, Terms}

import scala.collection.mutable.ListBuffer

/**
  * Represents an overloaded method: a set of methods ("overloads") with the same name for the same type
  * that have different parameter types.
  * @param name function name
  * @param container containing type
  */
class OverloadedMethod(val name: String,
                       val container: Type) extends ObjectMember with OverloadedApplicable[Method] {

  val description = Adjectives.Overloaded applyTo Terms.Method
  val definitionSite = None

  protected val overloads = ListBuffer.empty[Method]

  def overloadIsSuitable(overload: Method): Boolean = {
    overload.name == name && overload.container == container
  }

  protected def duplicateIssue(newOverload: Method, originalOverload: Method): Issue = {
    Issues.DuplicateMethodDefinition(newOverload.definition.get.prototypeSource, originalOverload.definitionSite)
  }

  protected def ambiguousCallIssue(callSource: SourceCode, candidates: Seq[Method], argumentTypes: Seq[Type]): Issue = {
    val notes = candidates map { candidate =>
      Issues.AmbiguousOverloadedCall.note(candidate.signatureString, candidate.definitionSite)
    }

    Issues.AmbiguousOverloadedCall(callSource, (Terms.Method, notes))
  }

  protected def invalidArgumentsIssue(callSource: SourceCode, argumentTypes: Seq[Type]): Issue = {
    val argTypeNames = argumentTypes map { _.qualifiedName }
    Issues.InvalidCallArguments(callSource, (Terms.Method, argTypeNames))
  }
}
