package colang.ast.parsed

import colang.SourceCode
import colang.ast.parsed.Applicable.{AmbiguousOverloadsFound, NoOverloadsFound, OverloadFound}
import colang.issues.{Adjectives, Issue, Issues, Terms}

import scala.collection.mutable.ListBuffer

class OverloadedFunction(val name: String,
                         val scope: Some[Scope]) extends Symbol {

  val description = Adjectives.Overloaded applyTo Terms.Function
  val definitionSite = None

  private val overloads = ListBuffer.empty[Function]

  /**
    * Tries to add a new overload to this function.
    * @param newOverload overload to add
    * @return encountered issues
    */
  def tryAddOverload(newOverload: Function): Seq[Issue] = {
    if (newOverload.name != name || newOverload.scope != scope) {
      throw new IllegalArgumentException("can't add unrelated overload to a function")
    }

    val existingOverloadOption = overloads find { Applicable.sameParameterTypes(_, newOverload )}

    existingOverloadOption match {
      case Some(existingOverload) =>
        val issue = Issues.DuplicateFunctionDefinition(newOverload.definition.get.prototypeSource,
          existingOverload.definitionSite)
        Seq(issue)
      case None =>
        overloads += newOverload
        Seq.empty
    }
  }

  /**
    * Finds the unambiguously matching function overload that can be applied to arguments with given types. If
    * call source was provided, will generate issues in case of error.
    * @param argumentTypes argument types
    * @param callSource optional call source
    * @return (Some(overload) if successful, encountered issues)
    */
  def resolveOverload(argumentTypes: Seq[Type], callSource: Option[SourceCode]): (Option[Function], Seq[Issue]) = {
    Applicable.resolveOverload(overloads, argumentTypes) match {
      case OverloadFound(overload) => (Some(overload), Seq.empty)

      case AmbiguousOverloadsFound(candidates) =>
        val notes = candidates map { candidate =>
          Issues.AmbiguousOverloadedCall.note(candidate.signatureString, candidate.definitionSite)
        }

        val issues = callSource match {
          case Some(cs) => Seq(Issues.AmbiguousOverloadedCall(cs, (Terms.Function, notes)))
          case None => Seq.empty
        }

        (None, issues)

      case NoOverloadsFound() =>
        val issues = callSource match {
          case Some(cs) =>
            val argTypeNames = argumentTypes map { _.qualifiedName }
            Seq(Issues.InvalidCallArguments(cs, (Terms.Function, argTypeNames)))
          case None => Seq.empty
        }

        (None, issues)
    }
  }

  /**
    * Returns all overloads.
    */
  def allOverloads: Seq[Function] = overloads
}
