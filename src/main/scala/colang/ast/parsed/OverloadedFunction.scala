package colang.ast.parsed

import colang.SourceCode
import colang.issues.{Issue, Issues, Terms}

import scala.collection.mutable.ListBuffer

class OverloadedFunction(val name: String,
                         val scope: Some[Scope]) extends Symbol {

  val description = Terms.OverloadedFunction
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

    val existingOverloadOption = overloads find { Function.sameParameterTypes(_, newOverload )}

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
    val candidates = overloads filter { _ canBeAppliedTo argumentTypes }
    if (candidates.nonEmpty) {
      val nonAmbiguousResult = candidates find { candidate =>
        (candidates - candidate) forall { _ canBeAppliedTo (candidate.parameters map { _.type_ }) }
      }

      nonAmbiguousResult match {
        case Some(overload) =>
          (Some(overload), Seq.empty)
        case None =>
          val notes = candidates map { candidate =>
            Issues.AmbiguousOverloadedFunctionCall.note(candidate.signatureString,
              candidate.definition map { _.prototypeSource })
          }

          val issues = callSource match {
            case Some(cs) => Seq(Issues.AmbiguousOverloadedFunctionCall(cs, notes))
            case None => Seq.empty
          }

          (None, issues)
      }
    } else {
      val issues = callSource match {
        case Some(cs) => Seq(Issues.InvalidFunctionArguments(cs, argumentTypes map { _.qualifiedName }))
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
