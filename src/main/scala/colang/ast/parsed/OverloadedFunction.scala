package colang.ast.parsed

import colang.{Error, Issue, Note, SourceCode}

import scala.collection.mutable.ListBuffer

class OverloadedFunction(val name: String,
                         val scope: Some[Scope]) extends Symbol {

  val description = "multiple function overloads"
  val declarationSite = None

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
        val issue = Error(newOverload.definition.get.prototypeSource, "there is already a function with the same name " +
          "and parameter types in the current scope.", existingOverload.declarationSiteNotes)
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
            val declaration = candidate.definition match {
              case Some(fd) => Some(fd.prototypeSource)
              case None => None
            }

            Note(declaration, s"it can mean ${candidate.signatureString}")
          }

          val issues = callSource match {
            case Some(cs) => Seq(Error(cs, "overloaded function call is ambiguous", notes))
            case None => Seq.empty
          }

          (None, issues)
      }
    } else {
      val issues = callSource match {
        case Some(cs) =>
          val argTypes = argumentTypes map { _.qualifiedName } mkString ", "
          Seq(Error(cs, s"function can't be applied to arguments with types: $argTypes"))
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
