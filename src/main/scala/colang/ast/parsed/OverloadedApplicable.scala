package colang.ast.parsed

import colang.SourceCode
import colang.ast.parsed.Applicable.{AmbiguousOverloadsFound, NoOverloadsFound, OverloadFound}
import colang.issues.Issue

import scala.collection.mutable.ListBuffer

/**
  * This trait defines common behavior related to applicable entities overloading.
  * @tparam A applicable entity kind
  */
trait OverloadedApplicable[A <: Applicable] {

  /**
    * Underlying ListBuffer for storing overloads.
    */
  protected def overloads: ListBuffer[A]

  /**
    * Returns true if the overload can be added to this object. This method should return false for entities
    * with different name or scope - totally unrelated.
    * @param overload overload to check
    * @return true if it's related to this overload set
    */
  protected def overloadIsSuitable(overload: A): Boolean

  // Subclasses must generate their issues themselves by implementing those methods:
  protected def duplicateIssue(newOverload: A, originalOverload: A): Issue
  protected def ambiguousCallIssue(callSource: SourceCode, candidates: Seq[A], argumentTypes: Seq[Type]): Issue
  protected def invalidArgumentsIssue(callSource: SourceCode, argumentTypes: Seq[Type]): Issue

  /**
    * Tries to add a new overload to this entity.
    * @param newOverload overload to add
    * @return encountered issues
    */
  def tryAddOverload(newOverload: A): Seq[Issue] = {
    if (!overloadIsSuitable(newOverload)) {
      throw new IllegalArgumentException("can't add unrelated overload")
    }

    val existingOverloadOption = overloads find { Applicable.sameParameterTypes(_, newOverload )}

    existingOverloadOption match {
      case Some(existingOverload) => Seq(duplicateIssue(newOverload, existingOverload))
      case None =>
        overloads += newOverload
        Seq.empty
    }
  }

  /**
    * Finds the unambiguously matching overload that can be applied to arguments with given types. If
    * call source was provided, will generate issues in case of error.
    * @param argumentTypes argument types
    * @param callSource optional call source
    * @return (Some(overload) if successful, encountered issues)
    */
  def resolveOverload(argumentTypes: Seq[Type], callSource: Option[SourceCode]): (Option[A], Seq[Issue]) = {
    Applicable.resolveOverload(overloads, argumentTypes) match {
      case OverloadFound(overload) => (Some(overload), Seq.empty)

      case AmbiguousOverloadsFound(candidates) =>
        val issues = callSource map { cs =>
          ambiguousCallIssue(cs, candidates, argumentTypes)
        }
        (None, issues.toSeq)

      case NoOverloadsFound() =>
        val issues = callSource map { cs =>
          invalidArgumentsIssue(cs, argumentTypes)
        }
        (None, issues.toSeq)
    }
  }

  /**
    * Returns all overloads.
    */
  def allOverloads: Seq[A] = overloads
}
