package colang.ast.parsed

import colang.SourceCode
import colang.ast.parsed.Applicable.{AmbiguousOverloadsFound, NoOverloadsFound, OverloadFound}
import colang.issues.{Issue, Issues, Terms}

import scala.collection.mutable

/**
  * A trait that describes the aspects of type that concern constructors.
  */
trait ConstructorContainer { this: Type =>
  private val constructors = mutable.ListBuffer.empty[Constructor]

  /**
    * Chooses a suitable constructor for given argument types or generates issues if it isn't possible.
    * @param argumentTypes constructor argument types
    * @param callSource optionally constructor call site
    * @return
    */
  def resolveConstructor(argumentTypes: Seq[Type], callSource: Option[SourceCode]): (Option[Constructor], Seq[Issue]) = {
    Applicable.resolveOverload(constructors, argumentTypes) match {
      case OverloadFound(overload) => (Some(overload), Seq.empty)

      case AmbiguousOverloadsFound(candidates) =>
        val notes = candidates map { candidate =>
          Issues.AmbiguousOverloadedCall.note(candidate.signatureString, candidate.definitionSite)
        }

        val issues = callSource match {
          case Some(cs) => Seq(Issues.AmbiguousOverloadedCall(cs, (Terms.Constructor, notes)))
          case None => Seq.empty
        }

        (None, issues)

      case NoOverloadsFound() =>
        val issues = callSource match {
          case Some(cs) =>
            val argTypeNames = argumentTypes map { _.qualifiedName }
            Seq(Issues.InvalidCallArguments(cs, (Terms.Constructor, argTypeNames)))
          case None => Seq.empty
        }

        (None, issues)
    }
  }

  /**
    * Adds a constructor unconditionally, throws if it can't be done.
    * @param newConstructor detached constructor
    */
  def addConstructor(newConstructor: Constructor): Unit = {
    val issues = tryAddConstructor(newConstructor)
    if (issues.nonEmpty) throw new IllegalArgumentException("couldn't add the constructor")
  }

  /**
    * Tries to register a constructor in the type.
    * @param newConstructor detached constructor
    * @return an issue if registration failed
    */
  def tryAddConstructor(newConstructor: Constructor): Seq[Issue] = {
    if (newConstructor.type_ != this) {
      throw new IllegalArgumentException("can't add unrelated constructor to a type")
    }

    val existingConstructorOption = constructors find { Applicable.sameParameterTypes(_, newConstructor) }

    existingConstructorOption match {
      case Some(existingConstructor) if existingConstructor.definition.isEmpty =>
        constructors -= existingConstructor
        constructors += newConstructor
        Seq.empty

      case Some(existingConstructor) =>
        val issue = Issues.DuplicateConstructorDefinition(newConstructor.definitionSite.get,
          existingConstructor.definitionSite)
        Seq(issue)

      case None =>
        constructors += newConstructor
        Seq.empty
    }
  }

  /**
    * Returns a Seq containing all constructors of this type.
    * @return all constructors in a Seq
    */
  def allConstructors: Seq[Constructor] = constructors
}
