package colang.ast.parsed

/**
  * A trait that generalizes different applicable entities, providing some common behaviour
  */
trait Applicable {

  def parameters: Seq[Variable]

  /**
    * Returns true if the object can be called with arguments of given types (this is, the arguments are
    * implicitly convertible to parameter types).
    * @param argumentTypes argument types
    * @return true if the object can be called with these arguments
    */
  def canBeAppliedTo(argumentTypes: Seq[Type]): Boolean = {
    if (parameters.isEmpty && argumentTypes.isEmpty) {
      true
    } else if (parameters.size == argumentTypes.size) {
      parameters map {_.type_} zip argumentTypes map { case (p, a) => a.isImplicitlyConvertibleTo(p) } reduce {_ && _}
    } else false
  }
}

object Applicable {

  /**
    * Checks if two applicable entities have the same parameter types.
    * @param f first applicable
    * @param g second applicable
    * @return true if applicables have the same parameter types.
    */
  def sameParameterTypes[A <: Applicable](f: A, g: A): Boolean = {
    if (f.parameters.isEmpty && g.parameters.isEmpty) {
      true
    } else if (f.parameters.size == g.parameters.size) {
      (f.parameters map { _.type_ }) zip (g.parameters map { _.type_ }) map { ts => ts._1 == ts._2 } reduce { _ && _ }
    } else false
  }

  /**
    * Tries to select a single unambiguous overload from a Seq that can be applied to arguments with given types.
    * @param overloads overloads to choose from
    * @param argumentTypes argument types
    * @return OverloadResolutionResult, see below
    */
  def resolveOverload[A <: Applicable](overloads: Seq[A], argumentTypes: Seq[Type]): OverloadResolutionResult[A] = {
    val candidates = overloads filter { _ canBeAppliedTo argumentTypes }
    if (candidates.nonEmpty) {
      val nonAmbiguousResult = candidates find { candidate =>
        val otherCandidates = candidates filterNot { _ eq candidate }
        otherCandidates forall { _ canBeAppliedTo (candidate.parameters map { _.type_ }) }
      }

      nonAmbiguousResult match {
        case Some(overload) => OverloadFound(overload)
        case None => AmbiguousOverloadsFound(candidates)
      }
    } else {
      NoOverloadsFound()
    }
  }

  sealed trait OverloadResolutionResult[A <: Applicable]

  case class OverloadFound[A <: Applicable](overload: A) extends OverloadResolutionResult[A]
  case class NoOverloadsFound[A <: Applicable]() extends OverloadResolutionResult[A]
  case class AmbiguousOverloadsFound[A <: Applicable](overloads: Seq[A]) extends OverloadResolutionResult[A]
}
