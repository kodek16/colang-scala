package colang.ast.parsed

/**
  * A trait that generalizes functions and methods, providing some common behaviour
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
