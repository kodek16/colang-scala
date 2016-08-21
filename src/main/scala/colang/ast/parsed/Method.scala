package colang.ast.parsed

import colang.{Note, SourceCode}

/**
  * Represents a method: a function that can only be called on an instance of some type.
  * Note that a method is not a symbol because it isn't stable.
  * @param name method name
  * @param declarationSite optionally method declaration site
  * @param container containing type
  * @param returnType method return type
  * @param parameters method parameters
  * @param body method body
  * @param native whether method is native
  */
class Method(val name: String,
             val declarationSite: Option[SourceCode],
             val container: Type,
             val returnType: Type,
             val parameters: Seq[Variable],
             val body: CodeBlock,
             val native: Boolean = false) {

  /**
    * Returns true if method can be called with arguments of given types (this is, whether the arguments are
    * implicitly convertible to parameter types).
    * @param argumentTypes argument types
    * @return true if function can be called with these arguments
    */
  def canBeAppliedTo(argumentTypes: Seq[Type]): Boolean = {
    if (parameters.isEmpty && argumentTypes.isEmpty) {
      true
    } else if (parameters.size == argumentTypes.size) {
      parameters map {_.type_} zip argumentTypes map { case (p, a) => a.isImplicitlyConvertibleTo(p) } reduce {_ && _}
    } else false
  }

  /**
    * A convenience method for optionally constructing a note pointing to the declaration site.
    */
  def declarationSiteNotes: Seq[Note] = {
    declarationSite match {
      case Some(site) => Seq(Note(site, "declared here"))
      case None => Seq.empty
    }
  }
}
