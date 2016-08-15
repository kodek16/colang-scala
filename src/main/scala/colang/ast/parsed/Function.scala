package colang.ast.parsed

import colang.SourceCode

/**
  * Represents a function: an independent callable code unit.
  * @param name function name
  * @param declarationSite optionally function declaration site
  * @param scope enclosing scope
  * @param returnType function return type
  * @param parameters function parameters
  * @param body function body
  * @param native whether function is native
  */
class Function(val name: String,
               val declarationSite: Option[SourceCode],
               val scope: Some[Scope],
               val returnType: Type,
               val parameters: Seq[Variable],
               val body: CodeBlock,
               val native: Boolean = false) extends Symbol {

  val description = "a function"

  /**
    * Returns a corresponding function type. Isn't very useful yet.
    */
  lazy val functionType : Type = {
    val paramString = (parameters map { _.type_.qualifiedName }).mkString(",")
    val retTypeString = returnType.qualifiedName
    val typeName = s"($paramString -> $retTypeString)"
    new Type(typeName, None, Some(scope.get.root))
  }

  /**
    * Returns true if function can be called with arguments of given types (this is, whether the arguments are
    * implicitly convertible to parameter types).
    * @param argumentTypes argument types
    * @return true if function can be called with these arguments
    */
  def canBeAppliedTo(argumentTypes: Seq[Type]): Boolean = {
    if (parameters.size == argumentTypes.size) {
      parameters map {_.type_} zip argumentTypes map { case (p, a) => a.isImplicitlyConvertibleTo(p) } reduce {_ && _}
    } else false
  }
}
