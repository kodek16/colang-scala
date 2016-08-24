package colang.ast.parsed

import colang.ast.raw

/**
  * Represents a function: an independent callable code unit.
  * @param name function name
  * @param scope enclosing scope
  * @param returnType function return type
  * @param parameters function parameters
  * @param body function body
  * @param definition raw function definition
  * @param native whether function is native
  */
class Function(val name: String,
               val scope: Some[Scope],
               val returnType: Type,
               val parameters: Seq[Variable],
               val body: CodeBlock,
               val definition: Option[raw.FunctionDefinition],
               val native: Boolean = false) extends Symbol with Applicable {

  val declarationSite = definition match {
    case Some(fd) => Some(fd.prototypeSource)
    case None => None
  }
  val description = "function"

  /**
    * Returns a corresponding function type. Isn't very useful yet.
    */
  lazy val functionType : Type = {
    val paramString = (parameters map { _.type_.qualifiedName }).mkString(",")
    val retTypeString = returnType.qualifiedName
    val typeName = s"($paramString -> $retTypeString)"
    new Type(
      name = typeName,
      scope = Some(scope.get.root),
      definition = None)
  }
}
