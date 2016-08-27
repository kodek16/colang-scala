package colang.ast.parsed

import colang.SourceCode

class Namespace(val name: String,
                val declarationSite: Option[SourceCode],
                val parent: Option[Namespace]) extends Symbol with Scope {

  val scope = parent
  val description = "namespace"

  //Following methods should be only used on a root namespace instance:
  //TODO actually, implement a RootNamespace subclass and put them there.

  /**
    * A type assigned to values that failed analysis.
    */
  val unknownType: Type = new Type(
    name = "(unknown type)",
    definition = None,
    scope = Some(this))


  /**
    * A type assigned to overloaded function references.
    */
  val overloadedFunctionType: Type = new Type(
    name = "(unresolved function overload)",
    definition = None,
    scope = Some(this))

  lazy val voidType = getPrimitiveType("void")
  lazy val intType = getPrimitiveType("int")
  lazy val doubleType = getPrimitiveType("double")
  lazy val boolType = getPrimitiveType("bool")

  private def getPrimitiveType(name: String): Type = {
    members get name match {
      case Some(t: Type) => t
      case Some(_) =>
        System.err.println(s"Error: '$name' is not a type in the standard library. Please check if your CO installation " +
          s"is correct and up-to-date.")
        sys.exit(2)
      case None =>
        System.err.println(s"Error: '$name' type declaration not found in the standard library. Please check if your " +
          s"CO installation is correct and up-to-date.")
        sys.exit(2)
    }
  }
}

