package colang.ast.parsed

import colang.SourceCode
import colang.issues.Terms
import colang.utils.InternalErrors

class Namespace(val name: String,
                val definitionSite: Option[SourceCode],
                val parent: Option[Namespace]) extends Symbol with Scope {

  val scope = parent
  val description = Terms.Namespace
}

/**
  * Represents a root namespace and provides some convenient accessors.
  */
class RootNamespace extends Namespace("", None, None) {

  /**
    * A type assigned to values that failed analysis.
    */
  val unknownType: Type = new NonReferenceType(
    name = "(unknown type)",
    definition = None,
    scope = Some(this))


  /**
    * A type assigned to overloaded function references.
    */
  val overloadedFunctionType: Type = new NonReferenceType(
    name = "(unresolved function overload)",
    definition = None,
    scope = Some(this))

  /**
    * A type assigned to bound method references.
    */
  val boundMethodType: Type = new NonReferenceType(
    name = "(bound method)",
    definition = None,
    scope = Some(this))

  lazy val voidType = getPrimitiveType("void")
  lazy val intType = getPrimitiveType("int")
  lazy val doubleType = getPrimitiveType("double")
  lazy val boolType = getPrimitiveType("bool")

  private def getPrimitiveType(name: String): Type = {
    members get name match {
      case Some(t: Type) => t
      case Some(_) => InternalErrors.primitiveTypeIsNotAType(name)
      case None => InternalErrors.missingPrimitiveType(name)
    }
  }
}
