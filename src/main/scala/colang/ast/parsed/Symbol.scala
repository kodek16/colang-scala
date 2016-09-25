package colang.ast.parsed

import colang.SourceCode
import colang.issues.Term

/**
  * Represents a stable named and scoped entity in the program.
  */
trait Symbol {

  /**
    * Symbol name
    */
  def name: String

  /**
    * Optionally a code fragment pointing to the symbol definition.
    */
  def definitionSite: Option[SourceCode]

  /**
    * Enclosing scope. Can only be None for the root namespace.
    */
  def scope: Option[Scope]

  /**
    * A term describing what type of symbol the object is.
    */
  def description: Term

  /**
    * For global symbols: a full name that can be used to refer to this symbol from the root namespace.
    * For local symbols: simple name.
    */
  lazy val qualifiedName: String = {
    val prefix = scope match {
      case Some(namedScope: Symbol) if namedScope.name.nonEmpty => namedScope.qualifiedName + "."
      case _ => ""
    }

    prefix + name
  }
}



