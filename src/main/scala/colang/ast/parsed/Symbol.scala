package colang.ast.parsed

import colang.{Note, SourceCode}

/**
  * Represents a stable named and scoped entity in the program.
  */
trait Symbol {

  /**
    * Symbol name
    */
  def name: String

  /**
    * Optionally a code fragment pointing to the symbol declaration.
    */
  def declarationSite: Option[SourceCode]

  /**
    * Enclosing scope. Can only be None for the root namespace.
    */
  def scope: Option[Scope]

  /**
    * A short string describing what type of symbol the object is.
    */
  def description: String

  /**
    * For global symbols: a full name that can be used to refer to this symbol from the root namespace.
    * For local symbols: simple name.
    */
  lazy val qualifiedName: String = {
    val prefix = scope match {
      case Some(ns: Namespace) if ns.name.nonEmpty => ns.qualifiedName + "."
      case _ => ""
    }

    prefix + name
  }

  /**
    * A convenience method for optionally constructing a note pointing to the declaration site.
    */
  def declarationSiteNotes: Seq[Note] = {
    declarationSite match {
      case Some(site) => Seq(Note(Some(site), "declared here"))
      case None => Seq.empty
    }
  }
}



