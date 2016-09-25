package colang.ast.parsed

import colang.SourceCode
import colang.issues.Term

/**
  * Represents an entity that depends on an instance of given type.
  */
trait ObjectMember {

  /**
    * Member name
    */
  def name: String

  /**
    * Optionally a code fragment pointing to the symbol definition.
    */
  def definitionSite: Option[SourceCode]

  /**
    * Containing type.
    */
  def container: Type

  /**
    * A term describing what kind of member the object is.
    */
  def description: Term

  lazy val qualifiedName: String = s"${container.qualifiedName}.$name"
}
