package colang.ast.parsed

import colang.{Error, Issue}

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Represents a set of symbols accessible only in a certain part of the program.
  */
trait Scope {

  /**
    * Parent scope. Can be None only for root namespace.
    */
  def parent: Option[Scope]

  /**
    * A default LinkedHashMap-based scope implementation is provided, implementers may override resolve(), tryAdd(),
    * and allMembers() methods if they wish.
    */
  protected val members: mutable.LinkedHashMap[String, Symbol] = mutable.LinkedHashMap.empty

  /**
    * Resolves a name in the current scope.
    * @param name symbol name
    * @return the symbol with the name in the closest scope, or None if it doesn't exist
    */
  def resolve(name: String): Option[Symbol] = {
    @tailrec
    def findSymbol(scope: Scope): Option[Symbol] = {
      scope.members get name match {
        case Some(symbol) => Some(symbol)
        case None => scope.parent match {
          case Some(parentScope) => findSymbol(parentScope)
          case None => None
        }
      }
    }

    findSymbol(this)
  }

  /**
    * Tries to register a symbol in the scope.
    * @param symbol detached symbol
    * @return an issue if registration failed
    */
  def tryAdd(symbol: Symbol): Option[Issue] = {
    members get symbol.name match {
      case Some(existingSymbol) =>
        val issue = Error(symbol.declarationSite.get,
          s"there is already ${existingSymbol.description} with the same name in this scope",
          existingSymbol.declarationSiteNotes)

        Some(issue)

      case None =>
        members(symbol.name) = symbol
        None
    }
  }

  /**
    * Returns a Seq containing all members of this scope.
    * @return all members in a Seq
    */
  def allMembers: Seq[Symbol] = members.values.toSeq

  /**
    * A reference to the root namespace.
    */
  lazy val root: Namespace = {
    parent match {
      case Some(scope) => scope.root
      case None => this.asInstanceOf[Namespace]
    }
  }
}

/**
  * Represents a local scope, that is a function scope or its direct or indirect descendant.
  * @param parent parent scope
  */
class LocalScope(val parent: Some[Scope]) extends Scope