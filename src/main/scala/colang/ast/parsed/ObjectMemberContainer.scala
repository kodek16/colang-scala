package colang.ast.parsed

import colang.issues.{Issue, Issues}

import scala.collection.mutable

/**
  * A trait that describes the aspects of type that concern instance fields and methods.
  */
trait ObjectMemberContainer { this: Type =>

  private val methods: mutable.LinkedHashMap[String, Method] = mutable.LinkedHashMap.empty

  /**
    * Resolves a method name.
    * @param name method name
    * @return the method with given name, or None if it doesn't exist
    */
  def resolveMethod(name: String): Option[Method] = methods get name

  /**
    * Adds a method unconditionally, throws if it can't be done.
    * @param method detached method
    */
  def addMethod(method: Method): Unit = {
    val issues = tryAddMethod(method)
    if (issues.nonEmpty) throw new IllegalArgumentException("couldn't add the method")
  }

  /**
    * Tries to register a method in the type.
    * @param method detached method
    * @return an issue if registration failed
    */
  def tryAddMethod(method: Method): Seq[Issue] = {
    methods get method.name match {
      case Some(existingMethod) if !existingMethod.native =>
        val issue = Issues.DuplicateMethodDefinition(method.definitionSite.get, existingMethod.definitionSite)
        Seq(issue)

      case None =>
        methods(method.name) = method
        Seq.empty
    }
  }

  /**
    * Returns a Seq containing all methods of this type.
    * @return all methods in a Seq
    */
  def allMethods: Seq[Method] = methods.values.toSeq
}
