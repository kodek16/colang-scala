package colang.ast.parsed

import colang.issues.{Issue, Issues}

import scala.collection.mutable

/**
  * A trait that describes the aspects of type that concern instance fields and methods.
  */
trait ObjectMemberContainer { this: Type =>

  private val objectMembers: mutable.LinkedHashMap[String, ObjectMember] = mutable.LinkedHashMap.empty

  /**
    * Resolves an object member name.
    * @param name object member name
    * @return field or method with given name, or None if it doesn't exist
    */
  def resolveObjectMember(name: String): Option[ObjectMember] = objectMembers get name

  /**
    * Adds an object member unconditionally, throws if it can't be done.
    * @param objectMember detached object member
    */
  def addObjectMember(objectMember: ObjectMember): Unit = {
    val issues = tryAddObjectMember(objectMember)
    if (issues.nonEmpty) throw new IllegalArgumentException("couldn't add object member")
  }

  /**
    * Tries to register an object member in the type. Will overload methods if necessary and possible.
    * @param objectMember detached object member
    * @return issues encountered while registering
    */
  def tryAddObjectMember(objectMember: ObjectMember): Seq[Issue] = {
    objectMembers get objectMember.name match {
      case Some(om: OverloadedMethod) if objectMember.isInstanceOf[Method] =>
        om.tryAddOverload(objectMember.asInstanceOf[Method])

      case Some(m: Method) if objectMember.isInstanceOf[Method] =>
        val overloadedMethod = new OverloadedMethod(m.name, m.container)
        overloadedMethod.tryAddOverload(m)  // Will always succeed
        val issues = overloadedMethod.tryAddOverload(objectMember.asInstanceOf[Method])

        objectMembers -= m.name
        objectMembers(overloadedMethod.name) = overloadedMethod
        issues

      case Some(existingMember) =>
        val issue = Issues.EntityNameTaken(objectMember.definitionSite.get,
          (existingMember.description, existingMember.definitionSite))
        Seq(issue)

      case None =>
        objectMembers(objectMember.name) = objectMember
        Seq.empty
    }
  }

  /**
    * Returns a Seq containing all methods of this type.
    * @return all methods in a Seq
    */
  def allMethods: Seq[Method] = objectMembers.values.toSeq flatMap {
    case m: Method => Some(m)
    case _ => None
  }

  /**
    * Returns a Seq containing all fields of this type.
    * @return all fields in a Seq
    */
  def allFields: Seq[Field] = objectMembers.values.toSeq flatMap {
    case f: Field => Some(f)
    case _ => None
  }
}
