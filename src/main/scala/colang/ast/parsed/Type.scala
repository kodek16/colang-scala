package colang.ast.parsed

import colang.ast.raw
import colang.ast.raw.TypeDefinition
import colang.Error
import colang.issues.{Error, Issue}

import scala.collection.mutable

/**
  * Represents a type.
  * @param name type name
  * @param scope enclosing scope
  * @param definition raw type definition
  * @param native whether type is native
  */
class Type(val name: String,
           val scope: Some[Scope],
           val definition: Option[TypeDefinition],
           val native: Boolean = false) extends Symbol with Scope {

  val parent = scope
  val declarationSite = definition match {
    case Some(td) => Some(td.headSource)
    case None => None
  }
  val description = "type"

  private val methods: mutable.LinkedHashMap[String, Method] = mutable.LinkedHashMap.empty

  /**
    * A default assign method is generated for every type.
    */
  methods("assign") = defaultAssignMethod

  private def defaultAssignMethod: Method = {
    val body = new CodeBlock(new LocalScope(Some(this)), None)
    val params = Seq(new Variable(
      name = "other",
      scope = Some(body.innerScope),
      type_ = this,
      definition = None))

    new Method(
      name = "assign",
      container = this,
      returnType = this,
      parameters = params,
      body = body,
      definition = None,
      native = true)
  }

  /**
    * Resolves a method name.
    * @param name method name
    * @return the method with given name, or None if it doesn't exist
    */
  def resolveMethod(name: String): Option[Method] = methods get name

  /**
    * Tries to register a method in the type.
    * @param method detached method
    * @return an issue if registration failed
    */
  def tryAddMethod(method: Method): Option[Issue] = {
    methods get method.name match {
      case Some(existingMethod) if !existingMethod.native =>
        val issue = Error(method.declarationSite.get,
          s"there is already a method with the same name for this type",
          existingMethod.declarationSiteNotes)

        Some(issue)

      case None =>
        methods(method.name) = method
        None
    }
  }

  /**
    * Returns a Seq containing all methods of this type.
    * @return all methods in a Seq
    */
  def allMethods: Seq[Method] = methods.values.toSeq

  /**
    * Returns true if a type can be implicitly converted to another type.
    * @param other target type
    * @return whether implicit conversion is possible
    */
  def isImplicitlyConvertibleTo(other: Type): Boolean = this eq other

  /**
    * Returns the most specific type that both types are implicitly convertable to, or None.
    * @param other other type
    * @return optional Least Upper Bound
    */
  def leastUpperBound(other: Type): Option[Type] = {
    if (this isImplicitlyConvertibleTo other) {
      Some(other)
    } else if (other isImplicitlyConvertibleTo this) {
      Some(this)
    } else None
  }
}

object Type {

  def resolve(scope: Scope, rawType: raw.Type): (Type, Seq[Issue]) = {
    scope.resolve(rawType.name.value) match {
      case Some(type_ : Type) => (type_, Seq.empty)
      case Some(otherSymbol) =>
        val issue = Error(
          rawType.source,
          s"${rawType.name.value} is not a type, but a ${otherSymbol.description}",
          otherSymbol.declarationSiteNotes)

        (scope.root.unknownType, Seq(issue))

      case None =>
        val issue = Error(rawType.source, s"there is no type named ${rawType.name.value} is this scope", Seq.empty)
        (scope.root.unknownType, Seq(issue))
    }
  }

  /**
    * Calculates Least Upper Bound of multiple types. See leastUpperBound instance method.
    * @param types types
    * @return Least Upper Bound
    */
  def leastUpperBound(types: Type*): Option[Type] = {
    if (types.isEmpty) {
      None
    } else {
      (types foldLeft Some(types.head).asInstanceOf[Option[Type]]) { (lhsOption, rhs) =>
        lhsOption match {
          case Some(lhs) => lhs leastUpperBound rhs
          case None => None
        }
      }
    }
  }
}
