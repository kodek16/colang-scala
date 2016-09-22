package colang.ast.parsed

import colang.ast.parsed.expression.{Expression, ImplicitDereferencing}
import colang.ast.raw
import colang.ast.raw.TypeDefinition
import colang.issues.{Issue, Issues, Terms}

/**
  * Represents a type. Different type aspects are defined in separate traits to keep this file smaller.
  * All types are divided into reference and non-reference types: each of these categories has its own concrete
  * Type subclass.
  * @param name type name
  * @param scope enclosing scope
  * @param definition raw type definition
  * @param native whether type is native
  */
abstract class Type(val name: String,
                    val scope: Some[Scope],
                    val definition: Option[TypeDefinition],
                    val native: Boolean = false) extends Symbol
                                                 with Scope
                                                 with ObjectMemberContainer
                                                 with ConstructorContainer {

  val parent = scope
  val definitionSite = definition map { _.headSource }

  val description = Terms.Type

  // A default constructor is added for every type (it shouldn't be, need to check if the type is Plain)
  // TODO don't do this if the type isn't plain
  addConstructor(generateDefaultConstructor)

  // A copy constructor is added for every type.
  addConstructor(generateCopyConstructor)

  private def generateDefaultConstructor: Constructor = {
    val localContext = LocalContext(applicableKind = Terms.Constructor, expectedReturnType = None)

    val body = new CodeBlock(new LocalScope(Some(this)), localContext, None)

    new Constructor(
      type_ = this,
      parameters = Seq.empty,
      body = body,
      definition = None,
      native = true)
  }

  private def generateCopyConstructor: Constructor = {
    val localContext = LocalContext(applicableKind = Terms.Constructor, expectedReturnType = None)
    val body = new CodeBlock(new LocalScope(Some(this)), localContext, None)
    val params = Seq(Variable(
      name = "other",
      scope = Some(body.innerScope),
      type_ = this,
      definition = None))

    new Constructor(
      type_ = this,
      parameters = params,
      body = body,
      definition = None,
      native = true)
  }

  def defaultConstructor: Option[Constructor] = resolveConstructor(Seq.empty, None)._1

  def copyConstructor: Constructor = resolveConstructor(Seq(this), None)._1.get

  /**
    * Returns true if a type can be implicitly converted to another type.
    * Note that subclasses may often override this method. This, for example, is the case with ReferenceType.
    * @param other target type
    * @return whether implicit conversion is possible
    */
  def isImplicitlyConvertibleTo(other: Type): Boolean = this eq other

  /**
    * Returns the most specific type that both types are implicitly convertible to, or None.
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

/**
  * Represents a non-reference type.
  */
class NonReferenceType(name: String,
                       scope: Some[Scope],
                       definition: Option[raw.TypeDefinition],
                       native: Boolean = false) extends Type(name, scope, definition, native) {

  /**
    * Returns a reference type associated with this type.
    * @return reference type
    */
  lazy val reference: ReferenceType = new ReferenceType(this)
}

/**
  * Represents a reference type.
  * Never construct those manually, use Type reference method instead.
  * @param referenced referenced type.
  */
class ReferenceType(val referenced: Type) extends Type(
  name = referenced.name + "&",
  scope = referenced.scope,
  definition = None,
  native = true) {

  // A default assign method is generated for every reference type.
  addObjectMember(defaultAssignMethod)

  private def defaultAssignMethod: Method = {
    val localContext = LocalContext(applicableKind = Terms.Method, expectedReturnType = Some(this))
    val body = new CodeBlock(new LocalScope(Some(this)), localContext, None)
    val params = Seq(Variable(
      name = "other",
      scope = Some(body.innerScope),
      type_ = referenced,
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

  // References can be implicitly converted to their referenced types,
  // AND to types they can be implicitly converted to.
  override def isImplicitlyConvertibleTo(other: Type): Boolean = {
    (this eq other) || (referenced isImplicitlyConvertibleTo other)
  }
}

object Type {

  def resolve(scope: Scope, rawType: raw.Type): (Type, Seq[Issue]) = {
    rawType match {
      case r: raw.SimpleType =>
        scope.resolve(r.name.value) match {
          case Some(type_ : Type) => (type_, Seq.empty)
          case Some(otherSymbol) =>
            val issue = Issues.InvalidReferenceAsType(rawType.source, otherSymbol.description)
            (scope.root.unknownType, Seq(issue))

          case None =>
            val issue = Issues.UnknownName(rawType.source, ())
            (scope.root.unknownType, Seq(issue))
        }

      case r: raw.ReferenceType =>
        val (referenced, referencedIssues) = Type.resolve(scope, r.referenced)
        referenced match {
          case t: NonReferenceType => (t.reference, referencedIssues)

          // Normally, overreferenced types have already been checked in the parser, so this should never be the case.
          case rt: ReferenceType =>
            val issue = Issues.OverreferencedType(r.source, referenced.qualifiedName)
            (scope.root.unknownType, referencedIssues :+ issue)
        }
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

  /**
    * Coerces the expression into given types, performing type conversion if necessary.
    * This function should only be called when implicit conversion was checked to be possible.
    * @param from expression to convert
    * @param to target type
    * @return resulting expression
    */
  def performImplicitConversion(from: Expression, to: Type): Expression = {
    from.type_ match {
      case `to` => from
      case rt: ReferenceType if rt.referenced == to => ImplicitDereferencing(from)
      case _ => throw new IllegalArgumentException("can't perform implicit conversion")
    }
  }

  /**
    * Coerces expressions to given types, performing implicit type conversions where necessary.
    * This function should only be called when expressions are checked to be implicitly convertible.
    * @param from expressions to convert
    * @param to target types
    * @return resulting expressions
    */
  def performImplicitConversions(from: Seq[Expression], to: Seq[Type]): Seq[Expression] = {
    if (from.size != to.size) {
      throw new IllegalArgumentException("source and target lists have different length")
    }

    (from zip to) map (performImplicitConversion _).tupled
  }
}
