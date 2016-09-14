package colang.ast.parsed

import colang.ast.parsed.expression.{Expression, ImplicitDereferencing}
import colang.ast.raw
import colang.ast.raw.TypeDefinition
import colang.issues.{Issue, Issues, Terms}

/**
  * Represents a type. Different type aspects are defined in separate traits to keep the file smaller.
  * @param name type name
  * @param scope enclosing scope
  * @param definition raw type definition
  * @param native whether type is native
  */
class Type(val name: String,
           val scope: Some[Scope],
           val definition: Option[TypeDefinition],
           val native: Boolean = false) extends Symbol with Scope with ObjectMemberContainer with ConstructorContainer {

  val parent = scope
  val definitionSite = definition match {
    case Some(td) => Some(td.headSource)
    case None => None
  }

  val description = Terms.Type

  // TODO ReferenceType throwing here is not the best solution, it should be probably eventually refactored using traits
  // TODO and templates
  /**
    * Returns a reference type for this type.
    * WARNING: This will throw when used on ReferenceType, because of overrefencing. Be careful when you use it.
    * @return reference type
    */
  lazy val reference: ReferenceType = new ReferenceType(this)

  // A default constructor is added for every type (it shouldn't be, need to check if the type is Plain)
  defaultConstructor foreach addConstructor

  // A copy constructor is added for every type.
  addConstructor(copyConstructor)

  // TODO return None if the type isn't plain
  lazy val defaultConstructor: Option[Constructor] = {
    // HACK: the LocalContext object has 'this' as expectedReturnType. Semantically for constructors it should be
    // 'void', but if we specify 'root.voidType' problems will arise because 'void' type may be not defined yet.
    // The LocalContext object is never used in native functions (since they have no body to parse with this context),
    // so it should be OK.
    val localContext = LocalContext(applicableKind = Terms.Constructor, expectedReturnType = this)

    val body = new CodeBlock(new LocalScope(Some(this)), localContext, None)

    Some(new Constructor(
      type_ = this,
      parameters = Seq.empty,
      body = body,
      native = true))
  }

  lazy val copyConstructor: Constructor = {
    // HACK: the LocalContext object has 'this' as expectedReturnType. Semantically for constructors it should be
    // 'void', but if we specify 'root.voidType' problems will arise because 'void' type may be not defined yet.
    // The LocalContext object is never used in native functions (since they have no body to parse with this context),
    // so it should be OK.
    val localContext = LocalContext(applicableKind = Terms.Constructor, expectedReturnType = this)
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
      native = true)
  }

  /**
    * Returns true if a type can be implicitly converted to another type.
    * Note that subclasses may often override this method. This, for example, is the case with ReferenceType.
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

  override lazy val reference: ReferenceType = throw new IllegalArgumentException("cannot reference a reference type")

  // A default assign method is generated for every reference type.
  addObjectMember(defaultAssignMethod)

  private def defaultAssignMethod: Method = {
    val localContext = LocalContext(applicableKind = Terms.Method, expectedReturnType = this)
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
        (referenced.reference, referencedIssues)
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
