package colang.ast.parsed

import colang.ast.raw
import colang.tokens.NativeKeyword
import colang.{Error, Issue}

import scala.collection.mutable

object TypeDefinition {

  private val generatedMethods: mutable.Map[raw.FunctionDefinition, Method] = mutable.Map.empty

  /**
    * Tries to add a type to the namespace, doesn't process methods.
    * @param namespace enclosing namespace
    * @param rawType raw type definition
    * @return (new type object (possibly detached), encountered issues)
    */
  def register(namespace: Namespace, rawType: raw.TypeDefinition): (Type, Seq[Issue]) = {
    val type_ = new Type(
      name = rawType.name.value,
      declarationSite = Some(rawType.source),
      scope = Some(namespace),
      native = rawType.specifiers.has(classOf[NativeKeyword]))

    (type_, namespace.tryAdd(type_).toSeq)
  }

  /**
    * Tries to register every method in the type, doesn't parse bodies.
    * @param type_ type object
    * @param rawType raw type definition
    * @return encountered issues
    */
  def registerMethods(type_ : Type, rawType: raw.TypeDefinition): Seq[Issue] = {
    rawType.body match {
      case Some(body) =>
        body.methods flatMap { md =>
          val (method, methodIssues) = MethodDefinition.register(type_, md)
          generatedMethods(md) = method
          methodIssues
        }
      case None if !type_.native =>
        Seq(Error(rawType.source, "missing type body"))

      case None if type_.native => Seq.empty
    }
  }
}
