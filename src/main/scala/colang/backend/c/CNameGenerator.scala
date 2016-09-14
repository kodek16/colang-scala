package colang.backend.c

import colang.ast.parsed.{Constructor, Method, ObjectMember, ReferenceType, Symbol}

import scala.collection.mutable

/**
  * A component that is responsible for generating (and storing) C names for different kinds of entities.
  */
trait CNameGenerator {

  // Implementations must return the same name for each argument on every call.
  def nameFor(symbol: Symbol): String
  def nameFor(objectMember: ObjectMember): String
  def nameFor(constructor: Constructor): String

  // After calling these methods with a specific name, this name must be returned from all consequent nameFor calls.
  def setNativeNameFor(symbol: Symbol, name: String): Unit
  def setNativeNameFor(method: Method, name: String): Unit
  def setNativeNameFor(constructor: Constructor, name: String): Unit
}

class CVerboseNameGenerator extends CNameGenerator {
  private val generatedNames = mutable.Map.empty[Any, String]
  private val nameUsages = mutable.Map.empty[String, Int]

  def nameFor(symbol: Symbol): String = {
    if (generatedNames contains symbol) {
      generatedNames(symbol)
    } else {
      symbol match {
        case rt: ReferenceType => nameFor(rt.referenced) + "*"
        case _ =>
          val name = adjustName("co_" + sanitizeName(symbol.qualifiedName))
          generatedNames(symbol) = name
          name
      }
    }
  }

  def nameFor(objectMember: ObjectMember): String = {
    if (generatedNames contains objectMember) {
      generatedNames(objectMember)
    } else {
      val name = adjustName(s"co_${sanitizeName(objectMember.container.qualifiedName)}_${sanitizeName(objectMember.name)}")
      generatedNames(objectMember) = name
      name
    }
  }

  def nameFor(constructor: Constructor): String = {
    if (generatedNames contains constructor) {
      generatedNames(constructor)
    } else {
      val name = adjustName("co_" + sanitizeName(constructor.type_.qualifiedName) + "_ctor")
      generatedNames(constructor) = name
      name
    }
  }

  private def adjustName(suggestedName: String): String = {
    if (nameUsages contains suggestedName) {
      val usages = nameUsages(suggestedName)
      nameUsages(suggestedName) = usages + 1
      suggestedName + "_" + (usages + 1)
    } else {
      nameUsages(suggestedName) = 1
      suggestedName
    }
  }

  def setNativeNameFor(symbol: Symbol, name: String) = generatedNames(symbol) = name
  def setNativeNameFor(method: Method, name: String) = generatedNames(method) = name
  def setNativeNameFor(constructor: Constructor, name: String) = generatedNames(constructor) = name

  private def sanitizeName(name: String) = name.replaceAll("[^A-Za-z0-9_]", "_")
}
