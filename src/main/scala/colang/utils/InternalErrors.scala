package colang.utils

import colang.utils.Localization._

/**
  * Errors that "should never happen".
  */
object InternalErrors {

  def missingPrelude: Nothing = {
    System.err.println(tr("prelude_not_found"))
    sys.exit(2)
  }

  def primitiveTypeIsNotAType(typeName: String): Nothing = {
    System.err.println(tr("primitive_type_is_not_a_type") format typeName)
    sys.exit(2)
  }

  def missingPrimitiveType(typeName: String): Nothing = {
    System.err.println(tr("missing_primitive_type") format typeName)
    sys.exit(2)
  }

  def noNativeSymbol(name: String): Nothing = {
    System.err.println(tr("no_native_symbol"))
    sys.exit(2)
  }
}
