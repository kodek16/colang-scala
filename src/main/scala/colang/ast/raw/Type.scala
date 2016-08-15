package colang.ast.raw

import colang.MappedStrategy
import colang.ast.raw.ParserImpl.identifierStrategy
import colang.tokens.Identifier

/**
  * Represents a type reference.
  * @param name type name
  */
case class Type(name: Identifier) extends Node {
  def source = name.source
}

object Type {
  val strategy = new MappedStrategy(identifierStrategy, Type.apply)
}