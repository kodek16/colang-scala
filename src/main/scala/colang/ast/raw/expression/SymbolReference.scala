package colang.ast.raw.expression

import colang.ast.raw.ParserImpl.identifierStrategy
import colang.tokens.Identifier
import colang.{MappedStrategy, SourceCode}

/**
  * Represents a stable symbol reference.
  * @param name symbol name
  */
case class SymbolReference(name: Identifier) extends Expression {
  def source: SourceCode = name.source
}

object SymbolReference {
  val strategy = MappedStrategy(identifierStrategy, SymbolReference.apply)
}
