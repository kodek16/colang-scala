package colang.ast.raw.expression

import colang.MappedStrategy
import colang.ast.raw.ParserImpl.SingleTokenStrategy
import colang.tokens.ThisKeyword

/**
  * Represents a 'this' expression referencing the current contextual object.
  * @param thisKeyword 'this' keyword
  */
case class ThisReference(thisKeyword: ThisKeyword) extends Expression {
  def source = thisKeyword.source
}

object ThisReference {
  val strategy = MappedStrategy(SingleTokenStrategy(classOf[ThisKeyword]), ThisReference.apply)
}
