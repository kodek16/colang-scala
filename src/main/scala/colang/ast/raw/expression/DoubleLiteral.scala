package colang.ast.raw.expression

import colang.ast.raw.ParserImpl.SingleTokenStrategy
import colang.{MappedStrategy, tokens}

/**
  * A wrapper for double literal tokens.
  * @param value underlying double literal token
  */
case class DoubleLiteral(value: tokens.DoubleLiteral) extends Expression {
  def source = value.source
}

object DoubleLiteral {
  val strategy = MappedStrategy(SingleTokenStrategy(classOf[tokens.DoubleLiteral]), DoubleLiteral.apply)
}
