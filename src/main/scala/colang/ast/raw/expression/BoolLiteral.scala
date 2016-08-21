package colang.ast.raw.expression

import colang.ast.raw.ParserImpl.SingleTokenStrategy
import colang.{MappedStrategy, tokens}

/**
  * A wrapper for bool literal tokens.
  * @param value underlying bool literal token
  */
case class BoolLiteral(value: tokens.BoolLiteral) extends Expression {
  def source = value.source
}

object BoolLiteral {
  val strategy = MappedStrategy(SingleTokenStrategy(classOf[tokens.BoolLiteral]), BoolLiteral.apply)
}