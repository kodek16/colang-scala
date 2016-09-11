package colang.ast.raw.expression

import colang.ast.raw.ParserImpl.SingleTokenStrategy
import colang.{MappedStrategy, tokens}

/**
  * A wrapper for int literal tokens.
  * @param value underlying int literal token
  */
case class IntLiteral(value: tokens.IntLiteral) extends Expression {
  def source = value.source
}

object IntLiteral {
  val strategy = MappedStrategy(SingleTokenStrategy(classOf[tokens.IntLiteral]), IntLiteral.apply)
}
