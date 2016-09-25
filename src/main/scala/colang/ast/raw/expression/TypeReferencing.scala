package colang.ast.raw.expression

import colang.MappedStrategy
import colang.ast.raw.ParserImpl
import colang.ast.raw.ParserImpl.SingleTokenStrategy
import colang.tokens.Ampersand

/**
  * Represents a "referencing" type expression.
  * @param referencedType referenced type
  * @param ampersand '&' token
  */
case class TypeReferencing(referencedType: Expression, ampersand: Ampersand) extends Expression {
  def source = referencedType.source + ampersand.source
}

object TypeReferencing {
  val strategy: ParserImpl.Strategy[PostfixOperator] = MappedStrategy(
    SingleTokenStrategy(classOf[Ampersand]),
    (ampersand: Ampersand) => new PostfixOperator {
      def apply = referencedType => TypeReferencing(referencedType, ampersand)
      def source = ampersand.source
    })
}
