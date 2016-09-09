package colang.ast.raw

import colang.Strategy.Result
import colang.Strategy.Result.{NoMatch, Success}
import colang.ast.raw.ParserImpl.{Present, SingleTokenStrategy, identifierStrategy}
import colang.issues.{Issues, Terms}
import colang.tokens.{Ampersand, Identifier, LogicalAnd}
import colang.{MappedStrategy, SourceCode, StrategyUnion, TokenStream}

/**
  * Represents a type reference.
  */
sealed trait Type extends Node

object Type {
  val strategy = StrategyUnion(
    ReferenceType.strategy,
    SimpleType.strategy)
}

/**
  * Represents a simple (non-reference) type reference.
  * @param name type name
  */
case class SimpleType(name: Identifier) extends Type {
  def source = name.source
}

object SimpleType {
  val strategy = new MappedStrategy(identifierStrategy, SimpleType.apply)
}

/**
  * Represents a reference type.
  * @param referenced referenced simple type
  * @param ampersand ampersand
  */
case class ReferenceType(referenced: Type, ampersand: Ampersand) extends Type {
  def source = referenced.source + ampersand.source
}

object ReferenceType {

  /**
    * A strategy that matches valid reference types.
    */
  private val validStrategy = new ParserImpl.Strategy[ReferenceType] {

    def apply(stream: TokenStream): Result[TokenStream, ReferenceType] = {
      ParserImpl.parseGroup()
        .definingElement(SimpleType.strategy)
        .definingElement(SingleTokenStrategy(classOf[Ampersand]))
        .parse(stream)
        .as[SimpleType, Ampersand] match {

        case (Present(simpleType), Present(ampersand), issues, streamAfterType) =>
          Success(ReferenceType(simpleType, ampersand), issues, streamAfterType)

        case _ => NoMatch()
      }
    }
  }

  /**
    * A strategy that matches "overreferenced" type with multiple ampersands (e.g. 'int&&').
    */
  private val invalidStrategy = new ParserImpl.Strategy[Type] {

    private val anyAmpersandStrategy = StrategyUnion(
      SingleTokenStrategy(classOf[Ampersand]),
      SingleTokenStrategy(classOf[LogicalAnd]))

    private case class AmpersandSequence(source: SourceCode, count: Int) extends Node

    private object AmpersandSequence {
      val strategy = new ParserImpl.Strategy[AmpersandSequence] {
        def apply(stream: TokenStream): Result[TokenStream, AmpersandSequence] = {
          ParserImpl.parseSequence(
            stream = stream,
            elementStrategy = anyAmpersandStrategy,
            elementDescription = Terms.Ampersand
          ) match {
            case (tokens, issues, streamAfterTokens) =>
              if (tokens.nonEmpty) {
                val count = (tokens map {
                  case a: Ampersand => 1
                  case aa: LogicalAnd => 2
                }).sum

                Success(AmpersandSequence(tokens.head.source + tokens.last.source, count), issues, streamAfterTokens)
              } else NoMatch()
          }
        }
      }
    }

    def apply(stream: TokenStream): Result[TokenStream, Type] = {
      ParserImpl.parseGroup()
        .definingElement(SimpleType.strategy)
        .definingElement(AmpersandSequence.strategy)
        .parse(stream)
        .as[SimpleType, AmpersandSequence] match {

        case (Present(simpleType), Present(ampersandSequence), issues, streamAfterType)
          if ampersandSequence.count >= 2 =>
          val referencedTypeName = simpleType.name.value + ("&" * (ampersandSequence.count - 1))
          val issue = Issues.OverreferencedType(simpleType.source + ampersandSequence.source, referencedTypeName)
          Success(simpleType, issues :+ issue, streamAfterType)

        case _ => NoMatch()
      }
    }
  }

  val strategy = StrategyUnion(
    invalidStrategy,
    validStrategy)
}