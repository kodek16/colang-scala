package colang.ast.raw

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.ast.raw.ParserImpl._
import colang.issues.Terms
import colang.tokens._
import colang.{SourceCode, TokenStream}

/**
  * Represents a type definition.
  * @param specifiers type specifiers list
  * @param keyword 'struct' (or eventually 'class') keyword
  * @param name type name
  * @param body type body, may be absent
  */
case class TypeDefinition(specifiers: SpecifiersList,
                          keyword: Keyword,
                          name: Identifier,
                          body: Option[TypeBody]) extends GlobalSymbolDefinition {

  def source: SourceCode = specifiers.source + body.getOrElse(name).source
  def headSource: SourceCode = specifiers.source + name.source

  override def toString: String = s"TypeDef: ${name.value}"
}

object TypeDefinition {
  val strategy = new ParserImpl.Strategy[TypeDefinition] {

    private val specifiersStrategy = new SpecifiersList.Strategy(
      Terms.Definition of Terms.Type,
      classOf[NativeKeyword])

    def apply(stream: TokenStream): Result[TokenStream, TypeDefinition] = {
      ParserImpl.parseGroup()
        .optionalElement(specifiersStrategy)    //Actually always present rather than optional.
        .definingElement(SingleTokenStrategy(classOf[StructKeyword]))
        .element(identifierStrategy, Terms.Name of Terms.Type)
        .optionalElement(TypeBody.strategy)
        .parse(stream)
        .as[SpecifiersList, Keyword, Identifier, TypeBody] match {

        case (Present(specifiersList), Present(keyword), Present(name), bodyOption, issues, streamAfterType) =>
          val typeDef = TypeDefinition(specifiersList, keyword, name, bodyOption.toOption)
          Success(typeDef, issues, streamAfterType)
        case (Present(_), Present(_) | Invalid(), Invalid() | Absent(), _, issues, streamAfterType) =>
          Malformed(issues, streamAfterType)
        case _ => NoMatch()
      }
    }
  }
}

/**
  * Represents a type body, which can currently only contain instance methods.
  * @param leftBrace opening brace
  * @param methods type methods
  * @param rightBrace closing brace
  */
case class TypeBody(leftBrace: LeftBrace, methods: Seq[FunctionDefinition], rightBrace: RightBrace) extends Node {
  def source: SourceCode = leftBrace.source + rightBrace.source
}

object TypeBody {
  val strategy = new ParserImpl.Strategy[TypeBody] {

    def apply(stream: TokenStream): Result[TokenStream, TypeBody] = {
      ParserImpl.parseEnclosedSequence(
        stream = stream,
        sequenceDescription = Terms.Body of Terms.Type,
        elementStrategy = FunctionDefinition.strategy,
        elementDescription = Terms.Definition of Terms.Method,
        openingElement = classOf[LeftBrace],
        closingElement = classOf[RightBrace],
        closingElementDescription = Terms.ClosingBrace
      ) match {
        case Some((leftBrace, elements, rightBraceOption, issues, streamAfterBlock)) =>
          val rightBrace = rightBraceOption match {
            case Some(rb) => rb
            case None =>
              val previousSource = if (elements.nonEmpty) elements.last.source else leftBrace.source
              RightBrace(previousSource.after)
          }

          Success(TypeBody(leftBrace, elements, rightBrace), issues, streamAfterBlock)
        case None => NoMatch()
      }
    }
  }
}
