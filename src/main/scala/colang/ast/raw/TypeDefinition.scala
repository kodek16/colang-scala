package colang.ast.raw

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.ast.raw.ParserImpl._
import colang.ast.raw.statement.VariablesDefinition
import colang.issues.Terms
import colang.tokens._
import colang.{SourceCode, StrategyUnion, TokenStream}

/**
  * Represents a type definition.
  * @param specifiers type specifiers list
  * @param keyword 'struct' (or eventually 'class') keyword
  * @param name type name
  * @param body type body
  */
case class TypeDefinition(specifiers: SpecifiersList,
                          keyword: Keyword,
                          name: Identifier,
                          body: TypeBody) extends GlobalSymbolDefinition with TypeMemberDefinition {

  def source: SourceCode = specifiers.source + body.source
  def headSource: SourceCode = specifiers.source + name.source

  override def toString: String = s"TypeDef: ${name.value}"
}

object TypeDefinition {
  val strategy: ParserImpl.Strategy[TypeDefinition] = new ParserImpl.Strategy[TypeDefinition] {

    private val specifiersStrategy = new SpecifiersList.Strategy(
      Terms.Definition of Terms.Type,
      classOf[NativeKeyword])

    def apply(stream: TokenStream): Result[TokenStream, TypeDefinition] = {
      ParserImpl.parseGroup()
        .optionalElement(specifiersStrategy)    //Actually always present rather than optional.
        .definingElement(SingleTokenStrategy(classOf[StructKeyword]))
        .element(identifierStrategy, Terms.Name of Terms.Type)
        .element(TypeBody.strategy, Terms.Body of Terms.Type)
        .parse(stream)
        .as[SpecifiersList, Keyword, Identifier, TypeBody] match {

        case (Present(specifiersList), Present(keyword), Present(name), Present(body), issues, streamAfterType) =>
          val typeDef = TypeDefinition(specifiersList, keyword, name, body)
          Success(typeDef, issues, streamAfterType)

        case (Present(_), Present(_) | Invalid(), _, _, issues, streamAfterType) =>
          Malformed(issues, streamAfterType)

        case _ => NoMatch()
      }
    }
  }
}

/**
  * Represents a type body.
  * @param leftBrace opening brace
  * @param members type members
  * @param rightBrace closing brace
  */
case class TypeBody(leftBrace: LeftBrace, members: Seq[TypeMemberDefinition], rightBrace: RightBrace) extends Node {
  def source: SourceCode = leftBrace.source + rightBrace.source
}

object TypeBody {
  val strategy = new ParserImpl.Strategy[TypeBody] {

    private val typeMemberStrategy: Strategy[TypeMemberDefinition] = StrategyUnion(
      TypeDefinition.strategy,
      ConstructorDefinition.strategy,
      FunctionDefinition.strategy,
      VariablesDefinition.strategy)

    def apply(stream: TokenStream): Result[TokenStream, TypeBody] = {
      ParserImpl.parseEnclosedSequence(
        stream = stream,
        sequenceDescription = Terms.Body of Terms.Type,
        elementStrategy = typeMemberStrategy,
        elementDescription = Terms.Definition of Terms.Member of Terms.Type,
        openingElement = classOf[LeftBrace],
        closingElement = classOf[RightBrace],
        closingElementDescription = Terms.ClosingBrace
      ) match {
        case Some((leftBrace, members, rightBraceOption, issues, streamAfterBlock)) =>
          val rightBrace = rightBraceOption match {
            case Some(rb) => rb
            case None =>
              val previousSource = if (members.nonEmpty) members.last.source else leftBrace.source
              RightBrace(previousSource.after)
          }

          Success(TypeBody(leftBrace, members, rightBrace), issues, streamAfterBlock)
        case None => NoMatch()
      }
    }
  }
}
