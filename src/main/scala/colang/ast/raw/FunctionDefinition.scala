package colang.ast.raw

import colang.Strategy.Result
import colang.Strategy.Result.{NoMatch, Success}
import colang.tokens._
import colang.TokenStream
import colang.ast.raw.ParserImpl.{identifierStrategy, SingleTokenStrategy}

/**
  * Represents a function (or a method) definition.
  * @param specifiers function specifiers list
  * @param returnType function return type
  * @param name function name
  * @param parameterList function parameter list
  * @param body function body, may be absent
  */
case class FunctionDefinition(specifiers: SpecifiersList,
                              returnType : Type,
                              name: Identifier,
                              parameterList: ParameterList,
                              body: Option[CodeBlock]) extends GlobalSymbolDefinition {

  lazy val source = specifiers.source + body.getOrElse(parameterList).source
  lazy val prototypeSource = specifiers.source + parameterList.source
}

object FunctionDefinition {
  val strategy = new ParserImpl.Strategy[FunctionDefinition] {

    private val specifiersStrategy = new SpecifiersList.Strategy(
      classOf[NativeKeyword])

    def apply(stream: TokenStream): Result[TokenStream, FunctionDefinition] = {
      ParserImpl.parseGroup()
        .element(specifiersStrategy,                          "function specifiers")
        .element(Type.strategy,                               "function return type",     stopIfAbsent = true)
        .element(identifierStrategy,                          "function name",            stopIfAbsent = true)
        .element(ParameterList.strategy,                      "function parameter list",  stopIfAbsent = true)
        .element(CodeBlock.strategy,                          "function body",            optional = true)
        .parse(stream)
        .as[SpecifiersList, Type, Identifier, ParameterList, CodeBlock] match {

        case (Some(specifiersList), Some(returnType), Some(name), Some(parameterList), bodyOption, issues, streamAfterFunction) =>
          Success(FunctionDefinition(specifiersList, returnType, name, parameterList, bodyOption), issues, streamAfterFunction)
        case _ => NoMatch()
      }
    }
  }
}