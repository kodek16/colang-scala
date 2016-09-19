package colang.ast.raw

import colang.Strategy.Result
import colang.Strategy.Result.{NoMatch, Success}
import colang.TokenStream
import colang.ast.raw.ParserImpl.{Present, SingleTokenStrategy, identifierStrategy}
import colang.issues.Terms
import colang.tokens._

/**
  * Represents either function or a method definition.
  * @param specifiers function specifiers list
  * @param returnType function return type
  * @param name function name
  * @param parameterList function parameter list
  * @param referenceMarker method definitions may contain an '&' after name that effectively moves the method from
  *                        type 'T' to type 'T&'. This should always be None in function (not method) definitions.
  * @param body function body, may be absent
  */
case class FunctionDefinition(specifiers: SpecifiersList,
                              returnType : Type,
                              name: Identifier,
                              referenceMarker: Option[Ampersand],
                              parameterList: ParameterList,
                              body: Option[CodeBlock]) extends GlobalSymbolDefinition with TypeMemberDefinition {

  lazy val source = specifiers.source + body.getOrElse(parameterList).source
  lazy val prototypeSource = specifiers.source + parameterList.source

  override def toString: String = s"FuncDef: ${name.value}"
}

object FunctionDefinition {
  val strategy = new ParserImpl.Strategy[FunctionDefinition] {

    private val specifiersStrategy = new SpecifiersList.Strategy(
      Terms.Definition of Terms.Function,
      classOf[NativeKeyword])

    def apply(stream: TokenStream): Result[TokenStream, FunctionDefinition] = {
      ParserImpl.parseGroup()
        .optionalElement(specifiersStrategy)    //Actually always present rather than optional.
        .definingElement(Type.strategy)
        .definingElement(identifierStrategy)
        .optionalElement(SingleTokenStrategy(classOf[Ampersand]))
        .definingElement(ParameterList.strategy)
        .optionalElement(CodeBlock.strategy)
        .parse(stream)
        .as[SpecifiersList, Type, Identifier, Ampersand, ParameterList, CodeBlock] match {

        case (Present(specifiersList), Present(returnType), Present(name), refMarkerOption, Present(parameterList),
              bodyOption, issues, streamAfterFunction) =>

          val funcDef = FunctionDefinition(
            specifiersList,
            returnType,
            name,
            refMarkerOption.toOption,
            parameterList,
            bodyOption.toOption)

          Success(funcDef, issues, streamAfterFunction)

        case _ => NoMatch()
      }
    }
  }
}
