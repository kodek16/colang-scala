package colang.ast.raw

import colang.Strategy.Result
import colang.Strategy.Result.{Skipped, NoMatch, Matched}
import colang.TokenStream
import colang.ast.raw.ParserImpl.{Present, SingleTokenStrategy}
import colang.issues.Terms
import colang.tokens.{ConstructorKeyword, NativeKeyword}

/**
  * Represents a type constructor definition.
  * @param specifiers constructor specifiers list
  * @param constructorKeyword 'constructor' keyword
  * @param parameterList constructor parameter list
  * @param body constructor body, may be absent
  */
case class ConstructorDefinition(specifiers: SpecifiersList,
                                 constructorKeyword: ConstructorKeyword,
                                 parameterList: ParameterList,
                                 body: Option[CodeBlock]) extends TypeMemberDefinition {

  def source = specifiers.source + body.getOrElse(parameterList).source
  def prototypeSource = specifiers.source + parameterList.source
}

object ConstructorDefinition {
  val strategy = new ParserImpl.Strategy[ConstructorDefinition] {

    private val specifiersStrategy = new SpecifiersList.Strategy(
      Terms.Definition of Terms.Constructor,
      classOf[NativeKeyword])

    def apply(stream: TokenStream): Result[TokenStream, ConstructorDefinition] = {
      ParserImpl.parseGroup()
        .optionalElement(specifiersStrategy)
        .definingElement(SingleTokenStrategy(classOf[ConstructorKeyword]))
        .element(ParameterList.strategy, Terms.List of Terms.Parameters)
        .optionalElement(CodeBlock.strategy)
        .parse(stream)
        .as[SpecifiersList, ConstructorKeyword, ParameterList, CodeBlock] match {

        case (Present(specifiers), Present(constructorKw), Present(parameterList), bodyOption,
              issues, streamAfterConstructor) =>
          val definition = ConstructorDefinition(specifiers, constructorKw, parameterList, bodyOption.toOption)
          Matched(definition, issues, streamAfterConstructor)

        case (Present(specifiers), Present(constructorKw), _, _, issues, streamAfterConstructor) =>
          Skipped(issues, streamAfterConstructor)

        case _ => NoMatch()
      }
    }
  }
}
