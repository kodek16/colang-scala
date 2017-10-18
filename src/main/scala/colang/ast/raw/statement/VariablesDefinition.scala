package colang.ast.raw.statement

import colang.Strategy.Result
import colang.Strategy.Result.{Skipped, NoMatch, Matched}
import colang.ast.raw.ParserImpl._
import colang.ast.raw._
import colang.ast.raw.expression.Expression
import colang.issues.Terms
import colang.tokens.{Assign, Comma, Identifier, StaticKeyword}
import colang.{SourceCode, TokenStream}

/**
  * Represents a single variable definition site.
  * @param name variable name
  * @param initializer optional initializer expression
  */
case class VariableDefinition(name: Identifier, initializer: Option[Expression]) extends Node {
  lazy val source: SourceCode = {
    initializer match {
      case Some(init) => name.source + init.source
      case None => name.source
    }
  }
}

object VariableDefinition {

  private case class VariableInitializer(assign: Assign, expression: Expression) extends Node {
    def source = assign.source + expression.source
  }

  private object VariableInitializer {
    val strategy = new ParserImpl.Strategy[VariableInitializer] {

      def apply(stream: TokenStream): Result[TokenStream, VariableInitializer] = {
        ParserImpl.parseGroup()
          .definingElement(SingleTokenStrategy(classOf[Assign]))
          .element(Expression.strategy, Terms.Initializer of Terms.Variable)
          .parse(stream)
          .as[Assign, Expression] match {

          case (Present(assign), Present(expression), issues, streamAfterInitializer) =>
            Matched(VariableInitializer(assign, expression), issues, streamAfterInitializer)

          case (Present(assign), Invalid() | Absent(), issues, streamAfterInitializer) =>
            Skipped(issues, streamAfterInitializer)

          case _ => NoMatch()
        }
      }
    }
  }

  val strategy = new ParserImpl.Strategy[VariableDefinition] {

    def apply(stream: TokenStream): Result[TokenStream, VariableDefinition] = {
      ParserImpl.parseGroup()
        .definingElement(identifierStrategy)
        .optionalElement(VariableInitializer.strategy)
        .parse(stream)
        .as[Identifier, VariableInitializer] match {

        case (Present(name), Present(initializer), issues, streamAfterVariable) =>
          Matched(VariableDefinition(name, Some(initializer.expression)), issues, streamAfterVariable)

        case (Present(name), Invalid(), issues, streamAfterVariable) =>
          Matched(VariableDefinition(name, None), issues, streamAfterVariable)

        case (Present(name), Absent(), issues, streamAfterVariable) =>
          Matched(VariableDefinition(name, None), issues, streamAfterVariable)

        case _ => NoMatch()
      }
    }
  }
}

/**
  * Represents a variable(s) definition statement, which may appear in multiple contexts. They are:
  * 1) In the global namespace - for global variables definitions
  * 2) In a type definition - for fields or static variables
  * 3) In a local scope - for local variables definitions
  * @param specifiers variable specifiers list
  * @param type_ variable(s) type
  * @param variables individual variable definitions
  */
case class VariablesDefinition(specifiers: SpecifiersList,
                               type_ : Expression,
                               variables: Seq[VariableDefinition]) extends Statement
                                                                   with GlobalSymbolDefinition
                                                                   with TypeMemberDefinition {
  def source: SourceCode = specifiers.source + variables.last.source
}

object VariablesDefinition {
  val strategy = new ParserImpl.Strategy[VariablesDefinition] {

    private case class VariableDefinitionSequence(variables: ::[VariableDefinition]) extends Node {
      def source = variables.head.source + variables.last.source
    }

    private val varsStrategy = new ParserImpl.Strategy[VariableDefinitionSequence] {
      def apply(stream: TokenStream): Result[TokenStream, VariableDefinitionSequence] = {
        ParserImpl.parseSequence(
          stream = stream,
          elementStrategy = VariableDefinition.strategy,
          elementDescription = Terms.Definition of Terms.Variable,
          mandatorySeparator = Some(classOf[Comma]),
          separatorDescription = Some(Terms.Comma)
        ) match {
          case (variables, issues, streamAfterVariables) if variables.nonEmpty =>
            Matched(VariableDefinitionSequence(variables.toList.asInstanceOf[::[VariableDefinition]]), issues, streamAfterVariables)
          case _ => NoMatch()
        }
      }
    }

    private val specifiersStrategy = new SpecifiersList.Strategy(
      Terms.Definition of Terms.Variables,
      classOf[StaticKeyword])

    def apply(stream: TokenStream): Result[TokenStream, VariablesDefinition] = {
      ParserImpl.parseGroup()
        .optionalElement(specifiersStrategy)
        .definingElement(Type.strategy)
        .lineContinuation()
        .element(varsStrategy, Terms.Definitions of Terms.Variables)
        .parse(stream)
        .as[SpecifiersList, Expression, VariableDefinitionSequence] match {

        case (Present(specifiers), Present(type_), Present(VariableDefinitionSequence(variables)),
              issues, streamAfterVariables) =>

            val node = VariablesDefinition(specifiers, type_, variables)
            Matched(node, issues, streamAfterVariables)

        case _ => NoMatch()
      }
    }
  }
}
