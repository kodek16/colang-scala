package colang.ast.raw.statement

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.ast.raw.ParserImpl._
import colang.ast.raw._
import colang.ast.raw.expression.Expression
import colang.issues.Terms
import colang.tokens.{Assign, Comma, Identifier}
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
        ParserImpl.parseGroup(Terms.Initializer of Terms.Variable)
          .definingElement(SingleTokenStrategy(classOf[Assign]))
          .element(Expression.strategy, Terms.Expression)
          .parse(stream)
          .as[Assign, Expression] match {

          case (Present(assign), Present(expression), issues, streamAfterInitializer) =>
            Success(VariableInitializer(assign, expression), issues, streamAfterInitializer)

          case (Present(assign), Invalid() | Absent(), issues, streamAfterInitializer) =>
            Malformed(issues, streamAfterInitializer)

          case _ => NoMatch()
        }
      }
    }
  }

  val strategy = new ParserImpl.Strategy[VariableDefinition] {

    def apply(stream: TokenStream): Result[TokenStream, VariableDefinition] = {
      ParserImpl.parseGroup(Terms.Definition of Terms.Variable)
        .definingElement(identifierStrategy)
        .optionalElement(VariableInitializer.strategy)
        .parse(stream)
        .as[Identifier, VariableInitializer] match {

        case (Present(name), Present(initializer), issues, streamAfterVariable) =>
          Success(VariableDefinition(name, Some(initializer.expression)), issues, streamAfterVariable)

        case (Present(name), Invalid(), issues, streamAfterVariable) =>
          Success(VariableDefinition(name, None), issues, streamAfterVariable)

        case (Present(name), Absent(), issues, streamAfterVariable) =>
          Success(VariableDefinition(name, None), issues, streamAfterVariable)

        case _ => NoMatch()
      }
    }
  }
}

/**
  * Represents a variable(s) definition statement, which may also appear at the top-level.
  * @param type_ variable(s) type
  * @param variables individual variable definitions
  */
case class VariablesDefinition(type_ : Type, variables: Seq[VariableDefinition]) extends Statement
                                                                                 with GlobalSymbolDefinition {
  def source: SourceCode = type_.source + variables.last.source
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
          sequenceDescription = Terms.Definitions of Terms.Variables,
          elementStrategy = VariableDefinition.strategy,
          elementDescription = Terms.Definition of Terms.Variable,
          mandatorySeparator = Some(classOf[Comma]),
          separatorDescription = Some(Terms.Comma)
        ) match {
          case (variables, issues, streamAfterVariables) if variables.nonEmpty =>
            Success(VariableDefinitionSequence(variables.toList.asInstanceOf[::[VariableDefinition]]), issues, streamAfterVariables)
          case _ => NoMatch()
        }
      }
    }

    def apply(stream: TokenStream): Result[TokenStream, VariablesDefinition] = {
      ParserImpl.parseGroup(Terms.Definition of Terms.Variables)
        .definingElement(Type.strategy)
        .element(varsStrategy, Terms.Definitions of Terms.Variables)
        .parse(stream)
        .as[Type, VariableDefinitionSequence] match {

        case (Present(type_), Present(VariableDefinitionSequence(variables)), issues, streamAfterVariables) =>
            Success(VariablesDefinition(type_, variables), issues, streamAfterVariables)
        case _ => NoMatch()
      }
    }
  }
}