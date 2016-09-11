package colang.ast.raw

import colang.Strategy.Result.Success
import colang.ast.raw.statement.VariablesDefinition
import colang.issues.{Adjectives, Terms}
import colang.{SourceCode, StrategyUnion, TokenStream}

/**
  * A marker trait for nodes that can represent top-level definitions in a source file.
  */
trait GlobalSymbolDefinition extends Node

/**
  * Represents a parsed CO source file.
  * @param symbols global symbol definitions
  */
case class TranslationUnit(symbols: Seq[GlobalSymbolDefinition]) extends Node {
  def source: SourceCode = symbols.head.source + symbols.last.source
}

object TranslationUnit {
  val strategy = new ParserImpl.Strategy[TranslationUnit] {

    private val globalSymbolStrategy = StrategyUnion(
      TypeDefinition.strategy,
      FunctionDefinition.strategy,
      VariablesDefinition.strategy)

    def apply(stream: TokenStream): Success[TokenStream, TranslationUnit] = {
      ParserImpl.parseSequence(
        stream = stream,
        elementStrategy = globalSymbolStrategy,
        elementDescription = Terms.Definition of (Adjectives.Global applyTo Terms.Symbol),
        greedy = true
      ) match {
        case (symbols, issues, newStream) => Success(TranslationUnit(symbols), issues, newStream)
      }
    }
  }
}
