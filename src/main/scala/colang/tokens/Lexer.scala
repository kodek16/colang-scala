package colang.tokens

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang._
import colang.issues.{Issue, Issues}

import scala.annotation.tailrec
import scala.util.matching.Regex

/**
  * Represents a compiler component that splits raw source code into tokens.
  */
trait Lexer {

  /**
    * Splits source file into tokens.
    * @param sourceFile source file
    * @return (tokens, encountered issues)
    */
  def splitIntoTokens(sourceFile: SourceFile): (Seq[Token], Seq[Issue])
}

/**
  * Actual lexer implementation.
  */
class LexerImpl extends Lexer {

  /**
    * The lexer basically operates on a strategy union of all possible token parsing strategies. As the StrategyUnion
    * description states, strategies order matters. Most strategies are implemented in an order-agnostic way (e.g.
    * 'assign' strategy will not match individual characters of 'equals' token), but some are not (keywords), so pay
    * attention to this.
    */
  private val strategies = StrategyUnion(
    LexerImpl.lineCommentStrategy,
    LexerImpl.blockCommentStrategy,

    StructKeyword.strategy,
    NativeKeyword.strategy,
    IfKeyword.strategy,
    ElseKeyword.strategy,
    WhileKeyword.strategy,
    ReturnKeyword.strategy,
    ThisKeyword.strategy,
    ConstructorKeyword.strategy,
    StaticKeyword.strategy,
    BoolLiteral.strategy,
    Identifier.strategy,

    DoubleLiteral.strategy,
    IntLiteral.strategy,
    LexerImpl.unknownNumberStrategy,

    LeftParen.strategy,
    RightParen.strategy,
    LeftBrace.strategy,
    RightBrace.strategy,
    Comma.strategy,
    Dot.strategy,
    Semicolon.strategy,
    Ampersand.strategy,

    LogicalNot.strategy,
    Multiply.strategy,
    Divide.strategy,
    Mod.strategy,
    Pow.strategy,
    Plus.strategy,
    Minus.strategy,
    Less.strategy,
    Greater.strategy,
    LessOrEquals.strategy,
    GreaterOrEquals.strategy,
    Equals.strategy,
    NotEquals.strategy,
    LogicalAnd.strategy,
    LogicalOr.strategy,
    Assign.strategy,

    Whitespace.strategy,

    LexerImpl.unknownTokenStrategy)

  def splitIntoTokens(file: SourceFile): (Seq[Token], Seq[Issue]) = {

    @tailrec
    def extractAllTokens(stream: SourceCodeStream,
                         extractedTokens: Vector[Token] = Vector.empty,
                         extractedIssues: Vector[Issue] = Vector.empty): (Vector[Token], Vector[Issue]) = {
      if (stream.isEmpty) {
        (extractedTokens, extractedIssues)
      } else {
        val (newTokens, newIssues, newStream) = strategies(stream) match {
          case Success(token, tokenIssues, streamAfterToken) =>
            (extractedTokens :+ token, extractedIssues ++ tokenIssues, streamAfterToken)
          case Malformed(tokenIssues, streamAfterToken) =>
            (extractedTokens, extractedIssues ++ tokenIssues, streamAfterToken)
          case NoMatch() => throw new RuntimeException("No matching lexer strategies were found.")
        }

        extractAllTokens(newStream, newTokens, newIssues)
      }
    }

    val stream = new SourceCodeStream(file)

    val (tokens, issues) = extractAllTokens(stream)
    val lastNonWsIndex = tokens lastIndexWhere { !_.isInstanceOf[Whitespace] }
    val filteredTokens = tokens take (lastNonWsIndex + 1)
    (filteredTokens, issues)
  }
}

object LexerImpl {
  type Strategy[T <: Token] = colang.Strategy[SourceCodeStream, T]

  /**
    * A strategy template for parsing "stateless" tokens: those that don't have any associated state except their type.
    * For example, '{' is a stateless LeftBrace token, while '5.6' is a stateful DoubleLiteral token with value '5.6'.
    * @param constructor stateless token constructor
    * @param re regex for token type
    * @tparam T stateless token type
    */
  class StatelessTokenStrategy[T <: Token](val constructor: SourceCode => T, val re: Regex) extends Strategy[T] {
    def apply(stream: SourceCodeStream): Strategy.Result[SourceCodeStream, T] = {
      re findPrefixOf stream match {
        case Some(text) =>
          val (source, newStream) = stream.take(text)
          Success(constructor(source), Seq.empty, newStream)
        case None => NoMatch()
      }
    }
  }

  /**
    * A fallback strategy that matches any non-whitespace characters.
    */
  val unknownTokenStrategy = new Strategy[Token] {
    def apply(stream: SourceCodeStream): Result[SourceCodeStream, Token] = {
      val re = """[^\s]+""".r

      re findPrefixOf stream match {
        case Some(text) =>
          val (source, streamAfterToken) = stream.take(text)
          val issue = Issues.UnknownCharacterSequence(source, ())
          Malformed(Seq(issue), streamAfterToken)
        case None => NoMatch()
      }
    }
  }

  /**
    * A strategy that matches any "word" ('.' is allowed) beginning with a digit. It generates the issue but treats
    * the word as it was '0', not returning Malformed (to improve further analysis).
    */
  val unknownNumberStrategy = new Strategy[IntLiteral] {
    def apply(stream: SourceCodeStream): Result[SourceCodeStream, IntLiteral] = {
      val re = """\d[\w\.]*""".r

      re findPrefixOf stream match {
        case Some(text) =>
          val (source, streamAfterToken) = stream.take(text)
          val issue = Issues.UnknownNumberFormat(source, ())
          Success(IntLiteral(0, source), Seq(issue), streamAfterToken)
        case None => NoMatch()
      }
    }
  }

  /**
    * A single-line C-style '//' comment strategy. Comments aren't returned as tokens, instead the strategy reports
    * Malformed without any issues.
    */
  val lineCommentStrategy = new Strategy[Token] {
    def apply(stream: SourceCodeStream): Result[SourceCodeStream, Token] = {
      val re = """(?m)\/\/.*$""".r
      re findPrefixOf stream match {
        case Some(text) =>
          val (_, streamAfterComment) = stream.take(text)
          Malformed(Seq.empty, streamAfterComment)
        case None => NoMatch()
      }
    }
  }

  /**
    * A multiline C-style comment strategy. Comments aren't returned as tokens, instead the strategy reports
    * Malformed without any issues.
    */
  val blockCommentStrategy = new Strategy[Token] {
    def apply(stream: SourceCodeStream): Result[SourceCodeStream, Token] = {
      val re = """(?s)\/\*(.*?)\*\/""".r
      re findPrefixOf stream match {
        case Some(text) =>
          val (_, streamAfterComment) = stream.take(text)
          Malformed(Seq.empty, streamAfterComment)
        case None => NoMatch()
      }
    }
  }
}
