package colang

import colang.tokens.{Token, Whitespace}

import scala.annotation.tailrec

/**
  * An immutable token stream that is processed by parser.
  * Different implementations are used for different cases, use companion object apply() method for construction.
  * A token stream always ends with a non-whitespace token, but the apply() method accepts any token sequences, and
  * will trim them if necessary.
  */
trait TokenStream {
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty

  /**
    * Reads a token from the stream, returning a new stream without it. Will throw if used on an empty stream.
    * @return (read token, stream after token)
    */
  def read: (Token, TokenStream)

  /**
    * Returns a source code fragment pointing before the next token. Note that this can be used with empty streams
    * to get a fragment pointing to EOF.
    * @return
    */
  def beforeNext: SourceCode

  /**
    * Returns a source code fragment pointing before the next non-whitespace token, or to EOF, if there are none.
    * @return
    */
  def beforeNextNonWhitespace: SourceCode = {
    if (nonEmpty) {
      readNonWhitespace._1.source.before
    } else {
      beforeNext
    }
  }

  /**
    * Reads a token from the stream, skipping all whitespace tokens that came before it. Will throw if used on empty
    * stream (a stream cannot contain only whitespace tokens, so a non-empty stream is guaranteed to have at least one
    * non-whitespace token).
    * @return (read token, stream after token)
    */
  @tailrec
  final def readNonWhitespace: (Token, TokenStream) = {
    val (token, streamAfterToken) = read
    if (token.isInstanceOf[Whitespace]) {
      streamAfterToken.readNonWhitespace
    } else {
      (token, streamAfterToken)
    }
  }

  /**
    * Skips to the first token of one of given types, leaving it in the stream, or to the end of stream, if there are
    * none.
    * @param types token types to stop at
    * @return (Some(source) if at least one token was skipped, stream at the found token)
    */
  def skipUntilOneOf(types: Class[_ <: Token]*): (Option[SourceCode], TokenStream) = {
    @tailrec
    def skipUntil(stream: TokenStream, collectedSource: Option[SourceCode]): (Option[SourceCode], TokenStream) = {
      if (stream.nonEmpty) {
        val (token, newStream) = stream.read
        if (types.contains(token.getClass)) {
          (collectedSource, stream)
        } else {
          val newSource = collectedSource match {
            case Some(source) => source + token.source
            case None => token.source
          }
          skipUntil(newStream, Some(newSource))
        }
      } else {
        (collectedSource, stream)
      }
    }

    skipUntil(this, None)
  }

  /**
    * Skips to the first non-whitespace token on the next line, leaving it in the stream, or to the end of stream
    * if there are none.
    * @return (Some(source) if at least one token was skipped, stream at the found token)
    */
  def skipLine: (Option[SourceCode], TokenStream) = {
    @tailrec
    def skipUntilNonWhitespace(stream: TokenStream, collectedSource: Option[SourceCode]): (Option[SourceCode], TokenStream) = {
      if (stream.nonEmpty) {
        val (token, newStream) = stream.read
        token match {
          case Whitespace(_, _) => skipUntilNonWhitespace(newStream, collectedSource)
          case _ => (collectedSource, stream)
        }
      } else {
        (collectedSource, stream)
      }
    }

    @tailrec
    def skipOnThisLine(stream: TokenStream, collectedSource: Option[SourceCode]): (Option[SourceCode], TokenStream) = {
      if (stream.nonEmpty) {
        val (token, newStream) = stream.read
        token match {
          case Whitespace(hasLineBreaks, _) if hasLineBreaks => skipUntilNonWhitespace(newStream, collectedSource)
          case _ =>
            val newSource = collectedSource match {
              case Some(source) => source + token.source
              case None => token.source
            }
            skipOnThisLine(newStream, Some(newSource))
        }
      } else {
        (collectedSource, stream)
      }
    }

    skipOnThisLine(this, None)
  }

  /**
    * Skips to the end of the stream.
    * @return (Some(source) if at least one token was skipped, stream at its end)
    */
  def skipAll: (Option[SourceCode], TokenStream) = {
    @tailrec
    def skip(stream: TokenStream, collectedSource: Option[SourceCode]): (Option[SourceCode], TokenStream) = {
      if (stream.nonEmpty) {
        val (token, newStream) = stream.read
        val newSource = collectedSource match {
          case Some(source) => source + token.source
          case None => token.source
        }

        skip(newStream, Some(newSource))
      } else {
        (collectedSource, stream)
      }
    }

    skip(this, None)
  }
}

object TokenStream {

  /**
    * Constructs a TokenStream from a non-empty list of tokens.
    * @param source source tokens
    * @return token stream implementation object
    */
  def apply(source: List[Token]): TokenStream = {
    if (source.exists(!_.isInstanceOf[Whitespace])) {
      new NonEmptyTokenStream(source)
    } else if (source.nonEmpty) {
      new EmptyTokenStream(source.last.source.after)
    } else throw new IllegalArgumentException("can't construct a token stream from an empty token sequence")
  }
}

/**
  * Represents a non-empty token stream ending with a non-whitespace token.
  */
class NonEmptyTokenStream(source: List[Token]) extends TokenStream {
  private val filteredSource = source take (source.lastIndexWhere(!_.isInstanceOf[Whitespace]) + 1) match {
    case nonEmpty: ::[Token] => nonEmpty
    case Nil => throw new IllegalArgumentException("can't construct NonEmptyTokenStream from whitespace-only token list")
  }

  def isEmpty = false

  def read: (Token, TokenStream) = {
    filteredSource match {
      case first :: second :: rest => (first, new NonEmptyTokenStream(second :: rest))
      case only :: Nil             => (only,  new EmptyTokenStream(only.source.after))
    }
  }

  def beforeNext = filteredSource.head.source.before
}

/**
  * Represents a "window" onto another token stream, limiting it to tokens surrounded by some kind of delimiters
  * (parentheses, braces, etc.). The stream behaves the same as a usual NonEmptyTokenStream, except when the closing
  * token was reached (the stream doesn't include it). The stream then reports that it's empty. The special breakOut()
  * method allows the client code to drop restrictions, returning to the original stream.
  * Limited streams can be nested.
  * This approach is used only in ParserImpl.parseEnclosedSequence since it allows to use greedy approach when parsing
  * the sequence.
  * @param underlyingStream source token stream
  * @param openingToken opening delimiter type
  * @param closingToken closing delimiter type
  * @param level how many closing tokens do we have to encounter before the end of the "window"
  */
class LimitedTokenStream[Open <: Token, Close <: Token](underlyingStream: TokenStream,
                                                        openingToken: Class[Open],
                                                        closingToken: Class[Close],
                                                        level: Int) extends TokenStream {
  lazy val isEmpty: Boolean = {
    lazy val limitReached = level == 1 && underlyingStream.readNonWhitespace._1.getClass == closingToken
    underlyingStream.isEmpty || limitReached
  }

  def read: (Token, TokenStream) = {
    if (nonEmpty) {
      val (token, newUnderlyingStream) = underlyingStream.read

      val levelChange = token.getClass match {
        case `openingToken` => 1
        case `closingToken` => -1
        case _ => 0
      }

      val newStream = new LimitedTokenStream(newUnderlyingStream, openingToken, closingToken, level + levelChange)
      (token, newStream)
    } else throw new UnsupportedOperationException("can't read from empty or limited stream")
  }

  def beforeNext = underlyingStream.beforeNext

  def breakOut: TokenStream = underlyingStream
}

/**
  * Represents an empty token stream.
  */
class EmptyTokenStream(val beforeNext: SourceCode) extends TokenStream {
  val isEmpty = true
  def read = throw new UnsupportedOperationException("can't read from empty token stream")
}
