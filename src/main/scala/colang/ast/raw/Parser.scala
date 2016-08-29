package colang.ast.raw

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang._
import colang.issues.{Error, Issue}
import colang.tokens._

import scala.annotation.tailrec

/**
  * Represents a compiler components that groups tokens into higher-level syntax tree nodes representing relationships
  * between them.
  */
trait Parser {

  /**
    * Constructs an abstract syntax tree (AST) from a sequence of terminal nodes (tokens).
    * @param tokens terminal nodes
    * @return (root AST node, encountered issues)
    */
  def parse(tokens: Seq[Token]): (TranslationUnit, Seq[Issue])
}

/**
  * Actual parser implementation.
  */
class ParserImpl extends Parser {

  def parse(tokens: Seq[Token]): (TranslationUnit, Seq[Issue]) = {
    val tokenStream = TokenStream(tokens.toList)

    TranslationUnit.strategy(tokenStream) match {
      case Success(translationUnit, issues, _) => (translationUnit, issues)
    }
  }
}

object ParserImpl {

  type Strategy[N <: Node] = colang.Strategy[TokenStream, N]

  /**
    * A strategy template for parsing tokens (terminal nodes) as-is, skipping whitespace before them.
    * @param tokenType token type
    */
  class SingleTokenStrategy[T <: Token](tokenType: Class[T]) extends Strategy[T] {
    def apply(stream: TokenStream): Result[TokenStream, T] = {
      if (stream.nonEmpty) {
        val (token, streamAfterToken) = stream.readNonWhitespace

        if (token.getClass == tokenType) {
          Success(token.asInstanceOf[T], Seq.empty, streamAfterToken)
        } else NoMatch()
      } else NoMatch()
    }
  }

  object SingleTokenStrategy {
    def apply[T <: Token](tokenType: Class[T]) = new SingleTokenStrategy(tokenType)
  }

  /**
    * A specialized SingleTokenStrategy for parsing identifiers. Produces better error messages when keywords are used
    * instead of identifiers.
    */
  val identifierStrategy = new Strategy[Identifier] {
    def apply(stream: TokenStream): Result[TokenStream, Identifier] = {
      if (stream.nonEmpty) {
        val (token, newStream) = stream.readNonWhitespace

        token match {
          case id: Identifier => Success(id, Seq.empty, newStream)
          case _: Keyword =>
            val issue = Error(token.source, s"${token.source.text} is a keyword, so it can't be used as an identifier")
            Success(Identifier(token.source.text, token.source), Seq(issue), newStream)
          case _ => NoMatch()
        }
      } else NoMatch()
    }
  }

  /**
    * A generic method for parsing sequences of nodes of the same type, possibly separated by a mandatory separator.
    * @param stream source token stream
    * @param elementStrategy strategy for parsing a single element of the sequence
    * @param elementDescription string describing a single element of the sequence, used in error messages
    * @param mandatorySeparator specify Some(Class[Separator]) if sequence elements must be separated by some token
    * @param separatorDescription string describing the separator, if one was specified
    * @param greedy the default behavior (when 'greedy' is false) is to treat unknown tokens as the end of the sequence,
    *               leaving them in the stream. If this parameter is set to true, this function will read the whole
    *               stream (which is not always the whole file, see LimitedTokenStream), treating unknown tokens as
    *               errors.
    * @param recoveryStopHints additional stop hints passed to recover() function
    * @tparam N sequence element type
    * @tparam Separator element separator type
    * @return (sequence elements, encountered issues, stream after the sequence)
    */
  def parseSequence[N <: Node, Separator <: Token](stream: TokenStream,
                                                   elementStrategy: Strategy[N],
                                                   elementDescription: String,
                                                   mandatorySeparator: Option[Class[Separator]] = None,
                                                   separatorDescription: String = "",
                                                   greedy: Boolean = false,
                                                   recoveryStopHints: Seq[Class[_ <: Token]] = Seq.empty)
  : (Seq[N], Seq[Issue], TokenStream) = {

    @tailrec
    def parseWithoutSeparator(stream: TokenStream,
                              collectedNodes: Vector[N] = Vector.empty,
                              collectedIssues: Vector[Issue] = Vector.empty): (Vector[N], Vector[Issue], TokenStream) = {

      elementStrategy(stream) match {
        case Success(node, issues, newStream) =>
          parseWithoutSeparator(newStream, collectedNodes :+ node, collectedIssues ++ issues)
        case Malformed(issues, newStream) =>
          parseWithoutSeparator(newStream, collectedNodes, collectedIssues ++ issues)
        case NoMatch() =>
          if (greedy && stream.nonEmpty) {
            val (invalidSource, newStream) = recover(stream, stopHints = recoveryStopHints)
            val issue = Error(invalidSource, "tokens don't form a valid " + elementDescription)
            parseWithoutSeparator(newStream, collectedNodes, collectedIssues :+ issue)
          } else {
            (collectedNodes, collectedIssues, stream)
          }
      }
    }

    @tailrec
    def parseWithSeparator(stream: TokenStream,
                           separatorType: Class[Separator],
                           collectedNodes: Vector[N] = Vector.empty,
                           collectedIssues: Vector[Issue] = Vector.empty): (Vector[N], Vector[Issue], TokenStream) = {

      val separatorStrategy = SingleTokenStrategy(separatorType)

      val (newNodes, nodeIssues, newStream) = elementStrategy(stream) match {
        case Success(node, issues, newStream_) => (Seq(node), issues, newStream_)
        case Malformed(issues, newStream_) => (Seq.empty, issues, newStream_)
        case NoMatch() =>
          if (greedy && stream.nonEmpty) {
            val (invalidSource, newStream) = recover(stream, stopHints = recoveryStopHints :+ mandatorySeparator.get)
            val issue = Error(invalidSource, "tokens don't form a valid " + elementDescription)
            (Seq.empty, Seq(issue), newStream)
          } else {
            (Seq.empty, Seq.empty, stream)
          }
      }

      separatorStrategy(newStream) match {
        case Success(separator, sepIssues, nextStream) =>
          parseWithSeparator(nextStream, separatorType, collectedNodes ++ newNodes, collectedIssues ++ nodeIssues ++ sepIssues)
        case Malformed(sepIssues, nextStream) =>
          //Same as above, Scala does't allow such alternatives in pattern matching.
          parseWithSeparator(nextStream, separatorType, collectedNodes ++ newNodes, collectedIssues ++ nodeIssues ++ sepIssues)
        case NoMatch() =>
          if (greedy && newStream.nonEmpty) {
            val issue = Error(newStream.beforeNext, "expected a separating " + separatorDescription)
            parseWithSeparator(newStream, separatorType, collectedNodes ++ newNodes, collectedIssues ++ nodeIssues :+ issue)
          } else {
            (collectedNodes ++ newNodes, collectedIssues ++ nodeIssues, newStream)
          }
      }
    }

    mandatorySeparator match {
      case Some(separatorType) => parseWithSeparator(stream, separatorType)
      case None => parseWithoutSeparator(stream)
    }
  }

  /**
    * A generic method for parsing sequences of nodes of the same type enclosed in some kind of delimiting tokens
    * (parentheses, braces, etc.), possibly separated by a mandatory separator.
    * @param stream source token stream
    * @param sequenceDescription string describing the sequence as a whole, used in error messages
    * @param elementStrategy strategy for parsing a single element of the sequence
    * @param elementDescription string describing a single element of the sequence, used in error messages
    * @param openingElement opening token type
    * @param openingElementDescription string describing the opening token, used in error messages
    * @param closingElement closing token type
    * @param closingElementDescription string describing the closing token, used in error messages
    * @param mandatorySeparator specify Some(Class[Separator]) if sequence elements must be separated by some token
    * @param separatorDescription string describing the separator, if one was specified
    * @param recoveryStopHints additional stop hints passed to recover() function
    * @tparam N sequence element type
    * @tparam Open opening token type
    * @tparam Close closing token type
    * @tparam Separator element separator type
    * @return if opening token was found, Some(opening token, sequence elements, closing token (if it was found),
    *         encountered issues, stream after the sequence). If it wasn't, None.
    */
  def parseEnclosedSequence[N <: Node, Open <: Token, Close <: Token, Separator <: Token](stream: TokenStream,
                                                                                          sequenceDescription: String,
                                                                                          elementStrategy: Strategy[N],
                                                                                          elementDescription: String,
                                                                                          openingElement: Class[Open],
                                                                                          openingElementDescription: String,
                                                                                          closingElement: Class[Close],
                                                                                          closingElementDescription: String,
                                                                                          mandatorySeparator: Option[Class[Separator]] = None,
                                                                                          separatorDescription: String = "",
                                                                                          recoveryStopHints: Seq[Class[_ <: Token]] = Seq.empty)
  : Option[(Open, Seq[N], Option[Close], Seq[Issue], TokenStream)] = {
    val openingElementStrategy = SingleTokenStrategy(openingElement)
    val closingElementStrategy = SingleTokenStrategy(closingElement)

    openingElementStrategy(stream) match {
      case Success(open, openIssues, streamAfterOpen) =>
        val limitedStream = new LimitedTokenStream(streamAfterOpen, openingElement, closingElement, 1)

        val (elements, elementIssues, limitedStreamOnEnd) =
          parseSequence(limitedStream, elementStrategy, elementDescription, mandatorySeparator, separatorDescription,
            greedy = true, recoveryStopHints = recoveryStopHints)

        val streamOnClose = limitedStreamOnEnd.asInstanceOf[LimitedTokenStream[Open, Close]].breakOut

        val (close, closeIssues, streamAfterClose) = closingElementStrategy(streamOnClose) match {
          case Success(c, ci, s) => (Some(c), ci, s)
          case Malformed(ci, s)  => (None,    ci, s)
          case NoMatch() =>
            val position = if (elements.nonEmpty) elements.last.source.after else open.source.after
            val issue = Error(position, s"expected a $closingElementDescription after $sequenceDescription")
            (None, Seq(issue), streamOnClose)
        }

        Some(open, elements, close, openIssues ++ elementIssues ++ closeIssues, streamAfterClose)

      case Malformed(_, _) | NoMatch() => None
    }
  }

  /**
    * Skips a number of tokens in an attempt to recover the stream to a fresh state. This method is used by greedy
    * sequence parsers when the element strategy can't match the tokens in the stream. It tries a few common strategies:
    * skipping to the closing brace when the opening couldn't be parsed, skipping to the next delimiter token (';' by
    * default, others can be specified in 'stopHints' parameter), and skipping to the next line. If none of these was
    * possible, all the tokens are discarded, skipping to the end of the stream.
    * @param stream source token stream to recover (can't be empty)
    * @param stopHints additional 'delimiter' tokens used as recovery stop hints
    * @return (skipped source code, stream after recovery)
    */
  private def recover(stream: TokenStream,
                      stopHints: Seq[Class[_ <: Token]] = Seq.empty): (SourceCode, TokenStream) = {

    //1: Code block recovery
    def recoverCodeBlock: Option[(SourceCode, TokenStream)] = {

      @tailrec
      def readBlock(stream: TokenStream, level: Int, collectedSource: SourceCode): (SourceCode, TokenStream) = {
        if (level > 0) {
          val (_, streamOnBrace) = stream.skipUntilOneOf(classOf[LeftBrace], classOf[RightBrace])

          if (streamOnBrace.nonEmpty) {
            val (brace, streamAfterBrace) = streamOnBrace.read
            brace match {
              case LeftBrace(source)  => readBlock(streamAfterBrace, level + 1, collectedSource + source)
              case RightBrace(source) => readBlock(streamAfterBrace, level - 1, collectedSource + source)
            }
          } else {
            (collectedSource + streamOnBrace.beforeNext, streamOnBrace)
          }
        } else {
          (collectedSource, stream)
        }
      }

      if (stream.nonEmpty) {
        val (nextToken, restStream) = stream.readNonWhitespace
        nextToken match {
          case LeftBrace(source) => Some(readBlock(restStream, 1, source))
          case _ => None
        }
      } else None
    }

    //2: Same line recovery
    def recoverSameLine: Option[(SourceCode, TokenStream)] = {
      val (droppedSourceOption, streamOnHint) = stream.skipUntilOneOf(stopHints :+ classOf[LeftBrace] :_*)
      if (streamOnHint.nonEmpty) {
        droppedSourceOption match {
          case Some(droppedSource) =>
            if (droppedSource.startLine == droppedSource.endLine) {
              Some((droppedSource, streamOnHint))
            } else None
          case None => None
        }
      } else None
    }

    //3: Next line recovery
    def recoverNextLine: Option[(SourceCode, TokenStream)] = {
      val (droppedSourceOption, streamOnNextToken) = stream.skipLine
      if (streamOnNextToken.nonEmpty) {
        droppedSourceOption match {
          case Some(droppedSource) => Some(droppedSource, streamOnNextToken)
          case None => Some(recover(streamOnNextToken, stopHints))
        }
      } else None
    }

    recoverCodeBlock orElse recoverSameLine orElse recoverNextLine getOrElse {
      //Couldn't do anything, discard rest of stream.
      if (stream.nonEmpty) {
        val (invalidSourceOption, emptyStream) = stream.skipAll
        (invalidSourceOption.get, emptyStream)
      } else throw new IllegalArgumentException("can't perform recovery on empty stream")
    }
  }

  /**
    * Provides a builder interface for parsing groups of nodes, possibly of different types. This could be implemented
    * in a much better way if Scala had variadic templates, but we have to work with what we have.
    * See the GroupParseBuilder and GroupParseResult for detailed explanation and every second non-trivial node class
    * for usage examples.
    * @return a GroupParseBuilder object
    */
  def parseGroup() = new GroupParseBuilder(Vector.empty)

  private case class GroupElement(strategy: Strategy[Node],
                                  description: String,
                                  stopIfAbsent: Boolean,
                                  optional: Boolean)

  class GroupParseBuilder private[ParserImpl] (elements: Vector[GroupElement]) {

    /**
      * Adds a new node parsing strategy to the end of the group.
      * @param strategy strategy for parsing the node
      * @param description string describing the node, used in error messages
      * @param stopIfAbsent abort the parsing if this strategy didn't match. The default behavior is to try to parse
      *                     every node in the group, emitting error messages for missing ones, but this makes it
      *                     impossible to create recursive node strategies.
      * @param optional if true, an error message won't be emitted even if this strategy didn't match
      * @return a GroupParseBuilder object
      */
    def element(strategy: Strategy[Node],
                description: String,
                stopIfAbsent: Boolean = false,
                optional: Boolean = false) = {
      new GroupParseBuilder(elements :+ GroupElement(strategy, description, stopIfAbsent, optional))
    }

    /**
      * Actually performs the parsing.
      * @param stream source token stream
      * @return a GroupParseResult object
      */
    def parse(stream: TokenStream): GroupParseResult = {
      @tailrec
      def doIt(elements: Vector[GroupElement],
               stream: TokenStream,
               collectedNodes: Vector[NodeOption[_]] = Vector.empty,
               collectedIssues: Vector[Issue] = Vector.empty): (Vector[NodeOption[_]], Vector[Issue], TokenStream) = {
        elements match {
          case element +: tail =>
            element.strategy(stream) match {
              case Success(node, issues_, newStream_) =>
                doIt(tail, newStream_, collectedNodes :+ Present(node), collectedIssues ++ issues_)
              case Malformed(issues_, newStream_) =>
                doIt(tail, newStream_, collectedNodes :+ Invalid(), collectedIssues ++ issues_)
              case NoMatch() if !element.stopIfAbsent =>
                if (element.optional) {
                  doIt(tail, stream, collectedNodes :+ Absent(), collectedIssues)
                } else {
                  val issue = Error(stream.beforeNext, "missing " + element.description)
                  doIt(tail, stream, collectedNodes :+ Absent(), collectedIssues :+ issue)
                }
              case _ =>
                val nones = Vector.fill(elements.size)(Absent())
                (collectedNodes ++ nones, collectedIssues, stream)
            }
          case _ =>
            (collectedNodes, collectedIssues, stream)
        }
      }

      val (nodes, issues, newStream) = doIt(elements, stream)
      new GroupParseResult(nodes, issues, newStream)
    }
  }

  /**
    * Represents a possibly present and valid node. Unlike Strategy.Result, subclasses don't provide encountered issues
    * and the new stream. This trait is only used as a return value from group parsing.
    * @tparam N node type
    */
  sealed trait NodeOption[+N <: Node] {
    def toOption: Option[N] = {
      this match {
        case Present(node) => Some(node)
        case _ => None
      }
    }
  }

  /**
    * Represents a successfully matched node.
    */
  case class Present[N <: Node](node: N) extends NodeOption[N]

  /**
    * Represents a malformed but present node.
    */
  case class Invalid[N <: Node]() extends NodeOption[N]

  /**
    * Represents an absent node.
    */
  case class Absent[N <: Node]() extends NodeOption[N]

  /**
    * A totally unsafe class that exists because variadic templates don't. Use 'as' method with explicitly specified
    * node types to extract NodeOptions for individual nodes, encountered issues and the stream after the group.
    */
  class GroupParseResult private [ParserImpl] (nodes: Seq[NodeOption[_]], issues: Vector[Issue], stream: TokenStream) {
    def as[N1 <: Node] = nodes match {
      case e1 +: _ =>
        (e1.asInstanceOf[NodeOption[N1]], issues, stream)
    }

    def as[N1 <: Node, N2 <: Node] = nodes match {
      case e1 +: e2 +: _ =>
        (e1.asInstanceOf[NodeOption[N1]], e2.asInstanceOf[NodeOption[N2]], issues, stream)
    }

    def as[N1 <: Node, N2 <: Node, N3 <: Node] = nodes match {
      case e1 +: e2 +: e3 +: _ =>
        (e1.asInstanceOf[NodeOption[N1]], e2.asInstanceOf[NodeOption[N2]], e3.asInstanceOf[NodeOption[N3]], issues, stream)
    }

    def as[N1 <: Node, N2 <: Node, N3 <: Node, N4 <: Node] = nodes match {
      case e1 +: e2 +: e3 +: e4 +: _ =>
        (e1.asInstanceOf[NodeOption[N1]], e2.asInstanceOf[NodeOption[N2]], e3.asInstanceOf[NodeOption[N3]],
         e4.asInstanceOf[NodeOption[N4]], issues, stream)
    }

    def as[N1 <: Node, N2 <: Node, N3 <: Node, N4 <: Node, N5 <: Node] = nodes match {
      case e1 +: e2 +: e3 +: e4 +: e5 +: _ =>
        (e1.asInstanceOf[NodeOption[N1]], e2.asInstanceOf[NodeOption[N2]], e3.asInstanceOf[NodeOption[N3]],
         e4.asInstanceOf[NodeOption[N4]], e5.asInstanceOf[NodeOption[N5]], issues, stream)
    }

    def as[N1 <: Node, N2 <: Node, N3 <: Node, N4 <: Node, N5 <: Node, N6 <: Node] = nodes match {
      case e1 +: e2 +: e3 +: e4 +: e5 +: e6 +: _ =>
        (e1.asInstanceOf[NodeOption[N1]], e2.asInstanceOf[NodeOption[N2]], e3.asInstanceOf[NodeOption[N3]],
         e4.asInstanceOf[NodeOption[N4]], e5.asInstanceOf[NodeOption[N5]], e6.asInstanceOf[NodeOption[N6]], issues, stream)
    }
  }
}
