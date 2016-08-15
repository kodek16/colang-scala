package colang

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}

import scala.annotation.tailrec

/**
  * Represents an "extraction strategy". This a common idiom for a routine that tries to extract some objects from
  * a raw object stream. Two examples are lexer strategies that extract tokens (terminal nodes) from a raw source code
  * stream, and parser strategies that extract syntactic subtrees from a stream of tokens.
  * Strategies work with immutable streams: see the Strategy.Result types for explanation.
  * @tparam Stream raw immutable object stream type
  * @tparam T extracted object type
  */
trait Strategy[Stream, +T] {

  /**
    * Tries to extract an object from the stream.
    * @param stream stream to extract from
    * @return extraction result
    */
  def apply(stream: Stream): Strategy.Result[Stream, T]
}

object Strategy {

  /**
    * Represents an "execution result" of a strategy. There are three possible result classes:
    * - object was successfully extracted (Success)
    * - object was discovered, but it was in a state beyond recovery (Malformed)
    * - object wasn't found (NoMatch)
    */
  sealed trait Result[Stream, +T]

  object Result {

    /**
      * Object was successfully extracted. This doesn't mean absolutely no issues were found: some kind of recovery may
      * have been performed, or some warnings may have been emitted by the strategy.
      * @param result extracted object
      * @param issues encountered issues
      * @param newStream object stream without the extracted object
      */
    case class Success[Stream, T](result: T, issues: Seq[Issue], newStream: Stream) extends Result[Stream, T]

    /**
      * Some defining parts of the object were extracted, but there was sensible way to recover the object itself.
      * The main purpose of separating this result class from NoMatch is the possibility of individual error diagnostics
      * for each strategy, providing better overall results.
      * @param issues issues related to the malformed object
      * @param newStream object stream without the malformed object
      */
    case class Malformed[Stream, T](issues: Seq[Issue], newStream: Stream) extends Result[Stream, T]

    /**
      * The strategy failed to recognize raw stream contents to be an object.
      */
    case class NoMatch[Stream, T]() extends Result[Stream, T]
  }
}

/**
  * A shorthand for transforming all objects extracted by a strategy using some transformation function.
  * @param from underlying strategy
  * @param mapper transformation function
  * @tparam Stream raw immutable object stream type
  * @tparam From underlying object type
  * @tparam To transformed object type
  */
class MappedStrategy[Stream, From, To](from: Strategy[Stream, From], mapper: From => To) extends Strategy[Stream, To] {
  def apply(stream: Stream): Result[Stream, To] = {
    from(stream) match {
      case Success(node, issues, newStream) => Success(mapper(node), issues, newStream)
      case Malformed(issues, newStream) => Malformed(issues, newStream)
      case NoMatch() => NoMatch()
    }
  }
}

object MappedStrategy {
  def apply[Stream, From, To](from: Strategy[Stream, From], mapper: From => To) = new MappedStrategy(from, mapper)
}

/**
  * A strategy aggregator that tries to apply multiple strategies one after another and returns whatever the first match
  * is. Note that strategy order is crucial: is a strategy that would return Malformed was put before a strategy that
  * would return Success, the union will return Malformed, because it's considered a match.
  * @param strategies alternative strategies
  * @tparam Stream raw immutable object stream type
  * @tparam T extracted object type
  */
class StrategyUnion[Stream, T](strategies: Strategy[Stream, T]*) extends Strategy[Stream, T] {
  def apply(stream: Stream): Result[Stream, T] = {

    @tailrec
    def applyStrategies(strategies: Seq[Strategy[Stream, T]]): Result[Stream, T] = {
      strategies match {
        case strategy +: tail =>
          strategy(stream) match {
            case result @ Success(_, _, _) => result
            case result @ Malformed(_, _)  => result
            case _ => applyStrategies(tail)
          }
        case _ => NoMatch()
      }
    }

    applyStrategies(strategies)
  }
}

object StrategyUnion {
  def apply[Stream, T](strategies: Strategy[Stream, T]*) = new StrategyUnion(strategies :_*)
}