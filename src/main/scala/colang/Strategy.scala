package colang

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.issues.Issue

import scala.annotation.tailrec

/**
  * Represents an "extraction strategy". This a common concept of a routine
  * that tries to extract some objects from a raw object stream.
  *
  * Two examples are lexer strategies that extract tokens (terminal nodes)
  * from a raw source code stream, and parser strategies that extract
  * syntactic subtrees from a stream of tokens.
  *
  * Strategies work with immutable streams: see the Strategy.Result types
  * for explanation.
  *
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
    * Represents an "execution result" of a strategy.
    */
  sealed trait Result[Stream, +T]

  object Result {

    /**
      * An object was successfully extracted. This doesn't necessarily mean
      * the object was valid: if any issues were encountered, they are saved in
      * the corresponding field.
      *
      * It is recommended to use this result whenever it's possible to recover
      * at least some data about the object, even if it's only its type.
      *
      * @param result extracted object
      * @param issues encountered issues
      * @param newStream object stream without the extracted object
      */
    case class Success[Stream, T](result: T,
                                  issues: Seq[Issue],
                                  newStream: Stream) extends Result[Stream, T]

    /**
      * Some kind of object was extracted, but it makes no sense to pass it
      * outside the strategy: either because the object is meant to be discarded
      * (e.g. comments), or because it couldn't be recovered to any type that
      * makes sense in the current context. It it's the latter, issues field
      * should contain an issue that describes what is wrong.
      *
      * @param issues encountered issues
      * @param newStream object stream without the extracted object
      */
    case class Malformed[Stream, T](issues: Seq[Issue],
                                    newStream: Stream) extends Result[Stream, T]

    /**
      * The strategy failed to recognize raw stream contents as an object.
      */
    case class NoMatch[Stream, T]() extends Result[Stream, T]
  }
}

/**
  * A shorthand for transforming all objects extracted by a strategy
  * using some transformation function.
  *
  * @param from underlying strategy
  * @param mapper transformation function
  * @tparam Stream raw immutable object stream type
  * @tparam From underlying object type
  * @tparam To transformed object type
  */
class MappedStrategy[Stream, From, To](from: Strategy[Stream, From],
                                       mapper: From => To) extends Strategy[Stream, To] {

  def apply(stream: Stream): Result[Stream, To] = {
    from(stream) match {
      case Success(node, issues, newStream) => Success(mapper(node), issues, newStream)
      case Malformed(issues, newStream) => Malformed(issues, newStream)
      case NoMatch() => NoMatch()
    }
  }
}

object MappedStrategy {
  def apply[Stream, From, To](from: Strategy[Stream, From],
                              mapper: From => To) = new MappedStrategy(from, mapper)
}

/**
  * A strategy aggregator that tries to apply multiple strategies one after
  * another and returns whatever the first match is.
  *
  * Note that strategy order is crucial: if a strategy that returns Skipped
  * is put before a strategy that returns Matched, the union returns Skipped,
  * because the object was successfully extracted.
  *
  * @param strategies aggregated strategies
  * @tparam Stream raw immutable object stream type
  * @tparam T extracted object type
  */
class StrategyUnion[Stream, T](strategies: Seq[Strategy[Stream, T]]) extends Strategy[Stream, T] {

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
  def apply[Stream, T](strategies: Strategy[Stream, T]*) = new StrategyUnion(strategies)
}
