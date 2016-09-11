package colang

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.issues.Issue
import org.scalatest._

abstract class UnitSpec extends FunSpec with Matchers with OptionValues with Inside with Inspectors {

  def makeSuccessfulStrategy[Stream, T](result: T, issues: Seq[Issue], newStream: Stream) = new Strategy[Stream, T] {
    def apply(stream: Stream): Result[Stream, T] = Success(result, issues, newStream)
  }

  def makeMalformedStrategy[Stream, T](issues: Seq[Issue], newStream: Stream) = new Strategy[Stream, T] {
    def apply(stream: Stream): Result[Stream, T] = Malformed(issues, newStream)
  }

  def makeNonMatchingStrategy[Stream, T] = new Strategy[Stream, T] {
    def apply(stream: Stream) = NoMatch()
  }
}
