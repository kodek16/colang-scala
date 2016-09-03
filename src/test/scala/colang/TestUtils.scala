package colang

import colang.Strategy.Result
import colang.Strategy.Result.{Malformed, NoMatch, Success}
import colang.issues.{Issue, Warning}

@Deprecated
object TestUtils {

  class InlineSourceFile(val name: String,
                         val text: String) extends SourceFile

  val emptySourceFile = new InlineSourceFile("empty.co", "")

  def makeWarning(message: String) = Warning("", SourceCode(emptySourceFile, 0, 0, 0, 0), message, Seq.empty)

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
