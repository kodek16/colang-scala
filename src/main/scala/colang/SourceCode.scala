package colang

import colang.utils.StringImplicits._

/**
  * Represents a source code fragment.
  * Note that in some cases character indices may not correspond to existing characters.
  * This is to allow pointing issues at non-existent code fragments (like line continuations). Because of this, be
  * careful when using them: prefer using 'text' property, or paddedSubstring() extension method.
  * @param file source file
  * @param startLine fragment start line (0-based inclusive)
  * @param startChar fragment start char (0-based inclusive)
  * @param endLine fragment end line (0-based inclusive)
  * @param endChar fragment end char (0-based inclusive)
  */
case class SourceCode(file: SourceFile,
                      startLine: Int, startChar: Int,
                      endLine: Int, endChar: Int) {

  /**
    * Raw source text
    */
  lazy val text: String = {
    if (startLine == endLine) {
      file.lines(startLine).paddedSubstring(startChar, endChar + 1)
    } else {
      val topLine = file.lines(startLine).paddedSubstring(startChar)
      val middleLines = file.lines.slice(startLine + 1, endLine)
      val bottomLine = file.lines(endLine).paddedSubstring(0, endChar + 1)

      val lines = Seq(topLine) ++ middleLines ++ Seq(bottomLine)
      lines mkString "\n"
    }
  }

  /**
    * Adds two source fragments together, returning the shortest fragment that includes both of them.
    * @param other other code fragment
    * @return source fragment sum
    */
  def +(other: SourceCode): SourceCode = {
    if (file != other.file) throw new IllegalArgumentException("can't combine source code from different files")

    val (newStartLine, newStartChar) = if (startLine < other.startLine ||
                                          (startLine == other.startLine && startChar < other.startChar)) {
      (startLine, startChar)
    } else {
      (other.startLine, other.startChar)
    }

    val (newEndLine, newEndChar) = if (endLine > other.endLine ||
                                      (endLine == other.endLine && endChar > other.endChar)) {
      (endLine, endChar)
    } else {
      (other.endLine, other.endChar)
    }

    SourceCode(file, newStartLine, newStartChar, newEndLine, newEndChar)
  }

  /**
    * Preceeding single character code fragment.
    */
  def before = SourceCode(file, startLine, startChar, startLine, startChar)

  /**
    * Succeeding single character code fragment.
    */
  def after = SourceCode(file, endLine, endChar + 1, endLine, endChar + 1)
}