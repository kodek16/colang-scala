package colang

/**
  * An immutable raw source code stream that is passed to lexer.
  * It extends CharSequence to allow simple regex matching, see tokens._ classes for examples.
  * @param file source file
  * @param startChar absolute position in source file of first character in stream. Doesn't get reset on line break
  * @param lineNo line number of first character in stream (0-based)
  * @param charNo character position in line of first character in stream (0-based)
  */
class SourceCodeStream(val file: SourceFile, val startChar: Int = 0,
                       val lineNo: Int = 0, val charNo: Int = 0) extends CharSequence {

  /**
    * Only added for implementing CharSequence, shouldn't be used directly.
    */
  def charAt(i: Int): Char = file.text(startChar + i)

  /**
    * Only added for implementing CharSequence, shouldn't be used directly.
    */
  def length(): Int = file.text.length - startChar

  /**
    * Only added for implementing CharSequence, shouldn't be used directly.
    */
  def subSequence(i: Int, i1: Int): CharSequence = file.text.substring(startChar + i, startChar + i1)

  def isEmpty = startChar == file.text.length
  def nonEmpty = !isEmpty

  /**
    * Extracts a character sequence from the stream.
    * What this method exactly does is taking prefix.length() characters from the stream, no matter what they are.
    * This signature allows convenient extraction after regex matching, see tokens._ classes for examples.
    * @param prefix text that should be extracted
    * @return (source code fragment, new stream)
    */
  def take(prefix: String): (SourceCode, SourceCodeStream) = {
    val positions = (prefix foldLeft Vector.empty[(Char, Int, Int)]) { (positions, curChar) =>
      positions match {
        case _ :+ previous =>
          val (prevChar, prevLineNo, prevCharNo) = previous
          if (prevChar == '\n') {
            positions :+ (curChar, prevLineNo + 1, 0)
          } else {
            positions :+ (curChar, prevLineNo, prevCharNo + 1)
          }
        case empty => positions :+ (curChar, lineNo, charNo)
      }
    }

    val newStartChar = startChar + prefix.length
    val (newLineNo, newCharNo) = if (positions.last._1 == '\n') {
      (positions.last._2 + 1, 0)
    } else {
      (positions.last._2, positions.last._3 + 1)
    }

    val codeSegment = SourceCode(file, lineNo, charNo, positions.last._2, positions.last._3)
    val newStream = new SourceCodeStream(file, newStartChar, newLineNo, newCharNo)

    (codeSegment, newStream)
  }
}
