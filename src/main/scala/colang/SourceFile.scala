package colang

import java.io.File

import scala.io.Source

/**
  * Represents a CO source file.
  */
trait SourceFile {
  /**
    * The name as it will appear in compiler messages.
    */
  val name: String

  /**
    * Raw source text.
    */
  val text: String

  /**
    * Raw source text split into lines.
    */
  lazy val lines = text.split('\n')

  /**
    * Source code fragment pointing to EOF.
    */
  lazy val eof: SourceCode = {
    val lastNonEmptyLineIndex = lines lastIndexWhere { _.trim.nonEmpty }
    val lineNo = if (lastNonEmptyLineIndex != -1) lastNonEmptyLineIndex else 0
    val charNo = (lines(lineNo) lastIndexWhere { !_.isWhitespace  }) + 1
    SourceCode(this, lineNo, charNo, lineNo, charNo)
  }
}

/**
  * Represent a CO source file corresponding to actual existing file.
  * @param file java.io.File object
  */
class RealSourceFile(file: File) extends SourceFile {
  val name = file.getPath
  val text = Source.fromFile(file).getLines().mkString("\n")
}
