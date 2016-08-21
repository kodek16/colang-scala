package colang.tokens

import colang.SourceCode
import colang.tokens.LexerImpl.StatelessTokenStrategy

/**
  * Represents a keyword. This trait helps the parser to produce better error messages when a keyword is used instead
  * of an identifier.
  */
trait Keyword extends Token

/**
  * 'struct' keyword
  */
case class StructKeyword(source: SourceCode) extends Keyword
object StructKeyword {
  val strategy = new StatelessTokenStrategy(StructKeyword.apply, """\bstruct\b""".r)
}

/**
  * 'native' keyword
  */
case class NativeKeyword(source: SourceCode) extends Keyword
object NativeKeyword {
  val strategy = new StatelessTokenStrategy(NativeKeyword.apply, """\bnative\b""".r)
}