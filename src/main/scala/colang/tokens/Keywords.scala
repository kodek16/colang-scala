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

/**
  * 'if' keyword
  */
case class IfKeyword(source: SourceCode) extends Keyword
object IfKeyword {
  val strategy = new StatelessTokenStrategy(IfKeyword.apply, """\bif\b""".r)
}

/**
  * 'else' keyword
  */
case class ElseKeyword(source: SourceCode) extends Keyword
object ElseKeyword {
  val strategy = new StatelessTokenStrategy(ElseKeyword.apply, """\belse\b""".r)
}

/**
  * 'while' keyword
  */
case class WhileKeyword(source: SourceCode) extends Keyword
object WhileKeyword {
  val strategy = new StatelessTokenStrategy(WhileKeyword.apply, """\bwhile\b""".r)
}

case class ReturnKeyword(source: SourceCode) extends Keyword
object ReturnKeyword {
  val strategy = new StatelessTokenStrategy(ReturnKeyword.apply, """\breturn\b""".r)
}