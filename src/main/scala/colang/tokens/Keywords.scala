package colang.tokens

import colang.SourceCode
import colang.tokens.LexerImpl.StatelessTokenStrategy

/**
  * Represents a keyword. This trait helps the parser to produce better error messages when a keyword is used instead
  * of an identifier.
  */
trait Keyword extends Token {

  /**
    * Textual keyword representation.
    */
  def text: String
}

case class StructKeyword(source: SourceCode) extends Keyword {
  val text = "struct"
}
object StructKeyword {
  val strategy = new StatelessTokenStrategy(StructKeyword.apply, """\bstruct\b""".r)
}

case class NativeKeyword(source: SourceCode) extends Keyword {
  val text = "native"
}
object NativeKeyword {
  val strategy = new StatelessTokenStrategy(NativeKeyword.apply, """\bnative\b""".r)
}

case class IfKeyword(source: SourceCode) extends Keyword {
  val text = "if"
}
object IfKeyword {
  val strategy = new StatelessTokenStrategy(IfKeyword.apply, """\bif\b""".r)
}

case class ElseKeyword(source: SourceCode) extends Keyword {
  val text = "else"
}
object ElseKeyword {
  val strategy = new StatelessTokenStrategy(ElseKeyword.apply, """\belse\b""".r)
}

case class WhileKeyword(source: SourceCode) extends Keyword {
  val text = "while"
}
object WhileKeyword {
  val strategy = new StatelessTokenStrategy(WhileKeyword.apply, """\bwhile\b""".r)
}

case class ReturnKeyword(source: SourceCode) extends Keyword {
  val text = "return"
}
object ReturnKeyword {
  val strategy = new StatelessTokenStrategy(ReturnKeyword.apply, """\breturn\b""".r)
}

case class ThisKeyword(source: SourceCode) extends Keyword {
  val text = "this"
}
object ThisKeyword {
  val strategy = new StatelessTokenStrategy(ThisKeyword.apply, """\bthis\b""".r)
}

case class ConstructorKeyword(source: SourceCode) extends Keyword {
  val text = "constructor"
}
object ConstructorKeyword {
  val strategy = new StatelessTokenStrategy(ConstructorKeyword.apply, """\bconstructor\b""".r)
}

case class StaticKeyword(source: SourceCode) extends Keyword {
  val text = "static"
}
object StaticKeyword {
  val strategy = new StatelessTokenStrategy(StaticKeyword.apply, """\bstatic\b""".r)
}

// AsKeyword is defined in Operators.scala
