package colang.tokens

import colang.SourceCode

/**
  * '(' token
  */
case class LeftParen(source: SourceCode) extends Token
object LeftParen {
  val strategy = new LexerImpl.StatelessTokenStrategy(LeftParen.apply, """\(""".r)
}

/**
  * ')' token
  */
case class RightParen(source: SourceCode) extends Token
object RightParen {
  val strategy = new LexerImpl.StatelessTokenStrategy(RightParen.apply, """\)""".r)
}

/**
  * '{' token
  */
case class LeftBrace(source: SourceCode) extends Token
object LeftBrace {
  val strategy = new LexerImpl.StatelessTokenStrategy(LeftBrace.apply, """\{""".r)
}

/**
  * '}' token
  */
case class RightBrace(source: SourceCode) extends Token
object RightBrace {
  val strategy = new LexerImpl.StatelessTokenStrategy(RightBrace.apply, """}""".r)
}

/**
  * ',' token
  */
case class Comma(source: SourceCode) extends Token
object Comma {
  val strategy = new LexerImpl.StatelessTokenStrategy(Comma.apply, """,""".r)
}

/**
  * ';' token
  */
case class Semicolon(source: SourceCode) extends Token
object Semicolon {
  val strategy = new LexerImpl.StatelessTokenStrategy(Semicolon.apply, """;""".r)
}