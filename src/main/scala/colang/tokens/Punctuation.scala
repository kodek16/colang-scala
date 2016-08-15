package colang.tokens

import colang.SourceCode

/**
  * '(' token
  */
case class LeftParen(source: SourceCode) extends Token
object LeftParen {
  val strategy = new LexerImpl.StatelessTokenStrategy[LeftParen](LeftParen.apply, """\(""".r)
}

/**
  * ')' token
  */
case class RightParen(source: SourceCode) extends Token
object RightParen {
  val strategy = new LexerImpl.StatelessTokenStrategy[RightParen](RightParen.apply, """\)""".r)
}

/**
  * '{' token
  */
case class LeftBrace(source: SourceCode) extends Token
object LeftBrace {
  val strategy = new LexerImpl.StatelessTokenStrategy[LeftBrace](LeftBrace.apply, """\{""".r)
}

/**
  * '}' token
  */
case class RightBrace(source: SourceCode) extends Token
object RightBrace {
  val strategy = new LexerImpl.StatelessTokenStrategy[RightBrace](RightBrace.apply, """}""".r)
}

/**
  * ',' token
  */
case class Comma(source: SourceCode) extends Token
object Comma {
  val strategy = new LexerImpl.StatelessTokenStrategy[Comma](Comma.apply, """,""".r)
}

/**
  * ';' token
  */
case class Semicolon(source: SourceCode) extends Token
object Semicolon {
  val strategy = new LexerImpl.StatelessTokenStrategy[Semicolon](Semicolon.apply, """;""".r)
}