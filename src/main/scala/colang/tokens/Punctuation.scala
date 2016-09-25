package colang.tokens

import colang.SourceCode
import colang.tokens.LexerImpl.StatelessTokenStrategy

/**
  * '(' token
  */
case class LeftParen(source: SourceCode) extends Token
object LeftParen {
  val strategy = new StatelessTokenStrategy(LeftParen.apply, """\(""".r)
}

/**
  * ')' token
  */
case class RightParen(source: SourceCode) extends Token
object RightParen {
  val strategy = new StatelessTokenStrategy(RightParen.apply, """\)""".r)
}

/**
  * '{' token
  */
case class LeftBrace(source: SourceCode) extends Token
object LeftBrace {
  val strategy = new StatelessTokenStrategy(LeftBrace.apply, """\{""".r)
}

/**
  * '}' token
  */
case class RightBrace(source: SourceCode) extends Token
object RightBrace {
  val strategy = new StatelessTokenStrategy(RightBrace.apply, """}""".r)
}

/**
  * ',' token
  */
case class Comma(source: SourceCode) extends Token
object Comma {
  val strategy = new StatelessTokenStrategy(Comma.apply, """,""".r)
}

/**
  * '.' token
  */
case class Dot(source: SourceCode) extends Token
object Dot {
  val strategy = new StatelessTokenStrategy(Dot.apply, """\.(?!\.)""".r)
}

/**
  * ';' token
  */
case class Semicolon(source: SourceCode) extends Token
object Semicolon {
  val strategy = new StatelessTokenStrategy(Semicolon.apply, """;""".r)
}

/**
  * '&' token (not '&&' or longer).
  */
case class Ampersand(source: SourceCode) extends Token
object Ampersand {
  val strategy = new StatelessTokenStrategy(Ampersand.apply, """&(?!&)""".r)
}
