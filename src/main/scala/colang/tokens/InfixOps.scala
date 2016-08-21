package colang.tokens

import colang.tokens.Associativity.Associativity
import colang.SourceCode

/**
  * Represents a binary infix operator.
  */
trait InfixOperator extends Token {

  /**
    * Operator precedence: higher values bind operands tighter.
    */
  def precedence: Int
  def associativity: Associativity
}

object Associativity extends Enumeration {
  /**
    * Defines whether an operator binds to the left (like '+': 'a + b + c' is '(a + b) + c')
    * or to the right (like '=': 'a = b = c' is 'a = (b = c)').
    */
  type Associativity = Value
  val LEFT, RIGHT = Value
}

/**
  * '*' operator
  */
case class Multiply(source: SourceCode) extends InfixOperator {
  val precedence = 40
  val associativity = Associativity.LEFT
}
object Multiply {
  val strategy = new LexerImpl.StatelessTokenStrategy(Multiply.apply, """\*(?!\*)""".r)
}

/**
  * '/' operator
  */
case class Divide(source: SourceCode) extends InfixOperator {
  val precedence = 40
  val associativity = Associativity.LEFT
}
object Divide {
  val strategy = new LexerImpl.StatelessTokenStrategy(Divide.apply, """\/(?!\/)""".r)
}

/**
  * '+' operator
  */
case class Plus(source: SourceCode) extends InfixOperator {
  val precedence = 30
  val associativity = Associativity.LEFT
}
object Plus {
  val strategy = new LexerImpl.StatelessTokenStrategy(Plus.apply, """\+(?!\+)""".r)
}

/**
  * '-' operator
  */
case class Minus(source: SourceCode) extends InfixOperator {
  val precedence = 30
  val associativity = Associativity.LEFT
}
object Minus {
  val strategy = new LexerImpl.StatelessTokenStrategy(Minus.apply, """\-(?!\-)""".r)
}

/**
  * '==' operator
  */
case class Equals(source: SourceCode) extends InfixOperator {
  val precedence = 20
  val associativity = Associativity.LEFT
}
object Equals {
  val strategy = new LexerImpl.StatelessTokenStrategy(Equals.apply, """==(?!=)""".r)
}

/**
  * '=' operator
  */
case class Assign(source: SourceCode) extends InfixOperator {
  val precedence = 10
  val associativity = Associativity.RIGHT
}
object Assign {
  val strategy = new LexerImpl.StatelessTokenStrategy(Assign.apply, """=(?!=)""".r)
}