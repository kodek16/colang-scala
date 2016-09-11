package colang.tokens

import colang.SourceCode
import colang.tokens.Associativity.Associativity
import colang.tokens.LexerImpl.StatelessTokenStrategy

/**
  * Represents an unary prefix operator (like '!' or '-').
  */
trait PrefixOperator extends Token {

  /**
    * Textual operator representation (e.g. '!').
    */
  def name: String
}

/**
  * '!' operator
  */
case class LogicalNot(source: SourceCode) extends PrefixOperator {
  val name = "!"
}
object LogicalNot {
  val strategy = new StatelessTokenStrategy(LogicalNot.apply, """\!(?!=)""".r)
}

/**
  * Represents a binary infix operator.
  */
trait InfixOperator extends Token {

  /**
    * Operator precedence: higher values bind operands tighter.
    */
  def precedence: Int
  def associativity: Associativity

  /**
    * Textual operator representation (e.g. '+', '-').
    */
  def name: String
}

object Associativity extends Enumeration {
  /**
    * Defines whether an operator binds to the left (like '+': 'a + b + c' is '(a + b) + c')
    * or to the right (like '=': 'a = b = c' is 'a = (b = c)').
    */
  type Associativity = Value
  val LEFT, RIGHT = Value
}

case class Multiply(source: SourceCode) extends InfixOperator {
  val precedence = 70
  val associativity = Associativity.LEFT
  val name = "*"
}
object Multiply {
  val strategy = new StatelessTokenStrategy(Multiply.apply, """\*(?!\*)""".r)
}

case class Divide(source: SourceCode) extends InfixOperator {
  val precedence = 70
  val associativity = Associativity.LEFT
  val name = "/"
}
object Divide {
  val strategy = new StatelessTokenStrategy(Divide.apply, """\/(?!\/)""".r)
}

case class Mod(source: SourceCode) extends InfixOperator {
  val precedence = 70
  val associativity = Associativity.LEFT
  val name = "%"
}
object Mod {
  val strategy = new StatelessTokenStrategy(Mod.apply, """%(?!%)""".r)
}

case class Plus(source: SourceCode) extends InfixOperator {
  val precedence = 60
  val associativity = Associativity.LEFT
  val name = "+"
}
object Plus {
  val strategy = new StatelessTokenStrategy(Plus.apply, """\+(?!\+)""".r)
}

/**
  * '-' operator (can be either binary or unary).
  */
case class Minus(source: SourceCode) extends InfixOperator with PrefixOperator {
  val precedence = 60
  val associativity = Associativity.LEFT
  val name = "-"
}
object Minus {
  val strategy = new StatelessTokenStrategy(Minus.apply, """\-(?!\-)""".r)
}

case class Less(source: SourceCode) extends InfixOperator {
  val precedence = 50
  val associativity = Associativity.LEFT
  val name = "<"
}
object Less {
  val strategy = new StatelessTokenStrategy(Less.apply, """<(?![<=])""".r)
}

case class Greater(source: SourceCode) extends InfixOperator {
  val precedence = 50
  val associativity = Associativity.LEFT
  val name = ">"
}
object Greater {
  val strategy = new StatelessTokenStrategy(Greater.apply, """>(?![>=])""".r)
}

case class LessOrEquals(source: SourceCode) extends InfixOperator {
  val precedence = 50
  val associativity = Associativity.LEFT
  val name = "<="
}
object LessOrEquals {
  val strategy = new StatelessTokenStrategy(LessOrEquals.apply, """<=(?!=)""".r)
}

case class GreaterOrEquals(source: SourceCode) extends InfixOperator {
  val precedence = 50
  val associativity = Associativity.LEFT
  val name = ">="
}
object GreaterOrEquals {
  val strategy = new StatelessTokenStrategy(GreaterOrEquals.apply, """>=(?!=)""".r)
}

case class Equals(source: SourceCode) extends InfixOperator {
  val precedence = 40
  val associativity = Associativity.LEFT
  val name = "=="
}
object Equals {
  val strategy = new StatelessTokenStrategy(Equals.apply, """==(?!=)""".r)
}

case class NotEquals(source: SourceCode) extends InfixOperator {
  val precedence = 40
  val associativity = Associativity.LEFT
  val name = "!="
}
object NotEquals {
  val strategy = new StatelessTokenStrategy(NotEquals.apply, """!=(?!=)""".r)
}

case class LogicalAnd(source: SourceCode) extends InfixOperator {
  val precedence = 30
  val associativity = Associativity.LEFT
  val name = "&&"
}
object LogicalAnd {
  val strategy = new StatelessTokenStrategy(LogicalAnd.apply, """&&(?!&)""".r)
}

case class LogicalOr(source: SourceCode) extends InfixOperator {
  val precedence = 20
  val associativity = Associativity.LEFT
  val name = "||"
}
object LogicalOr {
  val strategy = new StatelessTokenStrategy(LogicalOr.apply, """\|\|(?!\|)""".r)
}

case class Assign(source: SourceCode) extends InfixOperator {
  val precedence = 10
  val associativity = Associativity.RIGHT
  val name = "="
}
object Assign {
  val strategy = new StatelessTokenStrategy(Assign.apply, """=(?!=)""".r)
}
