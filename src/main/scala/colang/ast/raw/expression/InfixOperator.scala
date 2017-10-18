package colang.ast.raw.expression

import colang.Strategy.Result
import colang.Strategy.Result.{Skipped, NoMatch, Matched}
import colang._
import colang.ast.raw.ParserImpl
import colang.ast.raw.ParserImpl.SingleTokenStrategy
import colang.issues.{Issue, Issues}
import colang.tokens.Associativity.Associativity
import colang.tokens._

import scala.annotation.tailrec

/**
  * Represents a binary infix operator expression.
  * @param lhs left operand
  * @param operator operator
  * @param rhs right operand
  */
case class InfixOperator(lhs: Expression, operator: tokens.InfixOperator, rhs: Expression) extends Expression {
  def source = lhs.source + rhs.source
}

object InfixOperator {

  val strategy: ParserImpl.Strategy[Expression] = new RHSStrategy

  private val operatorStrategy = StrategyUnion(
    SingleTokenStrategy(classOf[AsKeyword]),
    SingleTokenStrategy(classOf[Pow]),
    SingleTokenStrategy(classOf[Multiply]),
    SingleTokenStrategy(classOf[Divide]),
    SingleTokenStrategy(classOf[Mod]),
    SingleTokenStrategy(classOf[Plus]),
    SingleTokenStrategy(classOf[Minus]),
    SingleTokenStrategy(classOf[Less]),
    SingleTokenStrategy(classOf[Greater]),
    SingleTokenStrategy(classOf[LessOrEquals]),
    SingleTokenStrategy(classOf[GreaterOrEquals]),
    SingleTokenStrategy(classOf[Equals]),
    SingleTokenStrategy(classOf[NotEquals]),
    SingleTokenStrategy(classOf[LogicalAnd]),
    SingleTokenStrategy(classOf[LogicalOr]),
    SingleTokenStrategy(classOf[Assign]))

  private class RHSStrategy(bindingPrecedence: Int = 0,
                            bindingAssociativity: Associativity = Associativity.RIGHT) extends ParserImpl.Strategy[Expression] {

    def apply(stream: TokenStream): Result[TokenStream, Expression] = {
      @tailrec
      def possiblyExtendRight(lhs: Expression,
                              stream: TokenStream,
                              collectedIssues: Vector[Issue] = Vector.empty): (Expression, Seq[Issue], TokenStream) = {

        operatorStrategy(stream) match {
          case Matched(operator, operatorIssues, streamAfterOperator) =>
            if (operator.precedence < bindingPrecedence
              || (operator.precedence == bindingPrecedence && bindingAssociativity == Associativity.LEFT)) {
              (lhs, collectedIssues, stream)
            } else {
              new RHSStrategy(operator.precedence, operator.associativity).apply(streamAfterOperator) match {
                case Matched(rhs, rhsIssues, streamAfterRhs) =>
                  val newExpr = InfixOperator(lhs, operator, rhs)
                  val newIssues = collectedIssues ++ operatorIssues ++ rhsIssues
                  possiblyExtendRight(newExpr, streamAfterRhs, newIssues)
                case Skipped(rhsIssues, streamAfterRhs) =>
                  (lhs, collectedIssues ++ operatorIssues ++ rhsIssues, streamAfterRhs)
                case NoMatch() =>
                  val issue = Issues.MissingRightOperand(operator.source.after, operator.name)
                  (lhs, collectedIssues ++ operatorIssues :+ issue, streamAfterOperator)
              }
            }
          case Skipped(_, _) | NoMatch() => (lhs, collectedIssues, stream)
        }
      }

      Expression.secondaryStrategy(stream) match {
        case Matched(expr1, expr1Issues, streamAfterExpr1) =>
          possiblyExtendRight(expr1, streamAfterExpr1, expr1Issues.toVector) match {
            case (resultExpr, issues, streamAfterEverything) => Matched(resultExpr, issues, streamAfterEverything)
          }
        case Skipped(issues, streamAfterExpr1) => Skipped(issues, streamAfterExpr1)
        case NoMatch() => NoMatch()
      }
    }
  }
}
