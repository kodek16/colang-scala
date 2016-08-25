package colang.ast.parsed.routines

import colang.ast.parsed.statement.{IfElseStatement, ReturnStatement, Statement, WhileStatement}
import colang.ast.parsed.{CodeBlock, Function, Type}
import colang.ast.raw
import colang.{Error, Issue}

private[routines] object CheckReturnStatements {

  /**
    * Checks that every function execution branch contains a return statement, and that there are none statements after
    * it.
    * @param functions functions to check
    * @return encountered issues
    */
  def checkReturnStatements(functions: Seq[Function]): Seq[Issue] = {
    functions flatMap { function =>
      val result = analyzeReturns(function.body, function.returnType)

      val missingReturnIssues = result match {
        case WontReturn(_) if function.returnType != function.scope.get.root.voidType =>
          function.definition match {
            case Some(raw.FunctionDefinition(_, _, _, _, Some(rawBody))) =>
              Seq(Error(rawBody.rightBrace.source.before, "missing 'return' statement"))
            case _ => Seq.empty
          }
        case _ => Seq.empty
      }

      result.issues ++ missingReturnIssues
    }
  }

  private sealed trait ReturnAnalysisResult {
    def issues: Seq[Issue]
  }

  private case class WillReturn(returnValueType: Option[Type], issues: Seq[Issue]) extends ReturnAnalysisResult
  private case class WontReturn(issues: Seq[Issue]) extends ReturnAnalysisResult

  /**
    * Analyzes given statement and determines if it is guaranteed to return and what type, if any, does it return.
    * Also generates issues for unreachable code and incompatible return types.
    * @param statement statement to check
    * @param expectedReturnType enclosing function return type
    * @return ReturnAnalysisResult
    */
  private def analyzeReturns(statement: Statement, expectedReturnType: Type): ReturnAnalysisResult = statement match {
    case IfElseStatement(_, ifBranch, None, _) =>
      val ifBranchResult = analyzeReturns(ifBranch, expectedReturnType)
      WontReturn(ifBranchResult.issues)

    case IfElseStatement(_, ifBranch, Some(elseBranch), _) =>
      val ifBranchResult = analyzeReturns(ifBranch, expectedReturnType)
      val elseBranchResult = analyzeReturns(elseBranch, expectedReturnType)

      (ifBranchResult, elseBranchResult) match {
        //Both branches return
        case (WillReturn(ifRetValType, ifIssues), WillReturn(elseRetValType, elseIssues)) =>
          val possibleReturnTypes = (ifRetValType ++ elseRetValType).toSeq
          val lub = Type.leastUpperBound(possibleReturnTypes :_*)

          WillReturn(lub, ifIssues ++ elseIssues)

        //At least one branch doesn't return
        case (ifResult: ReturnAnalysisResult, elseResult: ReturnAnalysisResult) =>
          WontReturn(ifResult.issues ++ elseResult.issues)
      }

    case WhileStatement(_, loop, _) =>
      val loopResult = analyzeReturns(loop, expectedReturnType)
      WontReturn(loopResult.issues)

    case ReturnStatement(retValOption, rawStmtOption) =>
      retValOption match {
        //If this is a 'return-value' statement.
        case Some(returnValue) =>
          val (returnType, issues) = if (!returnValue.type_.isImplicitlyConvertibleTo(expectedReturnType)) {
            rawStmtOption match {
              case Some(rawStmt) =>
                val actualTypeStr = returnValue.type_.qualifiedName
                val returnTypeStr = expectedReturnType.qualifiedName
                val issues = Seq(Error(rawStmt.source, s"a value of type '$actualTypeStr' cannot be returned from " +
                  s"a function returning '$returnTypeStr'"))

                (None, issues)
              case None => (None, Seq.empty)
            }
          } else (Some(returnValue.type_), Seq.empty)

          WillReturn(returnType, issues)

        //If this is a 'return-void' statement.
        case None =>
          val issues = if (expectedReturnType != expectedReturnType.scope.get.root.voidType) {
            rawStmtOption match {
              case Some(rawStmt) =>
                Seq(Error(rawStmt.source,
                  s"must return a value from a function returning '${expectedReturnType.qualifiedName}"))
              case None => Seq.empty
            }
          } else Seq.empty

          WillReturn(None, issues)
      }

    case cb: CodeBlock =>
      def addToResult(previousResult: ReturnAnalysisResult,
                      statementResult: (Option[raw.Node], ReturnAnalysisResult)): ReturnAnalysisResult = {
        (previousResult, statementResult) match {
          case (WillReturn(retValType, previousIssues), (rawStmt, _)) =>
            val issue = rawStmt match {
              case Some(r) => Some(Error(r.source, "code after 'return' statement will never be executed"))
              case None => None
            }

            WillReturn(retValType, previousIssues ++ issue)

          case (WontReturn(previousIssues), (_, WillReturn(retValType, thisIssues))) =>
            WillReturn(retValType, previousIssues ++ thisIssues)

          case (WontReturn(previousIssues), (_, WontReturn(thisIssues))) =>
            WontReturn(previousIssues ++ thisIssues)
        }
      }

      val statementsResults = (cb.statements map { _.rawNode }) zip (cb.statements map { analyzeReturns(_, expectedReturnType) })
      (statementsResults foldLeft WontReturn(Seq.empty).asInstanceOf[ReturnAnalysisResult]) (addToResult)

    case _ => WontReturn(Seq.empty)
  }
}
