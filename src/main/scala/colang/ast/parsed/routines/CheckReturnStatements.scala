package colang.ast.parsed.routines

import colang.ast.parsed.statement.{IfElseStatement, ReturnStatement, Statement, WhileStatement}
import colang.ast.parsed.{CodeBlock, Function, Method}
import colang.ast.raw
import colang.issues.{Issue, Issues}

private[routines] object CheckReturnStatements {

  /**
    * Checks that every function and method execution branch contains a return statement, and that there are
    * none statements after it.
    * @param functions functions to check
    * @param methods methods to check
    * @return encountered issues
    */
  def checkReturnStatements(functions: Seq[Function], methods: Seq[Method]): Seq[Issue] = {
    val functionsIssues = functions flatMap { function =>
      val result = analyzeReturns(function.body)

      val missingReturnIssue = result match {
        case WontReturn(_) if function.returnType != function.scope.get.root.voidType =>
          function.definition flatMap { funcDef =>
            funcDef.body flatMap { rawBody =>
              Some(Issues.MissingReturnStatement(rawBody.rightBrace.source.before, ()))
            }
          }

        case _ => None
      }

      result.issues ++ missingReturnIssue
    }

    val methodsIssues = methods flatMap { method =>
      val result = analyzeReturns(method.body)

      val missingReturnIssue = result match {
        case WontReturn(_) if method.returnType != method.container.scope.get.root.voidType =>
          method.definition flatMap { methodDef =>
            methodDef.body flatMap { rawBody =>
              Some(Issues.MissingReturnStatement(rawBody.rightBrace.source.before, ()))
            }
          }

        case _ => None
      }

      result.issues ++ missingReturnIssue
    }

    functionsIssues ++ methodsIssues
  }

  private sealed trait ReturnAnalysisResult {
    def issues: Seq[Issue]
  }

  private case class WillReturn(issues: Seq[Issue]) extends ReturnAnalysisResult
  private case class WontReturn(issues: Seq[Issue]) extends ReturnAnalysisResult

  /**
    * Analyzes given statement and determines if it is guaranteed to return and what type, if any, does it return;
    * and generates issues for unreachable code.
    * @param statement statement to check
    * @return ReturnAnalysisResult
    */
  private def analyzeReturns(statement: Statement): ReturnAnalysisResult = statement match {
    case IfElseStatement(_, ifBranch, None, _) =>
      val ifBranchResult = analyzeReturns(ifBranch)
      WontReturn(ifBranchResult.issues)

    case IfElseStatement(_, ifBranch, Some(elseBranch), _) =>
      val ifBranchResult = analyzeReturns(ifBranch)
      val elseBranchResult = analyzeReturns(elseBranch)

      (ifBranchResult, elseBranchResult) match {
        //Both branches return
        case (WillReturn(ifIssues), WillReturn(elseIssues)) =>
          WillReturn(ifIssues ++ elseIssues)

        //At least one branch doesn't return
        case (ifResult: ReturnAnalysisResult, elseResult: ReturnAnalysisResult) =>
          WontReturn(ifResult.issues ++ elseResult.issues)
      }

    case WhileStatement(_, loop, _) =>
      val loopResult = analyzeReturns(loop)
      WontReturn(loopResult.issues)

    case ReturnStatement(_, _) =>
      WillReturn(Seq.empty)

    case cb: CodeBlock =>
      def addToResult(previousResult: ReturnAnalysisResult,
                      statementResult: (Option[raw.Node], ReturnAnalysisResult)): ReturnAnalysisResult = {
        (previousResult, statementResult) match {
          case (WillReturn(previousIssues), (rawStmt, _)) =>
            val issue = rawStmt match {
              case Some(r) => Some(Issues.UnreachableCode(r.source, ()))
              case None => None
            }

            WillReturn(previousIssues ++ issue)

          case (WontReturn(previousIssues), (_, WillReturn(thisIssues))) =>
            WillReturn(previousIssues ++ thisIssues)

          case (WontReturn(previousIssues), (_, WontReturn(thisIssues))) =>
            WontReturn(previousIssues ++ thisIssues)
        }
      }

      val statementsResults = (cb.statements map { _.rawNode }) zip (cb.statements map analyzeReturns)
      statementsResults.foldLeft[ReturnAnalysisResult] (WontReturn(Seq.empty)) (addToResult)

    case _ => WontReturn(Seq.empty)
  }
}
