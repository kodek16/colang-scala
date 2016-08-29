package colang.ast.parsed.routines

import colang.ast.parsed.Function
import colang.ast.raw
import colang.Error
import colang.issues.{Error, Issue}

private[routines] object AnalyzeFunctionBodies {

  /**
    * Analyzes function bodies.
    * @param functions functions to analyze
    * @return encountered issues
    */
  def analyzeFunctionBodies(functions: Seq[Function]): Seq[Issue] = {
    functions flatMap { function =>
      val paramIssues = function.parameters flatMap { function.body.innerScope.tryAdd(_) }

      val bodyIssues = function.definition match {
        case Some(funcDef @ raw.FunctionDefinition(_, _, _, _, Some(rawBody))) =>
          val nativeIssues = if (function.native) {
            Seq(Error(funcDef.prototypeSource, "native function can't be defined with a body"))
          } else Seq.empty

          val statementIssues = function.body.addStatementsFromBlock(rawBody)
          nativeIssues ++ statementIssues
        case Some(funcDef @ raw.FunctionDefinition(_, _, _, _, None)) if !function.native =>
          Seq(Error(funcDef.prototypeSource, "missing function body"))

        case _ => Seq.empty
      }

      paramIssues ++ bodyIssues
    }
  }
}
