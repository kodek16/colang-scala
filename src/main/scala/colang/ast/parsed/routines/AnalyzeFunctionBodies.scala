package colang.ast.parsed.routines

import colang.ast.parsed.Function
import colang.ast.raw
import colang.issues.{Issue, Issues}

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
        case Some(funcDef @ raw.FunctionDefinition(_, _, _, _, _, Some(rawBody))) =>
          val nativeIssues = if (function.native) {
            Seq(Issues.NativeFunctionWithBody(funcDef.prototypeSource, ()))
          } else Seq.empty

          val statementIssues = function.body.addStatementsFromBlock(rawBody)
          nativeIssues ++ statementIssues

        case Some(funcDef @ raw.FunctionDefinition(_, _, _, _, _, None)) if !function.native =>
          Seq(Issues.FunctionDefinitionWithoutBody(funcDef.prototypeSource, ()))

        case _ => Seq.empty
      }

      paramIssues ++ bodyIssues
    }
  }
}
