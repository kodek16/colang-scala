package colang.ast.parsed.routines

import colang.ast.parsed.Method
import colang.ast.raw
import colang.issues.{Issue, Issues}

private[routines] object AnalyzeMethodBodies {

  /**
    * Analyzes method bodies.
    * @param methods methods to analyze
    * @return encountered issues
    */
  def analyzeMethodBodies(methods: Seq[Method]): Seq[Issue] = {
    methods flatMap { method =>
      val paramIssues = method.parameters flatMap { method.body.innerScope.tryAdd(_) }

      val bodyIssues = method.definition match {
        case Some(methDef @ raw.FunctionDefinition(_, _, _, _, _, Some(rawBody))) =>
          val nativeIssues = if (method.native) {
            Seq(Issues.NativeMethodWithBody(methDef.prototypeSource, ()))
          } else Seq.empty

          val statementIssues = method.body.addStatementsFromBlock(rawBody)
          nativeIssues ++ statementIssues

        case Some(methDef @ raw.FunctionDefinition(_, _, _, _, _, None)) if !method.native =>
          Seq(Issues.MethodDefinitionWithoutBody(methDef.prototypeSource, ()))

        case _ => Seq.empty
      }

      paramIssues ++ bodyIssues
    }
  }
}
