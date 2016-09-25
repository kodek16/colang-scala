package colang.ast.parsed.routines

import colang.ast.parsed.Constructor
import colang.ast.raw
import colang.issues.{Issue, Issues}

private[routines] object AnalyzeConstructorBodies {

  /**
    * Analyzes constructor bodies.
    * @param constructors constructors to analyze
    * @return encountered issues
    */
  def analyzeConstructorBodies(constructors: Seq[Constructor]): Seq[Issue] = {
    constructors flatMap { constructor =>
      val paramIssues = constructor.parameters flatMap { constructor.body.innerScope.tryAdd(_) }

      val bodyIssues = constructor.definition match {
        case Some(constructorDef @ raw.ConstructorDefinition(_, _, _, Some(rawBody))) =>
          val nativeIssues = if (constructor.native) {
            Seq(Issues.NativeConstructorWithBody(constructorDef.prototypeSource, ()))
          } else Seq.empty

          val statementIssues = constructor.body.addStatementsFromBlock(rawBody)
          nativeIssues ++ statementIssues

        case Some(constructorDef @ raw.ConstructorDefinition(_, _, _, None)) if !constructor.native =>
          Seq(Issues.ConstructorDefinitionWithoutBody(constructorDef.prototypeSource, ()))

        case _ => Seq.empty
      }

      paramIssues ++ bodyIssues
    }
  }
}
