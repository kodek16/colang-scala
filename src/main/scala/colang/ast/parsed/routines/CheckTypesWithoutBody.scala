package colang.ast.parsed.routines

import colang.ast.parsed.Type
import colang.ast.raw
import colang.issues.{Issue, Issues}

private[routines] object CheckTypesWithoutBody {

  /**
    * Generates issues for types without a body.
    * @param types types to check
    * @return generates issues
    */
  def checkTypesWithoutBody(types: Seq[Type]): Seq[Issue] = {
    types flatMap { type_ =>
      type_.definition match {
        case None => Seq.empty
        case Some(raw.TypeDefinition(_, _, _, Some(_))) => Seq.empty
        case Some(td @ raw.TypeDefinition(_, _, _, None)) =>
          val issue = Issues.TypeDefinitionWithoutBody(td.source, ())
          Seq(issue)
      }
    }
  }
}
