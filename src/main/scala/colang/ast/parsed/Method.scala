package colang.ast.parsed

import colang.ast.raw
import colang.issues.Note

/**
  * Represents a method: a function that can only be called on an instance of some type.
  * Note that a method is not a symbol because it isn't stable.
  * @param name method name
  * @param container containing type
  * @param returnType method return type
  * @param parameters method parameters
  * @param body method body
  * @param native whether method is native
  */
class Method(val name: String,
             val container: Type,
             val returnType: Type,
             val parameters: Seq[Variable],
             val body: CodeBlock,
             val definition: Option[raw.FunctionDefinition],
             val native: Boolean = false) extends Applicable {

  val declarationSite = definition match {
    case Some(fd) => Some(fd.prototypeSource)
    case None => None
  }

  /**
    * A convenience method for optionally constructing a note pointing to the declaration site.
    */
  def declarationSiteNotes: Seq[Note] = {
    declarationSite match {
      case Some(site) => Seq(Note(Some(site), "declared here"))
      case None => Seq.empty
    }
  }
}
