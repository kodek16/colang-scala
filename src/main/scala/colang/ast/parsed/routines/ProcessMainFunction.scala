package colang.ast.parsed.routines

import colang.ast.parsed.statement.Statement
import colang.ast.parsed.{Function, Namespace, Symbol}
import colang.{Error, Issue, SourceCode}

private[routines] object ProcessMainFunction {

  /**
    * Checks that 'main' function is valid and injects global variable initializers there.
    * @param rootNamespace root namespace
    * @param globalVarInitStatements global variable initializers
    * @param eof source code fragment pointing to the end of source CO file
    * @return encountered issues
    */
  def processMainFunction(rootNamespace: Namespace,
                          globalVarInitStatements: Seq[Statement],
                          eof: SourceCode): Seq[Issue] = {

    rootNamespace.resolve("main") match {
      case Some(f: Function) if f.returnType == rootNamespace.voidType && f.parameters.isEmpty =>
        //Inject global variable initializers here
        f.body.statements.prepend(globalVarInitStatements :_*)
        Seq.empty

      case Some(f: Function) if f.declarationSite.isDefined =>
        Seq(Error(f.declarationSite.get, "'main' function must accept no parameters and return 'void'"))
      case Some(s: Symbol) if s.declarationSite.isDefined =>
        Seq(Error(s.declarationSite.get, "'main' must be a function"))
      case _ =>
        Seq(Error(eof, "missing 'main' function"))
    }
  }
}
