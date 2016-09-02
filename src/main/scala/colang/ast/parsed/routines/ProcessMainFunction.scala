package colang.ast.parsed.routines

import colang.SourceCode
import colang.ast.parsed.statement.Statement
import colang.ast.parsed.{Function, OverloadedFunction, RootNamespace, Symbol}
import colang.issues.{Issue, Issues}

private[routines] object ProcessMainFunction {

  /**
    * Checks that 'main' function is valid and injects global variable initializers there.
    * @param rootNamespace root namespace
    * @param globalVarInitStatements global variable initializers
    * @param eof source code fragment pointing to the end of source CO file
    * @return encountered issues
    */
  def processMainFunction(rootNamespace: RootNamespace,
                          globalVarInitStatements: Seq[Statement],
                          eof: SourceCode): Seq[Issue] = {

    def processExistingMainFunction(main: Function): Seq[Issue] = {
      if (main.returnType == rootNamespace.voidType && main.parameters.isEmpty) {
        //Inject global variable initializers here
        main.body.statements.prepend(globalVarInitStatements :_*)
        Seq.empty
      } else if (main.definitionSite.isDefined) {
        Seq(Issues.InvalidMainFunctionSignature(main.definitionSite.get, ()))
      } else Seq.empty
    }

    rootNamespace.resolve("main") match {
      case Some(of: OverloadedFunction) =>
        val (overload, _) = of.resolveOverload(Seq.empty, None)
        overload match {
          case Some(o) => processExistingMainFunction(o)
          case None =>
            val someDefinedMainOption = of.allOverloads.find { _.definitionSite.isDefined }
            someDefinedMainOption match {
              case Some(invalidMain) => Seq(Issues.InvalidMainFunctionSignature(invalidMain.definitionSite.get, ()))
              case None => Seq(Issues.MissingMainFunction(eof, ()))
            }
        }

      case Some(f: Function) => processExistingMainFunction(f)
      case Some(s: Symbol) if s.definitionSite.isDefined =>
        Seq(Issues.MainIsNotFunction(s.definitionSite.get, ()))
      case _ =>
        Seq(Issues.MissingMainFunction(eof, ()))
    }
  }
}
