package colang.ast.parsed

import colang.SourceCode
import colang.ast.raw
import colang.issues.Issue

/**
  * Represents a compiler component that performs semantic analysis of the code, establishes symbol references and
  * does the last issue check.
  */
trait Analyzer {

  /**
    * Performs the code analysis.
    * @param symbolDefs raw top-level symbol definitions
    * @param eof source code fragment pointing to the end of source CO file
    * @return (populated root namespace, found issues)
    */
  def analyze(symbolDefs: Seq[raw.GlobalSymbolDefinition], eof: SourceCode): (RootNamespace, Seq[Issue])
}

/**
  * Actual analyzer implementation.
  * Analysis is performed step-by-step, and so is mutable by nature.
  * The usual pattern is to first "register" a new symbol in its container, without actually analyzing anything besides
  * its name and type. This allows to have non-trivial forward references, and in some cases circular dependencies
  * between symbols. After registering all symbols, full analysis is performed, possibly in several stages for the
  * same reason as above.
  * Sometimes a symbol can't be registered because of a name conflict. In this case, the created symbol is kept and
  * further analysis is done as usual, but the container won't be aware of the symbol: other symbols won't be able to
  * reference it. Symbol in this state is called "detached".
  */
class AnalyzerImpl extends Analyzer {
  def analyze(symbolDefs: Seq[raw.GlobalSymbolDefinition], eof: SourceCode): (RootNamespace, Seq[Issue]) = {
    val typeDefs = (symbolDefs filter { _.isInstanceOf[raw.TypeDefinition] }).
      asInstanceOf[Seq[raw.TypeDefinition]]
    val funcDefs = (symbolDefs filter { _.isInstanceOf[raw.FunctionDefinition] }).
      asInstanceOf[Seq[raw.FunctionDefinition]]
    val varDefs = (symbolDefs filter { _.isInstanceOf[raw.statement.VariablesDefinition] }).
      asInstanceOf[Seq[raw.statement.VariablesDefinition]]

    val rootNamespace = new RootNamespace()

    val (types, typesIssues) = routines.registerTypes(rootNamespace, typeDefs)
    val (functions, functionsIssues) = routines.registerFunctions(rootNamespace, funcDefs)
    val (methods, methodsIssues) = routines.registerMethods(types)
    val (variables, globalVarInitStatements, varIssues) = routines.registerGlobalVariables(rootNamespace, varDefs)
    val funcBodiesIssues = routines.analyzeFunctionBodies(functions)
    val mainFuncIssues = routines.processMainFunction(rootNamespace, globalVarInitStatements, eof)
    val returnIssues = routines.checkReturnStatements(functions)

    val issues = typesIssues ++ functionsIssues ++ methodsIssues ++ varIssues ++ funcBodiesIssues ++
      mainFuncIssues ++ returnIssues

    (rootNamespace, issues)
  }
}
