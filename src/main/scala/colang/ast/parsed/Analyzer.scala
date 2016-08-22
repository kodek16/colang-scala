package colang.ast.parsed

import colang.ast.parsed.statement.Statement
import colang.{Error, Issue, SourceCode}
import colang.ast.raw

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
  def analyze(symbolDefs: Seq[raw.GlobalSymbolDefinition], eof: SourceCode): (Namespace, Seq[Issue])
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
  def analyze(symbolDefs: Seq[raw.GlobalSymbolDefinition], eof: SourceCode): (Namespace, Seq[Issue]) = {
    val rootNamespace = new Namespace("", None, None)
    val issues = ListBuffer.empty[Issue]

    val generatedTypes = mutable.Map.empty[raw.TypeDefinition, Type]
    val generatedVariables = mutable.Map.empty[raw.statement.VariablesDefinition, Seq[Variable]]
    val generatedFunctions = mutable.Map.empty[raw.FunctionDefinition, Function]

    val globalVarInitStatements = ListBuffer.empty[Statement]

    registerTypes()
    assertThatNativeTypeDeclarationsArePresent()
    registerFunctions()
    registerMethods()
    registerGlobalVariables()
    analyzeFunctionBodies()
    processMainFunction()

    def registerTypes(): Unit = {
      symbolDefs foreach {
        case td: raw.TypeDefinition =>
          val (type_, typeIssues) = TypeDefinition.register(rootNamespace, td)
          generatedTypes(td) = type_
          issues ++= typeIssues
        case _ => ()
      }
    }

    def assertThatNativeTypeDeclarationsArePresent(): Unit = {
      def assertTypePresent(name: String): Unit = {
        rootNamespace resolve name match {
          case Some(t: Type) => ()
          case _ =>
            System.err.println(s"Error: '$name' type declaration not found in standard library. Please check if your " +
              s"CO installation is correct and up-to-date.")
            sys.exit(2)
        }
      }

      assertTypePresent("void")
      assertTypePresent("int")
      assertTypePresent("double")
      assertTypePresent("bool")
    }

    def registerFunctions(): Unit = {
      symbolDefs foreach {
        case fd: raw.FunctionDefinition =>
          val (function, functionIssues) = FunctionDefinition.register(rootNamespace, fd)
          generatedFunctions(fd) = function
          issues ++= functionIssues
        case _ => ()
      }
    }

    def registerMethods(): Unit = {
      symbolDefs foreach {
        case td: raw.TypeDefinition =>
          issues ++= TypeDefinition.registerMethods(generatedTypes(td), td)
        case _ => ()
      }
    }

    def registerGlobalVariables(): Unit = {
      symbolDefs foreach {
        case vsd: raw.statement.VariablesDefinition =>
          val (vars, initStatements, varsIssues) = VariablesDefinition.register(rootNamespace, vsd)
          generatedVariables(vsd) = vars
          issues ++= varsIssues
          globalVarInitStatements ++= initStatements
        case _ => ()
      }
    }

    def analyzeFunctionBodies(): Unit = {
      symbolDefs foreach {
        case fd: raw.FunctionDefinition =>
          issues ++= FunctionDefinition.analyzeBody(generatedFunctions(fd), fd)
        case _ => ()
      }
    }

    def processMainFunction(): Unit = {
      val mainIssues = rootNamespace.resolve("main") match {
        case Some(f: Function) if f.returnType.qualifiedName == "void" && f.parameters.isEmpty =>
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

      issues ++= mainIssues
    }

    (rootNamespace, issues)
  }
}
