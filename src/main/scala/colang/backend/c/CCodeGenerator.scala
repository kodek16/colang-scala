package colang.backend.c

import colang.ast.parsed.expression._
import colang.ast.parsed.statement.{IfElseStatement, ReturnStatement, Statement, WhileStatement}
import colang.ast.parsed.{CodeBlock, Function, Method, Namespace, OverloadedFunction, Symbol, Type, Variable}
import colang.backend.Backend
import colang.utils.SeqUtils

import scala.annotation.tailrec

/**
  * C backend implementation.
  * @param writer C code writer to use
  */
class CCodeGenerator(writer: CCodeWriter) extends Backend {

  def process(rootNamespace: Namespace): Unit = {
    val typeDefs = generateTypeDefinitions(rootNamespace)
    val varDefs = generateVariableDefinitions(rootNamespace)
    val funcProtos = generateFunctionPrototypes(rootNamespace)
    val funcDefs = generateFunctionDefinitions(rootNamespace)

    val headers = Seq("stdlib.h", "stdio.h", "math.h", "stdint.h")
    val sourceFile = CSourceFile(headers, typeDefs, varDefs, funcProtos, funcDefs)

    writer.write(sourceFile)
  }

  /**
    * Generates C struct definitions for non-native types.
    * @param rootNamespace root namespace
    * @return C struct definitions
    */
  private def generateTypeDefinitions(rootNamespace: Namespace): Seq[CTypeDefinition] = {
    @tailrec
    def generateTypeDefs(symbols: Seq[Symbol],
                         generatedDefs: Vector[CTypeDefinition] = Vector.empty): Vector[CTypeDefinition] = {
      symbols match {
        case (type_ : Type) +: rest if !type_.native =>
          generateTypeDefs(rest, generatedDefs :+ CTypeDefinition(type_))
        case _ +: rest => generateTypeDefs(rest, generatedDefs)
        case _ => generatedDefs
      }
    }

    generateTypeDefs(rootNamespace.allMembers)
  }

  /**
    * Generates C function prototypes for non-native functions and methods.
    * @param rootNamespace root namespace
    * @return C function prototypes
    */
  private def generateFunctionPrototypes(rootNamespace: Namespace): Seq[CSimpleStatement] = {
    @tailrec
    def generateFuncProtos(symbols: Seq[Symbol],
                           generatedProtos: Vector[CSimpleStatement] = Vector.empty): Vector[CSimpleStatement] = {
      symbols match {
        case (overloadedFunction: OverloadedFunction) +: rest =>
          val newProtos = overloadedFunction.allOverloads filterNot { _.native } map generateFunctionPrototype
          generateFuncProtos(rest, generatedProtos ++ newProtos)

        case (function: Function) +: rest if !function.native =>
          generateFuncProtos(rest, generatedProtos :+ generateFunctionPrototype(function))

        case (type_ : Type) +: rest =>
          val methodsProtos = type_.allMethods filter { !_.native } map generateMethodPrototype
          generateFuncProtos(rest, generatedProtos ++ methodsProtos)

        case _ +: rest => generateFuncProtos(rest, generatedProtos)
        case _ => generatedProtos
      }
    }

    generateFuncProtos(rootNamespace.allMembers)
  }

  /**
    * Extracts variables from the root namespace.
    * @param rootNamespace root namespace
    * @return extracted variables
    */
  private def generateVariableDefinitions(rootNamespace: Namespace): Seq[Variable] = {
    (rootNamespace.allMembers filter { _.isInstanceOf[Variable] }).asInstanceOf[Seq[Variable]]
  }

  /**
    * Generates C function definitions for non-native functions and methods.
    * @param rootNamespace root namespace
    * @return C function definitions
    */
  private def generateFunctionDefinitions(rootNamespace: Namespace): Seq[CBlock] = {
    def generateFuncDef(function: Function): CBlock = {
      val proto = generateFunctionPrototype(function)
      val body = generateCodeBlock(function.body, ignoredVariables = function.parameters)
      CBlock(proto.tokens, body.variables, body.statements)
    }

    def generateMethDef(method: Method): CBlock = {
      val proto = generateMethodPrototype(method)
      val body = generateCodeBlock(method.body, ignoredVariables = method.parameters)
      CBlock(proto.tokens, body.variables, body.statements)
    }

    @tailrec
    def generateFuncDefs(symbols: Seq[Symbol],
                         generatedDefs: Vector[CBlock] = Vector.empty): Vector[CBlock] = {
      symbols match {
        case (overloadedFunction: OverloadedFunction) +: rest =>
          val newDefs = overloadedFunction.allOverloads filterNot { _.native } map generateFuncDef
          generateFuncDefs(rest, generatedDefs ++ newDefs)

        case (function: Function) +: rest if !function.native =>
          generateFuncDefs(rest, generatedDefs :+ generateFuncDef(function))

        case (type_ : Type) +: rest =>
          val methodsDefs = type_.allMethods filter { !_.native } map generateMethDef
          generateFuncDefs(rest, generatedDefs ++ methodsDefs)

        case _ +: rest => generateFuncDefs(rest, generatedDefs)
        case _ => generatedDefs
      }
    }

    generateFuncDefs(rootNamespace.allMembers)
  }

  /**
    * Generates a C code block from a CO code block.
    * @param block CO code block
    * @param ignoredVariables variables that were already defined in another form (e.g. function parameters)
    * @return C code block
    */
  private def generateCodeBlock(block: CodeBlock, ignoredVariables: Seq[Variable] = Seq.empty): CBlock = {
    val variables = block.innerScope.allMembers.asInstanceOf[Seq[Variable]] filterNot {
      ignoredVariables.contains(_)
    }

    val statements = block.statements map generateStatement
    CBlock(Seq.empty, variables, statements)
  }

  /**
    * Generates a C statement from a CO statement.
    * @param statement CO statement
    * @return C statement
    */
  private def generateStatement(statement: Statement): CStatement = {
    statement match {
      case ifElseStmt: IfElseStatement =>
        val condition = generateExpression(ifElseStmt.condition)
        val heading = CLiteralToken("if") +:
          COptionalSpaceToken() +:
          CLiteralToken("(") +:
          condition.tokens :+
          CLiteralToken(")")

        val ifBranch = generateCodeBlock(ifElseStmt.ifBranch)

        val tail = ifElseStmt.elseBranch match {
          case Some(elseBlock) =>
            val elseHeading = Seq(CLiteralToken("else"))
            val elseBranch = generateCodeBlock(elseBlock)
            Some(CBlock(elseHeading, elseBranch.variables, elseBranch.statements))
          case None => None
        }

        CBlock(heading, ifBranch.variables, ifBranch.statements, tail)

      case whileStmt: WhileStatement =>
        val condition = generateExpression(whileStmt.condition)
        val heading = CLiteralToken("while") +:
          COptionalSpaceToken() +:
          CLiteralToken("(") +:
          condition.tokens :+
          CLiteralToken(")")

        val loop = generateCodeBlock(whileStmt.loop)

        CBlock(heading, loop.variables, loop.statements)

      case returnStmt: ReturnStatement =>
        val expressionTokens = returnStmt.returnValue match {
          case Some(value) => generateExpression(value).tokens
          case None => Seq.empty
        }

        CSimpleStatement(CLiteralToken("return ") +: expressionTokens)

      case cb: CodeBlock => generateCodeBlock(cb)
      case expr: Expression => CSimpleStatement(generateExpression(expr).tokens)
    }
  }

  /**
    * Generates a C expression from a CO expression.
    * @param expression CO expression
    * @return C expression
    */
  private def generateExpression(expression: Expression): CExpression = {
    expression match {
      case FunctionReference(function, _) => CExpression(Seq(CSymbolReferenceToken(function)))
      case VariableReference(variable, _) => CExpression(Seq(CSymbolReferenceToken(variable)))

      case IntLiteral(value, _) => CExpression(Seq(CLiteralToken(value.toString)))
      case BoolLiteral(value, _) => CExpression(Seq(CLiteralToken(if (value) "1" else "0")))

      case DoubleLiteral(value, _) =>
        val cStringRepr = if (value.isPosInfinity) {
          "INFINITY"
        } else if (value.isNegInfinity) {
          "-INFINITY"
        } else if (value.isNaN) {
          "NAN"
        } else {
          value.toString
        }
        CExpression(Seq(CLiteralToken(cStringRepr)))

      case FunctionCall(function, arguments, _) =>
        val cArguments = arguments map generateExpression

        val argSeparatorSource = Seq(CLiteralToken(","), COptionalSpaceToken())
        val argListSource = SeqUtils.interleave(cArguments map { _.tokens }, argSeparatorSource)

        CExpression(
          Seq(
            CSymbolReferenceToken(function),
            CLiteralToken("(")) ++
          argListSource ++
          Seq(
            CLiteralToken(")")))

      case MethodCall(method, instance, arguments, _) =>
        val cArguments = (instance +: arguments) map generateExpression

        val argSeparatorSource = Seq(CLiteralToken(","), COptionalSpaceToken())
        val argListSource = SeqUtils.interleave(cArguments map { _.tokens }, argSeparatorSource)

        CExpression(
          Seq(
            CMethodReferenceToken(method),
            CLiteralToken("(")) ++
          argListSource ++
          Seq(
            CLiteralToken(")")))
    }
  }

  /**
    * Generates a C function prototype for a CO function.
    * @param function CO function
    * @return C function prototype
    */
  private def generateFunctionPrototype(function: Function): CSimpleStatement = {
    val paramsSources = function.parameters map { v => Seq(
      CSymbolReferenceToken(v.type_),
      CLiteralToken(" "),
      CSymbolReferenceToken(v))
    }

    val paramSeparatorSource = Seq(CLiteralToken(","), COptionalSpaceToken())

    val paramListSource = SeqUtils.interleave(paramsSources, paramSeparatorSource)

    CSimpleStatement(
      Seq(
        CSymbolReferenceToken(function.returnType),
        CLiteralToken(" "),
        CSymbolReferenceToken(function),
        CLiteralToken("(")) ++
      paramListSource ++
      Seq(CLiteralToken(")")))
  }

  /**
    * Generates a C function prototype for a CO method.
    * @param method CO method
    * @return C function prototype
    */
  private def generateMethodPrototype(method: Method): CSimpleStatement = {
    val instanceParamSource = Seq(
      CSymbolReferenceToken(method.container),
      CLiteralToken(" "),
      CLiteralToken("_this"))

    val paramsSources = instanceParamSource +: (method.parameters map { v => Seq(
      CSymbolReferenceToken(v.type_),
      CLiteralToken(" "),
      CSymbolReferenceToken(v))
    })

    val paramSeparatorSource = Seq(CLiteralToken(","), COptionalSpaceToken())

    val paramListSource = SeqUtils.interleave(paramsSources, paramSeparatorSource)

    CSimpleStatement(
      Seq(
        CSymbolReferenceToken(method.returnType),
        CLiteralToken(" "),
        CMethodReferenceToken(method),
        CLiteralToken("(")) ++
      paramListSource ++
      Seq(CLiteralToken(")")))
  }
}