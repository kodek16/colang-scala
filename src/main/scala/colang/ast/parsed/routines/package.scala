package colang.ast.parsed

/**
  * This package contains various analyzer routines, each in its own file.
  */
package object routines {
  val analyzeFunctionBodies = AnalyzeFunctionBodies.analyzeFunctionBodies _
  val checkReturnStatements = CheckReturnStatements.checkReturnStatements _
  val checkTypesWithoutBody = CheckTypesWithoutBody.checkTypesWithoutBody _
  val processMainFunction = ProcessMainFunction.processMainFunction _
  val registerFunctions = RegisterFunctions.registerFunctions _
  val registerGlobalVariables = RegisterGlobalVariables.registerGlobalVariables _
  val registerMethods = RegisterMethods.registerMethods _
  val registerTypes = RegisterTypes.registerTypes _
  val registerVariables = RegisterVariables.registerVariables _
}
