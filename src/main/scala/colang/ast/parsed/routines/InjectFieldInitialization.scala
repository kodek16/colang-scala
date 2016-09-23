package colang.ast.parsed.routines

import colang.ast.parsed.Type
import colang.ast.parsed.statement.Statement

private[routines] object InjectFieldInitialization {

  /**
    * Injects field initialization statements into all type constructors.
    * @param fieldInitStatements field initialization statements grouped by field container type
    */
  def injectFieldInitialization(fieldInitStatements: Map[Type, Seq[Statement]]): Unit = {
    fieldInitStatements foreach { case (type_, initStatements) =>
      type_.allConstructors foreach { constructor =>
        constructor.body.statements.prepend(initStatements :_*)
      }
    }
  }
}
