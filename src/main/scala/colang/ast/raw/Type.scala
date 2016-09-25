package colang.ast.raw

import colang.ast.raw.expression.Expression

object Type {

  /**
    * In CO syntax, types are also expressions (sometimes called "type expressions"), so an alias for the common
    * expression strategy is used here (Type.strategy is more descriptive than Expression.strategy, though they refer
    * to the same entity that produces Expression instances).
    */
  val strategy = Expression.strategy
}
