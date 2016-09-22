package colang.ast.parsed

import colang.issues.Term

/**
  * This class contains information about local (closest enclosing function) context necessary for checking validity
  * of some statements ('return', eventually also 'break' and 'continue').
  * @param applicableKind what kind of applicable entity is this
  * @param expectedReturnType for functions and methods: Some(expected return type). Returns from constructors are
  *                           forbidden.
  * @param contextualObjectType for methods Some(type of 'this')
  */
case class LocalContext(applicableKind: Term,
                        expectedReturnType: Option[Type],
                        contextualObjectType: Option[Type] = None)

