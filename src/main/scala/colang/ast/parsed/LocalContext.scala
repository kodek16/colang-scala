package colang.ast.parsed

import colang.issues.Term

/**
  * This class contains information about local (closest enclosing function) context necessary for checking validity
  * of some statements ('return', eventually also 'break' and 'continue').
  * @param applicableKind what kind of applicable entity is this
  * @param expectedReturnType what is the expected return type
  * @param contextualObjectType for methods Some(type of 'this')
  */
case class LocalContext(applicableKind: Term,
                        expectedReturnType: Type,
                        contextualObjectType: Option[Type] = None)

