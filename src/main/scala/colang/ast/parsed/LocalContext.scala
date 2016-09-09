package colang.ast.parsed

import colang.issues.Term

/**
  * This class contains information about local (closest enclosing function) context necessary for checking validity
  * of some statements ('return', eventually also 'break' and 'continue').
  */
case class LocalContext(applicableKind: Term, expectedReturnType: Type)