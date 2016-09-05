package colang.issues

import java.util.Locale

import colang.SourceCode

/**
  * Issue factories implement this trait by defining locale-specific generation methods. The apply() method is
  * inherited.
  * @tparam T generated issue type
  * @tparam Args arguments types passed to generators
  */
trait LocaleAwareIssueFactory[T <: Issue, Args] {
  protected def en_US(source: SourceCode, args: Args): T
  protected def be_BY(source: SourceCode, args: Args): T
  protected def ru_RU(source: SourceCode, args: Args): T

  def apply(source: SourceCode, args: Args): T = {
    Locale.getDefault.getLanguage match {
      case "en" => en_US(source, args)
      case "be" => be_BY(source, args)
      case "ru" => ru_RU(source, args)
    }
  }
}

object Issues {

  /**
    * Generates an issue for numeric literals that are too big/small for their type.
    * Args: (relevant adjective (big/small), type name)
    */
  object NumericLiteralOverflow extends LocaleAwareIssueFactory[Error, (Adjective, String)] {
    private val code = "E0001"

    protected def en_US(source: SourceCode, args: (Adjective, String)): Error = {
      val (relation, typeName) = (args._1.en_US.text, args._2)
      Error(code, source, s"the literal value is too $relation for type '$typeName'", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: (Adjective, String)): Error = {
      val (relation, typeName) = (args._1.be_BY.neuter.nominative, args._2)
      Error(code, source, s"значэньне канстанты занадта $relation для тыпу '$typeName'", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: (Adjective, String)): Error = {
      val (relation, typeName) = (args._1.ru_RU.neuter.nominative, args._2)
      Error(code, source, s"значение константы слишком $relation для типа '$typeName'", notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for integer literals in scientific form with negative or fractional exponents.
    * Args: suggested 'double'-type literal
    */
  object IntegerLiteralWithNonNaturalExponent extends LocaleAwareIssueFactory[Error, String] {
    private val code = "E0002"

    protected def en_US(source: SourceCode, suggestedDoubleLiteral: String): Error = {
      Error(code, source, "integer literals can't have non-natural exponents. To make this a 'double' type " +
        s"literal, rewrite it as '$suggestedDoubleLiteral'", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, suggestedDoubleLiteral: String): Error = {
      Error(code, source, "цэлыя канстанты ня могуць мець ненатуральныя экспаненты. Каб надаць гэтай канстаньце тып " +
        s"'double', перапішыце яе як '$suggestedDoubleLiteral'", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, suggestedDoubleLiteral: String): Error = {
      Error(code, source, "целые константы не могут содержать ненатуральные экспоненты. Чтобы эта константа имела тип " +
        s"'double', перепишите её как '$suggestedDoubleLiteral'", notes = Seq.empty)
    }
  }

  object UnknownNumberFormat extends LocaleAwareIssueFactory[Error, Unit] {
    private val code = "E0003"

    protected def en_US(source: SourceCode, args: Unit): Error = {
      Error(code, source, "unknown number format", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: Unit): Error = {
      Error(code, source, "лік невядомага фармату", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: Unit): Error = {
      Error(code, source, "число неизвестного формата", notes = Seq.empty)
    }
  }

  object UnknownCharacterSequence extends LocaleAwareIssueFactory[Error, Unit] {
    private val code = "E0004"

    protected def en_US(source: SourceCode, args: Unit): Error = {
      Error(code, source, "unknown character sequence", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: Unit): Error = {
      Error(code, source, "невядомая пасьлядоўнасьць сімвалаў", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: Unit): Error = {
      Error(code, source, "неизвестная последовательность символов", notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for a missing variable initializer.
    * Args: relevant variable name
    */
  object MissingVariableInitializer extends LocaleAwareIssueFactory[Error, String] {
    private val code = "E0005"

    protected def en_US(source: SourceCode, varName: String): Error = {
      Error(code, source, s"missing initializer for variable '$varName' after '='", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, varName: String): Error = {
      Error(code, source, s"прапушчаны ініцыялізатар зьменнай '$varName' пасьля '='", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, varName: String): Error = {
      Error(code, source, s"пропущен инициализатор переменной '$varName' после '='", notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for missing right operand of a binary infix operator expression.
    * Args: operator textual representation
    */
  object MissingRightOperand extends LocaleAwareIssueFactory[Error, String] {
    private val code = "E0006"

    protected def en_US(source: SourceCode, operator: String): Error = {
      Error(code, source, s"missing right operand after '$operator'", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, operator: String): Error = {
      Error(code, source, s"прапушчаны правы аперанд пасьля '$operator'", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, operator: String): Error = {
      Error(code, source, s"пропущен правый операнд после '$operator'", notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for an unexpected specifier on a node.
    * Args: (specifier text, term describing the context)
    */
  object MisplacedSpecifier extends LocaleAwareIssueFactory[Error, (String, Term)] {
    private val code = "E0007"

    protected def en_US(source: SourceCode, args: (String, Term)): Error = {
      val (specifier, context) = (args._1, args._2.en_US)
      Error(code, source, s"${context.indefinite} cannot be '$specifier'", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: (String, Term)): Error = {
      val (specifier, context) = (args._1, args._2.be_BY)
      Error(code, source, s"${context.nominative} ня можа быць '$specifier'", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: (String, Term)): Error = {
      val (specifier, context) = (args._1, args._2.ru_RU)
      Error(code, source, s"${context.nominative} не может быть '$specifier'", notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for a repeated specifier.
    * Args: (specifier text, first occurrence)
    */
  object RepeatedSpecifier extends LocaleAwareIssueFactory[Error, (String, SourceCode)] {
    private val code = "E0008"

    protected def en_US(source: SourceCode, args: (String, SourceCode)): Error = {
      val (specifier, firstOccurrence) = args

      Error(code, source, s"'$specifier' specifier is repeated", notes = Seq(
        Note(Some(firstOccurrence), "first occurrence is here")))
    }

    protected def be_BY(source: SourceCode, args: (String, SourceCode)): Error = {
      val (specifier, firstOccurrence) = args

      Error(code, source, s"сьпецыфікатар '$specifier' паўтараецца", notes = Seq(
        Note(Some(firstOccurrence), "ў першы раз з'явіўся тут")))
    }

    protected def ru_RU(source: SourceCode, args: (String, SourceCode)): Error = {
      val (specifier, firstOccurrence) = args

      Error(code, source, s"спецификатор '$specifier' повторяется", notes = Seq(
        Note(Some(firstOccurrence), "первое появление здесь")))
    }
  }

  /**
    * Generates an issue for a keyword that is used as an identifier.
    * Args: keyword text
    */
  object KeywordAsIdentifier extends LocaleAwareIssueFactory[Error, String] {
    private val code = "E0009"

    protected def en_US(source: SourceCode, keyword: String): Error = {
      Error(code, source, s"'$keyword' is a keyword, so it can't be used as an identifier", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, keyword: String): Error = {
      Error(code, source, s"'$keyword' - ключавое слова, яно ня можа быць ідэнтыфікатарам", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, keyword: String): Error = {
      Error(code, source, s"'$keyword' - ключевое слово, оно не может быть идентификатором", notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for a parsing error.
    * Args: term describing expected node type
    */
  object MalformedNode extends LocaleAwareIssueFactory[Error, Term] {
    private val code = "E0010"

    protected def en_US(source: SourceCode, nodeDescription: Term): Error = {
      val description = (Adjectives.Valid applyTo nodeDescription).en_US
      Error(code, source, s"tokens don't form ${description.indefinite}", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, nodeDescription: Term): Error = {
      val description = (Adjectives.Valid applyTo nodeDescription).be_BY
      Error(code, source, s"код не з'яўляецца ${description.instrumental}", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, nodeDescription: Term): Error = {
      val description = (Adjectives.Valid applyTo nodeDescription).ru_RU
      Error(code, source, s"код не является ${description.instrumental}", notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for a missing expected node.
    * Args: term describing expected node type
    */
  object MissingNode extends LocaleAwareIssueFactory[Error, Term] {
    private val code = "E0011"

    protected def en_US(source: SourceCode, nodeDescription: Term): Error = {
      val description = nodeDescription.en_US
      Error(code, source, s"missing ${description.noDeterminer}", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, nodeDescription: Term): Error = {
      val description = nodeDescription.be_BY
      Error(code, source, s"не хапае ${description.genitive}", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, nodeDescription: Term): Error = {
      val description = nodeDescription.ru_RU
      Error(code, source, s"не хватает ${description.genitive}", notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for a missing sequence closing token (e.g. ')', '}')
    * Args: (term describing the sequence, term describing expected element type)
    */
  object MissingSequenceClosingElement extends LocaleAwareIssueFactory[Error, (Term, Term)] {
    private val code = "E0012"

    protected def en_US(source: SourceCode, args: (Term, Term)): Error = {
      val (sequence, closingElement) = (args._1.en_US, args._2.en_US)
      Error(code, source, s"missing ${closingElement.noDeterminer} marking the end of ${sequence.definite}",
        notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: (Term, Term)): Error = {
      val (sequence, closingElement) = (args._1.be_BY, args._2.be_BY)
      Error(code, source, s"не хапае ${closingElement.genitive} на канцы ${sequence.genitive}", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: (Term, Term)): Error = {
      val (sequence, closingElement) = (args._1.ru_RU, args._2.ru_RU)
      Error(code, source, s"не хватает ${closingElement.genitive} в конце ${sequence.genitive}", notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for a call with incompatible arguments.
    * Args: (applicable entity type, Seq of argument types)
    */
  object InvalidCallArguments extends LocaleAwareIssueFactory[Error, (Term, Seq[String])] {
    private val code = "E0013"

    protected def en_US(source: SourceCode, args: (Term, Seq[String])): Error = {
      val applicable = args._1.en_US
      val arguments = if (args._2.size == 1) {
        s"an argument of type '${args._2.head}'"
      } else {
        s"arguments of types '${args._2 mkString ", "}'"
      }
      Error(code, source, s"${applicable.definite} can't be applied to $arguments", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: (Term, Seq[String])): Error = {
      val applicable = args._1.be_BY
      val arguments = if (args._2.size == 1) {
        s"аргумента тыпу '${args._2.head}'"
      } else {
        s"аргументаў тыпаў '${args._2 mkString ", "}'"
      }
      Error(code, source, s"немагчыма прымяніць ${applicable.accusative} да $arguments", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: (Term, Seq[String])): Error = {
      val applicable = args._1.ru_RU
      val arguments = if (args._2.size == 1) {
        s"аргументу типа '${args._2.head}'"
      } else {
        s"аргументам типов '${args._2 mkString ", "}'"
      }
      Error(code, source, s"невозможно применить ${applicable.accusative} к $arguments", notes = Seq.empty)
    }
  }

  object ExpressionIsNotCallable extends LocaleAwareIssueFactory[Error, Unit] {
    private val code = "E0014"

    protected def en_US(source: SourceCode, args: Unit): Error = {
      Error(code, source, "the expression is not callable", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: Unit): Error = {
      Error(code, source, "выражэньне немагчыма выклікаць", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: Unit): Error = {
      Error(code, source, "выражение невозможно вызвать", notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for an undefined operator call.
    * Args: (operator name, Seq of argument types)
    */
  object UndefinedOperator extends LocaleAwareIssueFactory[Error, (String, Seq[String])] {
    private val code = "E0015"

    protected def en_US(source: SourceCode, args: (String, Seq[String])): Error = {
      val (operator, argumentTypes) = args
      val types = if (argumentTypes.size == 1) {
        s"type ${argumentTypes.head}"
      } else if (argumentTypes.size == 2) {
        s"types '${argumentTypes.head}' and ${argumentTypes.last}"
      } else throw new IllegalArgumentException("encountered an operator with unexpected arity")

      Error(code, source, s"operator '$operator' is not defined for $types", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: (String, Seq[String])): Error = {
      val (operator, argumentTypes) = args
      val types = if (argumentTypes.size == 1) {
        s"тыпу ${argumentTypes.head}"
      } else if (argumentTypes.size == 2) {
        s"тыпаў '${argumentTypes.head}' і ${argumentTypes.last}"
      } else throw new IllegalArgumentException("encountered an operator with unexpected arity")

      Error(code, source, s"аператар '$operator' не акрэсьлены для $types", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: (String, Seq[String])): Error = {
      val (operator, argumentTypes) = args
      val types = if (argumentTypes.size == 1) {
        s"типа ${argumentTypes.head}"
      } else if (argumentTypes.size == 2) {
        s"типов '${argumentTypes.head}' и ${argumentTypes.last}"
      } else throw new IllegalArgumentException("encountered an operator with unexpected arity")

      Error(code, source, s"оператор '$operator' не определён для $types", notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for an unexpected symbol reference used as an expression.
    * Args: term describing the symbol type
    */
  object InvalidReferenceAsExpression extends LocaleAwareIssueFactory[Error, Term] {
    private val code = "E0016"

    protected def en_US(source: SourceCode, symbolDescription: Term): Error = {
      val symbol = symbolDescription.en_US
      Error(code, source, s"${symbol.indefinite} cannot be an expression", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, symbolDescription: Term): Error = {
      val symbol = symbolDescription.be_BY
      Error(code, source, s"${symbol.nominative} ня можа быць выражэньнем", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, symbolDescription: Term): Error = {
      val symbol = symbolDescription.ru_RU
      Error(code, source, s"${symbol.nominative} не может быть выражением", notes = Seq.empty)
    }
  }

  object UnknownName extends LocaleAwareIssueFactory[Error, Unit] {
    private val code = "E0017"

    protected def en_US(source: SourceCode, args: Unit): Error = {
      Error(code, source, "unknown name", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: Unit): Error = {
      Error(code, source, "невядомае імя", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: Unit): Error = {
      Error(code, source, "неизвестное имя", notes = Seq.empty)
    }
  }

  object NativeFunctionWithBody extends LocaleAwareIssueFactory[Error, Unit] {
    private val code = "E0018"

    protected def en_US(source: SourceCode, args: Unit): Error = {
      Error(code, source, "a 'native' function cannot be defined with a body", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: Unit): Error = {
      Error(code, source, "'native'-функцыя ня можа быць акрэсьлена з целам", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: Unit): Error = {
      Error(code, source, "'native'-функция не может быть определена с телом", notes = Seq.empty)
    }
  }

  object FunctionDefinitionWithoutBody extends LocaleAwareIssueFactory[Error, Unit] {
    private val code = "E0019"

    protected def en_US(source: SourceCode, args: Unit): Error = {
      Error(code, source, "a function cannot be defined without a body", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: Unit): Error = {
      Error(code, source, "функцыя ня можа быць акрэсьлена бяз цела", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: Unit): Error = {
      Error(code, source, "функция не может быть определена без тела", notes = Seq.empty)
    }
  }

  object MissingReturnStatement extends LocaleAwareIssueFactory[Error, Unit] {
    private val code = "E0020"

    protected def en_US(source: SourceCode, args: Unit): Error = {
      Error(code, source, "missing 'return' statement", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: Unit): Error = {
      Error(code, source, "прапушчана інструкцыя 'return'", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: Unit): Error = {
      Error(code, source, "пропущена инструкция 'return'", notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for a return statement with value that is incompatible with the enclosing function
    * return type.
    * Args: (return value type, function return type)
    */
  object IncompatibleFunctionReturnValue extends LocaleAwareIssueFactory[Error, (String, String)] {
    private val code = "E0021"

    protected def en_US(source: SourceCode, args: (String, String)): Error = {
      val (actualType, expectedType) = args
      Error(code, source, s"a value of type '$actualType' cannot be returned from a function returning '$expectedType'",
        notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: (String, String)): Error = {
      val (actualType, expectedType) = args
      Error(code, source, s"немагчыма вярнуць значэньне тыпу '$actualType' з функцыі, якая вяртае '$expectedType'",
        notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: (String, String)): Error = {
      val (actualType, expectedType) = args
      Error(code, source, s"нельзя вернуть значение типа '$actualType' из функции, возвращающей '$expectedType'",
        notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for a 'return-void' statement in a non-void function.
    * Args: function return type
    */
  object FunctionReturnWithoutValue extends LocaleAwareIssueFactory[Error, String] {
    private val code = "E0022"

    protected def en_US(source: SourceCode, returnType: String): Error = {
      Error(code, source, s"must return a value from a function returning '$returnType'", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, returnType: String): Error = {
      Error(code, source, s"из функции, возвращающей '$returnType', необходимо вернуть значение", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, returnType: String): Error = {
      Error(code, source, s"з функцыі, якая вяртае '$returnType', трэба вярнуць значэньне", notes = Seq.empty)
    }
  }

  object UnreachableCode extends LocaleAwareIssueFactory[Error, Unit] {
    private val code = "E0023"

    protected def en_US(source: SourceCode, args: Unit): Error = {
      Error(code, source, "code after 'return' statement will never be executed", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: Unit): Error = {
      Error(code, source, "код пасьля інструкцыі 'return' ніколі ня будзе выкананы", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: Unit): Error = {
      Error(code, source, "код после инструкции 'return' никогда не будет исполнен", notes = Seq.empty)
    }
  }

  object TypeDefinitionWithoutBody extends LocaleAwareIssueFactory[Error, Unit] {
    private val code = "E0024"

    protected def en_US(source: SourceCode, args: Unit): Error = {
      Error(code, source, "a type cannot be defined without a body", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: Unit): Error = {
      Error(code, source, "тып ня можа быць акрэсьлены бяз цела", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: Unit): Error = {
      Error(code, source, "тип не может быть определён без тела", notes = Seq.empty)
    }
  }

  object InvalidMainFunctionSignature extends LocaleAwareIssueFactory[Error, Unit] {
    private val code = "E0025"

    protected def en_US(source: SourceCode, args: Unit): Error = {
      Error(code, source, "'main' function must accept no parameters and return 'void'", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: Unit): Error = {
      Error(code, source, "функцыя 'main' павінна ня прымаць параметраў і вяртаць 'void'", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: Unit): Error = {
      Error(code, source, "функция 'main' должна не принимать параметров и возвращать 'void'", notes = Seq.empty)
    }
  }

  object MainIsNotFunction extends LocaleAwareIssueFactory[Error, Unit] {
    private val code = "E0026"

    protected def en_US(source: SourceCode, args: Unit): Error = {
      Error(code, source, "'main' must be a function", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: Unit): Error = {
      Error(code, source, "'main' павінна быць функцыяй", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: Unit): Error = {
      Error(code, source, "'main' должна быть функцией", notes = Seq.empty)
    }
  }

  object MissingMainFunction extends LocaleAwareIssueFactory[Error, Unit] {
    private val code = "E0027"

    protected def en_US(source: SourceCode, args: Unit): Error = {
      Error(code, source, "missing 'main' function", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: Unit): Error = {
      Error(code, source, "няма функцыі 'main'", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: Unit): Error = {
      Error(code, source, "нет функции 'main'", notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for a variable initializer of different type than the variable itself.
    * Args: (initializer type, variable type)
    */
  object IncompatibleVariableInitializer extends LocaleAwareIssueFactory[Error, (String, String)] {
    private val code = "E0028"

    protected def en_US(source: SourceCode, args: (String, String)): Error = {
      val (initializerType, variableType) = args
      Error(code, source, s"initializer of type '$initializerType' is incompatible with " +
        s"the variable of type '$variableType'", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: (String, String)): Error = {
      val (initializerType, variableType) = args
      Error(code, source, s"ініцыялізатар тыпу '$initializerType' нясумяшчальны са зьменнай тыпу '$variableType'",
        notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: (String, String)): Error = {
      val (initializerType, variableType) = args
      Error(code, source, s"инициализатор типа '$initializerType' несовместим с переменной типа '$variableType'",
        notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for a non-bool condition.
    * Args: (statement name, condition type)
    */
  object InvalidConditionType extends LocaleAwareIssueFactory[Error, (String, String)] {
    private val code = "E0029"

    protected def en_US(source: SourceCode, args: (String, String)): Error = {
      val (statement, conditionType) = args
      Error(code, source, s"'$statement' statement condition must have type 'bool', but has type '$conditionType'",
        notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: (String, String)): Error = {
      val (statement, conditionType) = args
      Error(code, source, s"умова інструкцыі '$statement' павінна мець тып 'bool', але мае тып '$conditionType'",
        notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: (String, String)): Error = {
      val (statement, conditionType) = args
      Error(code, source, s"условие инструкции '$statement' должно быть типа 'bool', а не '$conditionType'",
        notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for a duplicate function definition (same name and parameter types)
    * Args: optionally definition site of the original function
    */
  object DuplicateFunctionDefinition extends LocaleAwareIssueFactory[Error, Option[SourceCode]] {
    private val code = "E0030"

    protected def en_US(source: SourceCode, originalOption: Option[SourceCode]): Error = {
      val notes = originalOption match {
        case Some(originalDefinition) => Seq(Note(Some(originalDefinition), "defined here"))
        case None => Seq.empty
      }

      Error(code, source, "there is already a function with the same name and parameter types in the current scope",
        notes)
    }

    protected def be_BY(source: SourceCode, originalOption: Option[SourceCode]): Error = {
      val notes = originalOption match {
        case Some(originalDefinition) => Seq(Note(Some(originalDefinition), "акрэсьленая тут"))
        case None => Seq.empty
      }

      Error(code, source, "у гэтай прасторы імёнаў ужо ёсьць функцыя з такімі самымі імем і тыпамі параметраў", notes)
    }

    protected def ru_RU(source: SourceCode, originalOption: Option[SourceCode]): Error = {
      val notes = originalOption match {
        case Some(originalDefinition) => Seq(Note(Some(originalDefinition), "определённая здесь"))
        case None => Seq.empty
      }

      Error(code, source, "в этом пространстве имён уже есть функция с такими же именем и типами параметров", notes)
    }
  }

  /**
    * Generates an issue for an ambiguous overloaded applicable entity call.
    * Args: (applicable entity type, Seq of notes: use note methods to generate them)
    */
  object AmbiguousOverloadedCall extends LocaleAwareIssueFactory[Error, (Term, Seq[Note])] {
    private val code = "E0031"

    protected def en_US(source: SourceCode, args: (Term, Seq[Note])): Error = {
      val applicable = args._1.en_US
      val notes = args._2
      Error(code, source, s"the overloaded ${applicable.noDeterminer} call is ambiguous", notes)
    }

    protected def be_BY(source: SourceCode, args: (Term, Seq[Note])): Error = {
      val applicable = (Adjectives.Overloaded applyTo args._1).be_BY
      val notes = args._2
      Error(code, source, s"выклік ${applicable.genitive} можна трактаваць па-рознаму", notes)
    }

    protected def ru_RU(source: SourceCode, args: (Term, Seq[Note])): Error = {
      val applicable = (Adjectives.Overloaded applyTo args._1).ru_RU
      val notes = args._2
      Error(code, source, s"вызов ${applicable.genitive} можно понимать по-разному", notes)
    }

    def note(signature: String, definition: Option[SourceCode]): Note = {
      val text = Locale.getDefault.getLanguage match {
        case "en" => s"it can mean a call to '$signature'"
        case "be" => s"як выклік '$signature'"
        case "ru" => s"как вызов '$signature'"
      }

      Note(definition, text)
    }
  }

  /**
    * Generates an issue for a symbol definition with a taken name.
    * Args: (term describing original symbol, optionally its definition site)
    */
  object SymbolNameTaken extends LocaleAwareIssueFactory[Error, (Term, Option[SourceCode])] {
    private val code = "E0032"

    protected def en_US(source: SourceCode, args: (Term, Option[SourceCode])): Error = {
      val symbol = args._1.en_US
      val notes = args._2 match {
        case Some(definitionSite) => Seq(Note(Some(definitionSite), "defined here"))
        case None => Seq.empty
      }

      Error(code, source, s"this name is already used by ${symbol.indefinite} in the current scope", notes)
    }

    protected def be_BY(source: SourceCode, args: (Term, Option[SourceCode])): Error = {
      val symbol = args._1.be_BY
      val notes = args._2 match {
        case Some(definitionSite) => Seq(Note(Some(definitionSite), "акрэсьленьне - тут"))
        case None => Seq.empty
      }

      Error(code, source, s"у гэтай прасторы імёнаў ужо ёсьць ${symbol.nominative} з гэтым імем", notes)
    }

    protected def ru_RU(source: SourceCode, args: (Term, Option[SourceCode])): Error = {
      val symbol = args._1.ru_RU
      val notes = args._2 match {
        case Some(definitionSite) => Seq(Note(Some(definitionSite), "определение - здесь"))
        case None => Seq.empty
      }

      Error(code, source, s"в этом пространстве имён уже есть ${symbol.nominative} с этим именем", notes)
    }
  }

  /**
    * Generates an issue for a duplicate method definition (same name).
    * Args: optionally definition site of the original method
    */
  object DuplicateMethodDefinition extends LocaleAwareIssueFactory[Error, Option[SourceCode]] {
    private val code = "E0033"

    protected def en_US(source: SourceCode, originalOption: Option[SourceCode]): Error = {
      val notes = originalOption match {
        case Some(originalDefinition) => Seq(Note(Some(originalDefinition), "defined here"))
        case None => Seq.empty
      }

      Error(code, source, "there is already a method with the same name for this type", notes)
    }

    protected def be_BY(source: SourceCode, originalOption: Option[SourceCode]): Error = {
      val notes = originalOption match {
        case Some(originalDefinition) => Seq(Note(Some(originalDefinition), "акрэсьлены тут"))
        case None => Seq.empty
      }

      Error(code, source, "для гэтага тыпу ужо акрэсьлены метад з такім самым імем", notes)
    }

    protected def ru_RU(source: SourceCode, originalOption: Option[SourceCode]): Error = {
      val notes = originalOption match {
        case Some(originalDefinition) => Seq(Note(Some(originalDefinition), "определённый здесь"))
        case None => Seq.empty
      }

      Error(code, source, "для этого типа уже определён метод с таким же именем", notes)
    }
  }

  /**
    * Generates an issue for an unexpected symbol reference used as a type.
    * Args: term describing the symbol type
    */
  object InvalidReferenceAsType extends LocaleAwareIssueFactory[Error, Term] {
    private val code = "E0034"

    protected def en_US(source: SourceCode, symbolDescription: Term): Error = {
      val symbol = symbolDescription.en_US
      Error(code, source, s"${symbol.indefinite} cannot be a type", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, symbolDescription: Term): Error = {
      val symbol = symbolDescription.be_BY
      Error(code, source, s"${symbol.nominative} ня можа быць тыпам", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, symbolDescription: Term): Error = {
      val symbol = symbolDescription.ru_RU
      Error(code, source, s"${symbol.nominative} не может быть типом", notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for an overreferenced type (like 'int&&').
    * Args: referenced type name (in the above case: 'int&')
    */
  object OverreferencedType extends LocaleAwareIssueFactory[Error, String] {
    private val code = "E0035"

    protected def en_US(source: SourceCode, referencedType: String): Error = {
      Error(code, source, s"type '$referencedType', which is itself a reference, cannot be referenced again.",
        notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, referencedType: String): Error = {
      Error(code, source, s"нельга спасылацца на тып '$referencedType', які ўжо з'яўляецца спасылкай.",
        notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, referencedType: String): Error = {
      Error(code, source, s"нельзя ссылаться на тип '$referencedType', который сам по себе является ссылкой.",
        notes = Seq.empty)
    }
  }

  /**
    * Generates an issue for a duplicate constructor definition (same parameter types).
    * Args: optionally definition site of the original constructor
    */
  object DuplicateConstructorDefinition extends LocaleAwareIssueFactory[Error, Option[SourceCode]] {
    private val code = "E0036"

    protected def en_US(source: SourceCode, originalOption: Option[SourceCode]): Error = {
      val notes = originalOption match {
        case Some(originalDefinition) => Seq(Note(Some(originalDefinition), "defined here"))
        case None => Seq.empty
      }

      Error(code, source, "there is already a constructor with the same parameter types for this type",
        notes)
    }

    protected def be_BY(source: SourceCode, originalOption: Option[SourceCode]): Error = {
      val notes = originalOption match {
        case Some(originalDefinition) => Seq(Note(Some(originalDefinition), "акрэсьлены тут"))
        case None => Seq.empty
      }

      Error(code, source, "для гэтага тыпу ўжо акрэсьлены канструктар з такімі самымі тыпамі параметраў", notes)
    }

    protected def ru_RU(source: SourceCode, originalOption: Option[SourceCode]): Error = {
      val notes = originalOption match {
        case Some(originalDefinition) => Seq(Note(Some(originalDefinition), "определённый здесь"))
        case None => Seq.empty
      }

      Error(code, source, "для этого типа уже определён конструктор с такими же типами параметров", notes)
    }
  }
}
