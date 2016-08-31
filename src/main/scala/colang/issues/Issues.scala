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

  /**
    * Generates an issue for numeric literal of unknown format.
    */
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

  /**
    * Generates an issue for an unknown token.
    */
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
    * Args: (specifier text, term describing context)
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
      val verb = Verbs.IsMissing.en_US(description)

      Error(code, source, s"${description.definite} $verb", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, nodeDescription: Term): Error = {
      val description = nodeDescription.be_BY
      val verb = Verbs.IsMissing.be_BY(description)

      Error(code, source, s"$verb ${description.nominative}", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, nodeDescription: Term): Error = {
      val description = nodeDescription.ru_RU
      val verb = Verbs.IsMissing.ru_RU(description)

      Error(code, source, s"$verb ${description.nominative}", notes = Seq.empty)
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
      val verb = Verbs.IsMissing.en_US(closingElement)

      Error(code, source, s"${closingElement.noDeterminer} marking the end of ${sequence.definite} $verb",
        notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: (Term, Term)): Error = {
      val (sequence, closingElement) = (args._1.be_BY, args._2.be_BY)
      val verb = Verbs.IsMissing.be_BY(closingElement)

      Error(code, source, s"$verb ${closingElement.nominative} на канцы ${sequence.genitive}", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: (Term, Term)): Error = {
      val (sequence, closingElement) = (args._1.ru_RU, args._2.ru_RU)
      val verb = Verbs.IsMissing.ru_RU(closingElement)

      Error(code, source, s"$verb ${closingElement.nominative} в конце ${sequence.genitive}", notes = Seq.empty)
    }
  }
}
