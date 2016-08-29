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
      val (relation, typeName) = args match {
        case (adj: EnglishAdjective, name) => (adj.text, name)
      }

      Error(code, source, s"the literal value is too $relation for type '$typeName'", notes = Seq.empty)
    }

    protected def be_BY(source: SourceCode, args: (Adjective, String)): Error = {
      val (relation, typeName) = args match {
        case (adj: BelarusianAdjective, name) => (adj.nominative.neuter, name)
      }
      Error(code, source, s"значэньне канстанты занадта $relation для тыпу '$typeName'", notes = Seq.empty)
    }

    protected def ru_RU(source: SourceCode, args: (Adjective, String)): Error = {
      val (relation, typeName) = args match {
        case (adj: RussianAdjective, name) => (adj.nominative.neuter, name)
      }
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
}
