package colang.issues

import java.util.Locale

import colang.SourceCode

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
}
