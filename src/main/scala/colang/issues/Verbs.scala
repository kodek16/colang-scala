package colang.issues

class EnglishVerb(val singular: String, val plural: String) {
  def apply(term: EnglishTerm): String = {
    term.number match {
      case EnglishTerm.Singular => singular
      case EnglishTerm.Plural => plural
    }
  }
}

class BelarusianVerb(val masculine: String, val feminine: String, val neuter: String, val plural: String) {
  def apply(term: BelarusianTerm): String = {
    term.gender match {
      case BelarusianTerm.Masculine => masculine
      case BelarusianTerm.Feminine => feminine
      case BelarusianTerm.Neuter => neuter
      case BelarusianTerm.Plural => plural
    }
  }
}

class RussianVerb(val masculine: String, val feminine: String, val neuter: String, val plural: String) {
  def apply(term: RussianTerm): String = {
    term.gender match {
      case RussianTerm.Masculine => masculine
      case RussianTerm.Feminine => feminine
      case RussianTerm.Neuter => neuter
      case RussianTerm.Plural => plural
    }
  }
}

object Verbs {
  object IsMissing {
    val en_US = new EnglishVerb("is missing", "are missing")
    val be_BY = new BelarusianVerb("прапушчаны", "прапушчана", "прапушчана", "прапушчаныя")
    val ru_RU = new RussianVerb("пропущен", "пропущена", "пропущено", "пропущены")
  }
}