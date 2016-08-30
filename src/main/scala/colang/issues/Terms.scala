package colang.issues

/**
  * A sum of all locale-specific term types.
  */
abstract class Term {

  def en_US: EnglishTerm
  def be_BY: BelarusianTerm
  def ru_RU: RussianTerm

  /**
    * Combines two terms with 'or'
    * @param what right term ('B' in 'A of B')
    * @return combined term
    */
  def of(what: Term): Term = {
    val self = this

    new Term {
      lazy val en_US = self.en_US of what.en_US
      lazy val be_BY = self.be_BY of what.be_BY
      lazy val ru_RU = self.ru_RU of what.ru_RU
    }
  }
}

/**
  * English terms may require a determiner in some context. This class supports three forms:
  * no determiner, indefinite form ('a'/'an' for singular) and definite form ('the')
  * @param noDeterminer no-determiner form
  * @param indefinite indefinite form
  */
class EnglishTerm(val noDeterminer: String,
                  val indefinite: String) {

  def definite = "the " + noDeterminer

  def of(what: EnglishTerm): EnglishTerm = {
      new EnglishTerm(
        what.noDeterminer + " " + this.noDeterminer,
        what.indefinite   + " " + this.noDeterminer)
  }
}

/**
  * Belarusian terms are declined by cases.
  */
class BelarusianTerm(val nominative: String,
                     val genitive: String,
                     val dative: String,
                     val accusative: String,
                     val instrumental: String,
                     val prepositional: String,
                     val gender: BelarusianTerm.Gender) {

  def of(what: BelarusianTerm): BelarusianTerm = {
    new BelarusianTerm(
      this.nominative     + " " + what.genitive,
      this.genitive       + " " + what.genitive,
      this.dative         + " " + what.genitive,
      this.accusative     + " " + what.genitive,
      this.instrumental   + " " + what.genitive,
      this.prepositional  + " " + what.genitive,
      this.gender)
  }
}

object BelarusianTerm {

  /**
    * Describes what adjective form should be used with a noun. 'Plural' isn't really a gender, but as for adjective
    * forms it can be treated as such.
    */
  sealed trait Gender
  case object Masculine extends Gender
  case object Feminine extends Gender
  case object Neuter extends Gender
  case object Plural extends Gender
}

/**
  * Russian terms are declined by cases.
  */
class RussianTerm(val nominative: String,
                  val genitive: String,
                  val dative: String,
                  val accusative: String,
                  val instrumental: String,
                  val prepositional: String,
                  val gender: RussianTerm.Gender) {

  def of(what: RussianTerm): RussianTerm = {
    new RussianTerm(
      this.nominative     + " " + what.genitive,
      this.genitive       + " " + what.genitive,
      this.dative         + " " + what.genitive,
      this.accusative     + " " + what.genitive,
      this.instrumental   + " " + what.genitive,
      this.prepositional  + " " + what.genitive,
      this.gender)
  }
}

object RussianTerm {

  /**
    * Describes what adjective form should be used with a noun. 'Plural' isn't really a gender, but as for adjective
    * forms it can be treated as such.
    */
  sealed trait Gender
  case object Masculine extends Gender
  case object Feminine extends Gender
  case object Neuter extends Gender
  case object Plural extends Gender
}

object Terms {

  object Function extends Term {
    val en_US = new EnglishTerm("function", "a function")

    val be_BY = new BelarusianTerm(
      "функцыя",
      "функцыі",
      "функцыі",
      "функцыю",
      "функцыяй",
      "функцыі",
      BelarusianTerm.Feminine)

    val ru_RU = new RussianTerm(
      "функция",
      "функции",
      "функции",
      "функцию",
      "функцией",
      "функции",
      RussianTerm.Feminine)
  }

  object Type extends Term {
    val en_US = new EnglishTerm("type", "a type")

    val be_BY = new BelarusianTerm(
      "тып",
      "тыпу",
      "тыпу",
      "тып",
      "тыпам",
      "тыпе",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "тип",
      "типа",
      "типу",
      "тип",
      "типом",
      "типе",
      RussianTerm.Masculine)
  }

  object Definition extends Term {
    val en_US = new EnglishTerm("definition", "a definition")

    val be_BY = new BelarusianTerm(
      "акрэсьленьне",
      "акрэсьленьня",
      "акрэсьленьню",
      "акрэсьленьне",
      "акрэсьленьнем",
      "акрэсьленьні",
      BelarusianTerm.Neuter)

    val ru_RU = new RussianTerm(
      "определение",
      "определения",
      "определению",
      "определение",
      "определением",
      "определении",
      RussianTerm.Neuter)
  }
}
