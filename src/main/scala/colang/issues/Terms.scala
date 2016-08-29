package colang.issues

import java.util.Locale

/**
  * A common trait for all locale-specific term traits.
  * A term is a noun or a noun phrase, either singular or plural.
  */
trait Term

/**
  * English terms may require a determiner in some context. This class supports three forms:
  * no determiner, indefinite form ('a'/'an' for singular) and definite form ('the')
  * @param noDeterminer no-determiner form
  * @param indefinite indefinite form
  */
class EnglishTerm(val noDeterminer: String,
                  val indefinite: String) extends Term {

  def definite = "the " + noDeterminer
}

/**
  * Belarusian terms are declined by cases.
  */
class BelarusianTerm(val nominative: String,
                     val genitive: String,
                     val dative: String,
                     val accusative: String,
                     val instrumental: String,
                     val prepositional: String) extends Term

/**
  * Russian terms are declined by cases.
  */
class RussianTerm(val nominative: String,
                  val genitive: String,
                  val dative: String,
                  val accusative: String,
                  val instrumental: String,
                  val prepositional: String) extends Term

/**
  * Term factories implement this trait by defining locale-specific generation methods. The apply() method is
  * inherited.
  */
trait LocaleAwareTermFactory {
  protected def en_US: EnglishTerm
  protected def be_BY: BelarusianTerm
  protected def ru_RU: RussianTerm

  def apply(): Term = {
    Locale.getDefault.getLanguage match {
      case "en" => en_US
      case "be" => be_BY
      case "ru" => ru_RU
    }
  }
}

object Terms {

  object Function extends LocaleAwareTermFactory {
    def en_US = new EnglishTerm("function", "a function")

    def be_BY = new BelarusianTerm(
      "функцыя",
      "функцыі",
      "функцыі",
      "функцыю",
      "функцыяй",
      "функцыі")

    def ru_RU = new RussianTerm(
      "функция",
      "функции",
      "функции",
      "функцию",
      "функцией",
      "функции")
  }

  object Definition extends LocaleAwareTermFactory {
    def en_US = new EnglishTerm("definition", "a definition")

    def be_BY = new BelarusianTerm(
      "акрэсьленьне",
      "акрэсьленьня",
      "акрэсьленьню",
      "акрэсьленьне",
      "акрэсьленьнем",
      "акрэсьленьні")

    def ru_RU = new RussianTerm(
      "определение",
      "определения",
      "определению",
      "определение",
      "определением",
      "определении")
  }

  val FunctionDefinition = Definition of Function

  /**
    * Combines two terms with 'or'. While the implementation is probably correct for most cases, some exceptions may
    * eventually appear, so this method is private for additional safety. If you need to use it, expose a new factory
    * (e.g. FunctionDefinition above).
    */
  private implicit class OfTerm(leftTerm: LocaleAwareTermFactory) {
    def of(rightTerm: LocaleAwareTermFactory) = new LocaleAwareTermFactory {
      protected def en_US = {
        (leftTerm(), rightTerm()) match {
          case (enLeft: EnglishTerm, enRight: EnglishTerm) =>
            new EnglishTerm(
              enRight.noDeterminer + " " + enLeft.noDeterminer,
              enRight.indefinite   + " " + enLeft.noDeterminer)
        }
      }

      protected def be_BY = {
        (leftTerm(), rightTerm()) match {
          case (beLeft: BelarusianTerm, beRight: BelarusianTerm) =>
            new BelarusianTerm(
              beLeft.nominative     + " " + beRight.genitive,
              beLeft.genitive       + " " + beRight.genitive,
              beLeft.dative         + " " + beRight.genitive,
              beLeft.accusative     + " " + beRight.genitive,
              beLeft.instrumental   + " " + beRight.genitive,
              beLeft.prepositional  + " " + beRight.genitive)
        }
      }

      protected def ru_RU = {
        (leftTerm(), rightTerm()) match {
          case (ruLeft: RussianTerm, ruRight: RussianTerm) =>
            new RussianTerm(
              ruLeft.nominative     + " " + ruRight.genitive,
              ruLeft.genitive       + " " + ruRight.genitive,
              ruLeft.dative         + " " + ruRight.genitive,
              ruLeft.accusative     + " " + ruRight.genitive,
              ruLeft.instrumental   + " " + ruRight.genitive,
              ruLeft.prepositional  + " " + ruRight.genitive)
        }
      }
    }
  }
}
