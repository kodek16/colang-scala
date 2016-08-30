package colang.issues

/**
  * A sum of all locale-specific adjective types.
  */
abstract class Adjective {

  def en_US: EnglishAdjective
  def be_BY: BelarusianAdjective
  def ru_RU: RussianAdjective

  /**
    * Modifies the term by adding the adjective to it.
    * @param term term to modify
    * @return new extended term
    */
  def applyTo(term: Term): Term = {
    val self = this

    new Term {
      lazy val en_US = self.en_US applyTo term.en_US
      lazy val be_BY = self.be_BY applyTo term.be_BY
      lazy val ru_RU = self.ru_RU applyTo term.ru_RU
    }
  }
}

class EnglishAdjective(val text: String) {

  /**
    * Prepends the adjective to the term.
    * @param term term to modify
    * @return new extended term
    */
  def applyTo(term: EnglishTerm): EnglishTerm = {
    val indefiniteForm = if ((term.indefinite startsWith "a ") || (term.indefinite startsWith "an ")) {
      val article = text.charAt(0) match {
        case 'a' | 'i' | 'u' | 'e' | 'o' => "an"
        case _ => "a"
      }

      article + " " + this.text + " " + (term.indefinite split " " drop 1 mkString " ")
    } else {
      this.text + term.indefinite
    }

    new EnglishTerm(this.text + " " + term.noDeterminer, indefiniteForm)
  }
}

/**
  * Belarusian adjective forms are declined by cases, genders, and plurality. The constructor accepts a single string
  * literal that contains all forms, see below for usage examples.
  * @param formsTable a table containing all adjective forms
  */
class BelarusianAdjective(formsTable: String) {
  private val formsArray = formsTable.split("\\s+")

  def masculine = new GenderSpecificAdjective(0)
  def feminine  = new GenderSpecificAdjective(1)
  def neuter    = new GenderSpecificAdjective(2)
  def plural    = new GenderSpecificAdjective(3)

  class GenderSpecificAdjective(offset: Int) {
    def nominative    = formsArray(0 + offset)
    def genitive      = formsArray(4 + offset)
    def dative        = formsArray(8 + offset)
    def accusative    = formsArray(12 + offset)
    def instrumental  = formsArray(16 + offset)
    def prepositional = formsArray(20 + offset)
  }

  /**
    * Prepends the adjective to the term.
    * @param term term to modify
    * @return new extended term
    */
  def applyTo(term: BelarusianTerm): BelarusianTerm = {
    val genderSpecific = term.gender match {
      case BelarusianTerm.Masculine => masculine
      case BelarusianTerm.Feminine => feminine
      case BelarusianTerm.Neuter => neuter
      case BelarusianTerm.Plural => plural
    }

    new BelarusianTerm(
      genderSpecific.nominative     + " " + term.nominative,
      genderSpecific.genitive       + " " + term.genitive,
      genderSpecific.dative         + " " + term.dative,
      genderSpecific.accusative     + " " + term.accusative,
      genderSpecific.instrumental   + " " + term.instrumental,
      genderSpecific.prepositional  + " " + term.prepositional,
      term.gender)
  }
}

/**
  * Russian adjective forms are declined by cases, genders, and plurality. The constructor accepts a single string
  * literal that contains all forms, see below for usage examples.
  * @param formsTable a table containing all adjective forms
  */
class RussianAdjective(formsTable: String) {
  private val formsArray = formsTable.split("\\s+")

  def masculine = new GenderSpecificAdjective(0)
  def feminine  = new GenderSpecificAdjective(1)
  def neuter    = new GenderSpecificAdjective(2)
  def plural    = new GenderSpecificAdjective(3)

  class GenderSpecificAdjective(offset: Int) {
    def nominative    = formsArray(0 + offset)
    def genitive      = formsArray(4 + offset)
    def dative        = formsArray(8 + offset)
    def accusative    = formsArray(12 + offset)
    def instrumental  = formsArray(16 + offset)
    def prepositional = formsArray(20 + offset)
  }

  /**
    * Prepends the adjective to the term.
    * @param term term to modify
    * @return new extended term
    */
  def applyTo(term: RussianTerm): RussianTerm = {
    val genderSpecific = term.gender match {
      case RussianTerm.Masculine => masculine
      case RussianTerm.Feminine => feminine
      case RussianTerm.Neuter => neuter
      case RussianTerm.Plural => plural
    }

    new RussianTerm(
      genderSpecific.nominative     + " " + term.nominative,
      genderSpecific.genitive       + " " + term.genitive,
      genderSpecific.dative         + " " + term.dative,
      genderSpecific.accusative     + " " + term.accusative,
      genderSpecific.instrumental   + " " + term.instrumental,
      genderSpecific.prepositional  + " " + term.prepositional,
      term.gender)
  }
}

object Adjectives {

  object Big extends Adjective {
    val en_US = new EnglishAdjective("big")

    val be_BY = new BelarusianAdjective(
      """вялікі   вялікая вялікае  вялікія
        |вялікага вялікай вялікага вялікіх
        |вялікаму вялікай вялікаму вялікім
        |вялікі   вялікую вялікае  вялікія
        |вялікім  вялікай вялікім  вялікімі
        |вялікім  вялікай вялікім  вялікіх
      """.stripMargin)

    val ru_RU = new RussianAdjective(
      """большой  большая большое  большие
        |большого большой большого больших
        |большому большой большому большим
        |большой  большую большое  большие
        |большим  большой большим  большими
        |большом  большой большом  больших
      """.stripMargin)
  }

  object Small extends Adjective {
    val en_US = new EnglishAdjective("small")

    val be_BY = new BelarusianAdjective(
      """малы   малая малое  малыя
        |малога малой малога малых
        |малому малой малому малым
        |малы   малую малое  малыя
        |малым  малой малым  малымі
        |малым  малой малым  малых
      """.stripMargin)

    val ru_RU = new RussianAdjective(
      """маленький  маленькая маленькое  маленькие
        |маленького маленькой маленького маленьких
        |маленькому маленькой маленькому маленьким
        |маленький  маленькую маленькое  маленькие
        |маленьким  маленькой маленьким  маленькими
        |маленьком  маленькой маленьком  маленьких
      """.stripMargin)
  }

  object Valid extends Adjective {
    val en_US = new EnglishAdjective("valid")

    val be_BY = new BelarusianAdjective(
      """карэктны   карэктная карэктнае  карэктныя
        |карэктнага карэктнай карэктнага карэктных
        |карэктнаму карэктнай карэктнаму карэктным
        |карэктны   карэктную карэктнае  карэктныя
        |карэктным  карэктнай карэктным  карэктнымі
        |карэктным  карэктнай карэктным  карэктных
      """.stripMargin)

    val ru_RU = new RussianAdjective(
      """корректный  корректная корректное  корректные
        |корректного корректной корректного корректных
        |корректному корректной корректному корректным
        |корректный  корректную корректное  корректные
        |корректным  корректной корректным  корректными
        |корректном  корректной корректном  корректных
      """.stripMargin)
  }
}
