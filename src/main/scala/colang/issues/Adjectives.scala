package colang.issues

import java.util.Locale

/**
  * A common trait for all locale-specific adjective classes.
  */
trait Adjective

class EnglishAdjective(val text: String) extends Adjective

/**
  * Belarusian adjective forms are declined by cases, genders, and plurality. The constructor accepts a single string
  * literal that contains all forms, see below for usage examples.
  * @param formsTable a table containing all adjective forms
  */
class BelarusianAdjective(formsTable: String) extends Adjective {
  private val formsArray = formsTable.split("\\s+")

  def nominative    = new PartialFormResult(0)
  def genitive      = new PartialFormResult(4)
  def dative        = new PartialFormResult(8)
  def accusative    = new PartialFormResult(12)
  def instrumental  = new PartialFormResult(16)
  def prepositional = new PartialFormResult(20)

  class PartialFormResult(offset: Int) {

    def masculine = formsArray(offset + 0)
    def feminine  = formsArray(offset + 1)
    def neuter    = formsArray(offset + 2)
    def plural    = formsArray(offset + 3)
  }
}

/**
  * Russian adjective forms are declined by cases, genders, and plurality. The constructor accepts a single string
  * literal that contains all forms, see below for usage examples.
  * @param formsTable a table containing all adjective forms
  */
class RussianAdjective(formsTable: String) extends Adjective {
  private val formsArray = formsTable.split("\\s+")

  def nominative    = new PartialFormResult(0)
  def genitive      = new PartialFormResult(4)
  def dative        = new PartialFormResult(8)
  def accusative    = new PartialFormResult(12)
  def instrumental  = new PartialFormResult(16)
  def prepositional = new PartialFormResult(20)

  class PartialFormResult(offset: Int) {

    def masculine = formsArray(offset + 0)
    def feminine  = formsArray(offset + 1)
    def neuter    = formsArray(offset + 2)
    def plural    = formsArray(offset + 3)
  }
}

/**
  * Adjective factories implement this trait by defining locale-specific generation methods. The apply() method is
  * inherited.
  */
trait LocaleAwareAdjectiveFactory {
  protected def en_US: EnglishAdjective
  protected def be_BY: BelarusianAdjective
  protected def ru_RU: RussianAdjective

  def apply(): Adjective = {
    Locale.getDefault.getLanguage match {
      case "en" => en_US
      case "be" => be_BY
      case "ru" => ru_RU
    }
  }
}

object Adjectives {

  object Big extends LocaleAwareAdjectiveFactory {
    def en_US = new EnglishAdjective("big")

    def be_BY = new BelarusianAdjective(
      """вялікі   вялікая вялікае  вялікія
        |вялікага вялікай вялікага вялікіх
        |вялікаму вялікай вялікаму вялікім
        |вялікі   вялікую вялікае  вялікія
        |вялікім  вялікай вялікім  вялікімі
        |вялікім  вялікай вялікім  вялікіх
      """.stripMargin)

    def ru_RU = new RussianAdjective(
      """большой  большая большое  большие
        |большого большой большого больших
        |большому большой большому большим
        |большой  большую большое  большие
        |большим  большой большим  большими
        |большом  большой большом  больших
      """.stripMargin)
  }

  object Small extends LocaleAwareAdjectiveFactory {
    def en_US = new EnglishAdjective("small")

    def be_BY = new BelarusianAdjective(
      """малы   малая малое  малыя
        |малога малой малога малых
        |малому малой малому малым
        |малы   малую малое  малыя
        |малым  малой малым  малымі
        |малым  малой малым  малых
      """.stripMargin)

    def ru_RU = new RussianAdjective(
      """маленький  маленькая маленькое  маленькие
        |маленького маленькой маленького маленьких
        |маленькому маленькой маленькому маленьким
        |маленький  маленькую маленькое  маленькие
        |маленьким  маленькой маленьким  маленькими
        |маленьком  маленькой маленьком  маленьких
      """.stripMargin)
  }
}
