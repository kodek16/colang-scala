package colang.issues

/**
  * A product of all locale-specific term types.
  */
abstract class Term {

  def en_US: EnglishTerm
  def be_BY: BelarusianTerm
  def ru_RU: RussianTerm

  /**
    * Combines two terms with 'of'
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

  /**
    * Combines two terms with 'in'
    * @param what right term ('B' in 'A in B')
    * @return combined term
    */
  def in(what: Term): Term = {
    val self = this

    new Term {
      lazy val en_US = self.en_US in what.en_US
      lazy val be_BY = self.be_BY in what.be_BY
      lazy val ru_RU = self.ru_RU in what.ru_RU
    }
  }
}

/**
  * English terms may require a determiner in some context. This class supports three forms:
  * no determiner, indefinite form ('a'/'an' for singular) and definite form ('the').
  * @param noDeterminer no-determiner form
  * @param number grammatical number
  */
class EnglishTerm(val noDeterminer: String,
                  val number: EnglishTerm.Number) {

  lazy val indefinite = {
    number match {
      case EnglishTerm.Singular =>
        val article = noDeterminer.charAt(0) match {
          case 'a' | 'i' | 'e' | 'u' | 'o' => "an"
          case _ => "a"
        }

        article + " " + noDeterminer

      case EnglishTerm.Plural => noDeterminer
    }
  }

  def definite = "the " + noDeterminer

  def of(what: EnglishTerm): EnglishTerm = {
    new EnglishTerm(
      what.noDeterminer + " " + this.noDeterminer,
      this.number)
  }

  def in(what: EnglishTerm): EnglishTerm = {
    val self = this

    new EnglishTerm(self.noDeterminer + " in " + what.noDeterminer, self.number) {
      override lazy val indefinite: String = self.indefinite + " in " + what.indefinite
      override def definite: String = self.definite + " in " + what.definite
    }
  }
}

object EnglishTerm {

  /**
    * Describes what adjective and verb forms should be used with a noun.
    */
  sealed trait Number
  case object Singular extends Number
  case object Plural extends Number
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
      BelarusianTerm.addShortUWhereNecessary(this.nominative     + " " + what.genitive),
      BelarusianTerm.addShortUWhereNecessary(this.genitive       + " " + what.genitive),
      BelarusianTerm.addShortUWhereNecessary(this.dative         + " " + what.genitive),
      BelarusianTerm.addShortUWhereNecessary(this.accusative     + " " + what.genitive),
      BelarusianTerm.addShortUWhereNecessary(this.instrumental   + " " + what.genitive),
      BelarusianTerm.addShortUWhereNecessary(this.prepositional  + " " + what.genitive),
      this.gender)
  }

  def in(what: BelarusianTerm): BelarusianTerm = {
    val u = if (BelarusianTerm.isVowel(what.prepositional.charAt(0))) " ў " else " у "

    new BelarusianTerm(
      this.nominative    + u + what.prepositional,
      this.genitive      + u + what.prepositional,
      this.dative        + u + what.prepositional,
      this.accusative    + u + what.prepositional,
      this.instrumental  + u + what.prepositional,
      this.prepositional + u + what.prepositional,
      this.gender)
  }
}

object BelarusianTerm {

  /**
    * Describes what adjective and verb forms should be used with a noun. 'Plural' isn't really a gender, but as for
    * adjective forms it can be treated as such.
    */
  sealed trait Gender
  case object Masculine extends Gender
  case object Feminine extends Gender
  case object Neuter extends Gender
  case object Plural extends Gender

  /**
    * Changes 'у' into 'ў' where necessary.
    * @param s input string
    * @return processed string
    */
  def addShortUWhereNecessary(s: String): String = {
    val chars = s.toCharArray.zipWithIndex

    def shouldBeShortU(index: Int): Boolean = {
      if (chars(index)._1 == 'у') {
        val nextCharOption = chars drop (index + 1) find {
          !_._1.isWhitespace
        }
        nextCharOption match {
          case Some((c, _)) => isVowel(c)
          case None => false
        }
      } else false
    }

    val result = chars map { case (c, i) =>
      if (shouldBeShortU(i)) 'ў' else c
    }

    result mkString
  }

  def isVowel(c: Char) = "аіуэояюеё" contains c
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

  def in(what: RussianTerm): RussianTerm = {
    new RussianTerm(
      this.nominative    + " в " + what.prepositional,
      this.genitive      + " в " + what.prepositional,
      this.dative        + " в " + what.prepositional,
      this.accusative    + " в " + what.prepositional,
      this.instrumental  + " в " + what.prepositional,
      this.prepositional + " в " + what.prepositional,
      this.gender)
  }
}

object RussianTerm {

  /**
    * Describes what adjective and verb forms should be used with a noun. 'Plural' isn't really a gender, but as for
    * adjective forms it can be treated as such.
    */
  sealed trait Gender
  case object Masculine extends Gender
  case object Feminine extends Gender
  case object Neuter extends Gender
  case object Plural extends Gender
}

object Terms {

  object Function extends Term {
    val en_US = new EnglishTerm("function", EnglishTerm.Singular)

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
    val en_US = new EnglishTerm("type", EnglishTerm.Singular)

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
    val en_US = new EnglishTerm("definition", EnglishTerm.Singular)

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

  object Definitions extends Term {
    val en_US = new EnglishTerm("definitions", EnglishTerm.Plural)

    val be_BY = new BelarusianTerm(
      "акрэсьленьні",
      "акрэсьленьняў",
      "акрэсьленьням",
      "акрэсьленьні",
      "акрэсьленьнямі",
      "акрэсьленьнях",
      BelarusianTerm.Plural)

    val ru_RU = new RussianTerm(
      "определения",
      "определений",
      "определениям",
      "определения",
      "определениями",
      "определениях",
      RussianTerm.Plural)
  }

  object Argument extends Term {
    val en_US = new EnglishTerm("argument", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "аргумент",
      "аргумента",
      "аргументу",
      "аргумент",
      "аргументам",
      "аргуменьце",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "аргумент",
      "аргумента",
      "аргументу",
      "аргумент",
      "аргументом",
      "аргументе",
      RussianTerm.Masculine)
  }

  object Arguments extends Term {
    val en_US = new EnglishTerm("arguments", EnglishTerm.Plural)

    val be_BY = new BelarusianTerm(
      "аргументы",
      "аргументаў",
      "аргументам",
      "аргументы",
      "аргументамі",
      "аргументах",
      BelarusianTerm.Plural)

    val ru_RU = new RussianTerm(
      "аргументы",
      "аргументов",
      "аргументам",
      "аргументы",
      "аргументами",
      "аргументах",
      RussianTerm.Plural)
  }

  object List extends Term {
    val en_US = new EnglishTerm("list", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "сьпіс",
      "сьпіса",
      "сьпісу",
      "сьпіс",
      "сьпісам",
      "сьпісе",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "список",
      "списка",
      "списку",
      "список",
      "списком",
      "списке",
      RussianTerm.Masculine)
  }

  object OpeningParen extends Term {
    val en_US = new EnglishTerm("opening '('", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "адкрывальная '('",
      "адкрывальнай '('",
      "адкрывальнай '('",
      "адкрывальную '('",
      "адкрывальнай '('",
      "адкрывальнай '('",
      BelarusianTerm.Feminine)

    val ru_RU = new RussianTerm(
      "открывающая '('",
      "открывающей '('",
      "открывающей '('",
      "открывающую '('",
      "открывающей '('",
      "открывающей '('",
      RussianTerm.Feminine)
  }

  object ClosingParen extends Term {
    val en_US = new EnglishTerm("closing ')'", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "закрывальная ')'",
      "закрывальнай ')'",
      "закрывальнай ')'",
      "закрывальную ')'",
      "закрывальнай ')'",
      "закрывальнай ')'",
      BelarusianTerm.Feminine)

    val ru_RU = new RussianTerm(
      "закрывающая ')'",
      "закрывающей ')'",
      "закрывающей ')'",
      "закрывающую ')'",
      "закрывающей ')'",
      "закрывающей ')'",
      RussianTerm.Feminine)
  }

  object OpeningBrace extends Term {
    val en_US = new EnglishTerm("opening '{'", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "адкрывальная '{'",
      "адкрывальнай '{'",
      "адкрывальнай '{'",
      "адкрывальную '{'",
      "адкрывальнай '{'",
      "адкрывальнай '{'",
      BelarusianTerm.Feminine)

    val ru_RU = new RussianTerm(
      "открывающая '{'",
      "открывающей '{'",
      "открывающей '{'",
      "открывающую '{'",
      "открывающей '{'",
      "открывающей '{'",
      RussianTerm.Feminine)
  }

  object ClosingBrace extends Term {
    val en_US = new EnglishTerm("closing ')'", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "закрывальная '}'",
      "закрывальнай '}'",
      "закрывальнай '}'",
      "закрывальную '}'",
      "закрывальнай '}'",
      "закрывальнай '}'",
      BelarusianTerm.Feminine)

    val ru_RU = new RussianTerm(
      "закрывающая '}'",
      "закрывающей '}'",
      "закрывающей '}'",
      "закрывающую '}'",
      "закрывающей '}'",
      "закрывающей '}'",
      RussianTerm.Feminine)
  }

  object Comma extends Term {
    val en_US = new EnglishTerm("comma", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "коска",
      "коскі",
      "косцы",
      "коску",
      "коскай",
      "косцы",
      BelarusianTerm.Feminine)

    val ru_RU = new RussianTerm(
      "запятая",
      "запятой",
      "запятой",
      "запятую",
      "запятой",
      "запятой",
      RussianTerm.Feminine)
  }

  object Operator extends Term {
    val en_US = new EnglishTerm("operator", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "аператар",
      "аператара",
      "аператару",
      "аператар",
      "аператарам",
      "аператару",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "оператор",
      "оператора",
      "оператору",
      "оператор",
      "оператором",
      "операторе",
      RussianTerm.Masculine)
  }

  object Operators extends Term {
    val en_US = new EnglishTerm("operators", EnglishTerm.Plural)

    val be_BY = new BelarusianTerm(
      "аператары",
      "аператараў",
      "аператарам",
      "аператары",
      "аператарамі",
      "аператарах",
      BelarusianTerm.Plural)

    val ru_RU = new RussianTerm(
      "операторы",
      "операторов",
      "операторам",
      "операторы",
      "операторами",
      "операторах",
      RussianTerm.Plural)
  }

  object Expression extends Term {
    val en_US = new EnglishTerm("expression", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "выражэньне",
      "выражэньня",
      "выражэньню",
      "выражэньне",
      "выражэньнем",
      "выражэньні",
      BelarusianTerm.Neuter)

    val ru_RU = new RussianTerm(
      "выражение",
      "выражения",
      "выражению",
      "выражение",
      "выражением",
      "выражении",
      RussianTerm.Neuter)
  }

  object Parentheses extends Term {
    val en_US = new EnglishTerm("parentheses", EnglishTerm.Plural)

    val be_BY = new BelarusianTerm(
      "дужкі",
      "дужак",
      "дужкам",
      "дужкі",
      "дужкамі",
      "дужках",
      BelarusianTerm.Plural)

    val ru_RU = new RussianTerm(
      "скобки",
      "скобок",
      "скобкам",
      "скобки",
      "скобками",
      "скобках",
      RussianTerm.Plural)
  }

  object Statement extends Term {
    val en_US = new EnglishTerm("statement", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "інструкцыя",
      "інструкцыі",
      "інструкцыі",
      "інструкцыю",
      "інструкцыяй",
      "інструкцыі",
      BelarusianTerm.Feminine)

    val ru_RU = new RussianTerm(
      "инструкция",
      "инструкции",
      "инструкции",
      "инструкцию",
      "инструкцией",
      "инструкции",
      RussianTerm.Feminine)

    def apply(statementType: String) = new Term {
      val en_US = new EnglishTerm(s"'$statementType' statement", EnglishTerm.Singular)

      val be_BY = new BelarusianTerm(
        s"інструкцыя '$statementType'",
        s"інструкцыі '$statementType'",
        s"інструкцыі '$statementType'",
        s"інструкцыю '$statementType'",
        s"інструкцыяй '$statementType'",
        s"інструкцыі '$statementType'",
        BelarusianTerm.Feminine)

      val ru_RU = new RussianTerm(
        s"инструкция '$statementType'",
        s"инструкции '$statementType'",
        s"инструкции '$statementType'",
        s"инструкцию '$statementType'",
        s"инструкцией '$statementType'",
        s"инструкции '$statementType'",
        RussianTerm.Feminine)
    }
  }

  object Branch {
    def apply(branchName: String) = new Term {
      val en_US = new EnglishTerm(s"'$branchName' branch", EnglishTerm.Singular)

      val be_BY = new BelarusianTerm(
        s"галіна '$branchName'",
        s"галіны '$branchName'",
        s"галіне '$branchName'",
        s"галіну '$branchName'",
        s"галінай '$branchName'",
        s"галіне '$branchName'",
        BelarusianTerm.Feminine)

      val ru_RU = new RussianTerm(
        s"ветвь '$branchName'",
        s"ветви '$branchName'",
        s"ветви '$branchName'",
        s"ветвь '$branchName'",
        s"ветвью '$branchName'",
        s"ветви '$branchName'",
        RussianTerm.Feminine)
    }
  }

  object Initializer extends Term {
    val en_US = new EnglishTerm("initializer", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "ініцыялізатар",
      "ініцыялізатара",
      "ініцыялізатару",
      "ініцыялізатар",
      "ініцыялізатарам",
      "ініцыялізатару",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "инициализатор",
      "инициализатора",
      "инициализатору",
      "инициализатор",
      "инициализатором",
      "инициализаторе",
      RussianTerm.Masculine)
  }

  object Variable extends Term {
    val en_US = new EnglishTerm("variable", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "зьменная",
      "зьменнай",
      "зьменнай",
      "зьменную",
      "зьменнай",
      "зьменнай",
      BelarusianTerm.Feminine)

    val ru_RU = new RussianTerm(
      "переменная",
      "переменной",
      "переменной",
      "переменную",
      "переменной",
      "переменной",
      RussianTerm.Feminine)
  }

  object Variables extends Term {
    val en_US = new EnglishTerm("variables", EnglishTerm.Plural)

    val be_BY = new BelarusianTerm(
      "зьменныя",
      "зьменных",
      "зьменным",
      "зьменныя",
      "зьменнымі",
      "зьменных",
      BelarusianTerm.Plural)

    val ru_RU = new RussianTerm(
      "переменные",
      "переменных",
      "переменным",
      "переменные",
      "переменными",
      "переменных",
      RussianTerm.Plural)
  }

  object Body extends Term {
    val en_US = new EnglishTerm("body", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "цела",
      "цела",
      "целу",
      "цела",
      "целам",
      "целе",
      BelarusianTerm.Neuter)

    val ru_RU = new RussianTerm(
      "тело",
      "тела",
      "телу",
      "тело",
      "телом",
      "теле",
      RussianTerm.Neuter)
  }

  object Loop extends Term {
    val en_US = new EnglishTerm("loop", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "цыкл",
      "цыкла",
      "цыклу",
      "цыкл",
      "цыклам",
      "цыкле",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "цикл",
      "цикла",
      "циклу",
      "цикл",
      "циклом",
      "цикле",
      RussianTerm.Masculine)
  }

  object Code extends Term {
    val en_US = new EnglishTerm("code", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "код",
      "кода",
      "коду",
      "код",
      "кодам",
      "кодзе",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "код",
      "кода",
      "коду",
      "код",
      "кодом",
      "коде",
      RussianTerm.Masculine)
  }

  object Block extends Term {
    val en_US = new EnglishTerm("block", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "блок",
      "блока",
      "блоку",
      "блок",
      "блокам",
      "блоку",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "блок",
      "блока",
      "блоку",
      "блок",
      "блоком",
      "блоке",
      RussianTerm.Masculine)
  }

  object Parameter extends Term {
    val en_US = new EnglishTerm("parameter", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "параметр",
      "параметра",
      "параметру",
      "параметр",
      "параметрам",
      "параметры",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "параметр",
      "параметра",
      "параметру",
      "параметр",
      "параметром",
      "параметре",
      RussianTerm.Masculine)
  }

  object Parameters extends Term {
    val en_US = new EnglishTerm("parameters", EnglishTerm.Plural)

    val be_BY = new BelarusianTerm(
      "параметры",
      "параметраў",
      "параметрам",
      "параметры",
      "параметрамі",
      "параметрах",
      BelarusianTerm.Plural)

    val ru_RU = new RussianTerm(
      "параметры",
      "параметров",
      "параметрам",
      "параметры",
      "параметрами",
      "параметрах",
      RussianTerm.Plural)
  }

  object Name extends Term {
    val en_US = new EnglishTerm("name", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "імя",
      "імені",
      "імені",
      "імя",
      "імем",
      "імені",
      BelarusianTerm.Neuter)

    val ru_RU = new RussianTerm(
      "имя",
      "имени",
      "имени",
      "имя",
      "именем",
      "имени",
      RussianTerm.Neuter)
  }

  object Specifier extends Term {
    val en_US = new EnglishTerm("specifier", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "сьпецыфікатар",
      "сьпецыфікатара",
      "сьпецыфікатару",
      "сьпецыфікатар",
      "сьпецыфікатарам",
      "сьпецыфікатару",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "спецификатор",
      "спецификатора",
      "спецификатору",
      "спецификатор",
      "спецификатором",
      "спецификаторе",
      RussianTerm.Masculine)
  }

  object Specifiers extends Term {
    val en_US = new EnglishTerm("specifier", EnglishTerm.Plural)

    val be_BY = new BelarusianTerm(
      "сьпецыфікатары",
      "сьпецыфікатараў",
      "сьпецыфікатарам",
      "сьпецыфікатары",
      "сьпецыфікатарамі",
      "сьпецыфікатарах",
      BelarusianTerm.Plural)

    val ru_RU = new RussianTerm(
      "спецификаторы",
      "спецификаторов",
      "спецификаторам",
      "спецификаторы",
      "спецификаторами",
      "спецификаторах",
      RussianTerm.Plural)
  }

  object Symbol extends Term {
    val en_US = new EnglishTerm("symbol", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "сімвал",
      "сімвала",
      "сімвалу",
      "сімвал",
      "сімвалам",
      "сімвале",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "символ",
      "символа",
      "символу",
      "символ",
      "символом",
      "символе",
      RussianTerm.Masculine)
  }

  object Symbols extends Term {
    val en_US = new EnglishTerm("symbols", EnglishTerm.Plural)

    val be_BY = new BelarusianTerm(
      "сімвалы",
      "сімвалаў",
      "сімвалам",
      "сімвалы",
      "сімваламі",
      "сімвалах",
      BelarusianTerm.Plural)

    val ru_RU = new RussianTerm(
      "символы",
      "символов",
      "символам",
      "символы",
      "символами",
      "символах",
      RussianTerm.Plural)
  }

  object Context extends Term {
    val en_US = new EnglishTerm("context", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "кантэкст",
      "кантэкста",
      "кантэксту",
      "кантэкст",
      "кантэкстам",
      "кантэксьце",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "контекст",
      "контекста",
      "контексту",
      "контекст",
      "контекстом",
      "контексте",
      RussianTerm.Masculine)
  }

  object Method extends Term {
    val en_US = new EnglishTerm("method", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "метад",
      "метада",
      "метаду",
      "метад",
      "метадам",
      "метадзе",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "метод",
      "метода",
      "методу",
      "метод",
      "методом",
      "методе",
      RussianTerm.Masculine)
  }

  object Namespace extends Term {
    val en_US = new EnglishTerm("namespace", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "прастора імёнаў",
      "прасторы імёнаў",
      "прасторы імёнаў",
      "прастору імёнаў",
      "прасторай імёнаў",
      "прасторы імёнаў",
      BelarusianTerm.Feminine)

    val ru_RU = new RussianTerm(
      "пространство имён",
      "пространства имён",
      "пространству имён",
      "пространство имён",
      "пространством имён",
      "пространстве имён",
      RussianTerm.Neuter)
  }

  object Ampersand extends Term {
    val en_US = new EnglishTerm("ampersand", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "амперсанд",
      "амперсанда",
      "амперсанду",
      "амперсанд",
      "амперсандам",
      "амперсандзе",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "амперсанд",
      "амперсанда",
      "амперсанду",
      "амперсанд",
      "амперсандом",
      "амперсанде",
      RussianTerm.Masculine)
  }

  object Constructor extends Term {
    val en_US = new EnglishTerm("constructor", EnglishTerm.Singular)

    val be_BY = new BelarusianTerm(
      "канструктар",
      "канструктара",
      "канструктару",
      "канструктар",
      "канструктарам",
      "канструктару",
      BelarusianTerm.Masculine)

    val ru_RU = new RussianTerm(
      "конструктор",
      "конструктора",
      "конструктору",
      "конструктор",
      "конструктором",
      "конструкторе",
      RussianTerm.Masculine)
  }
}
