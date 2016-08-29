package colang.ast.raw

import colang.Strategy.Result
import colang.Strategy.Result.Success
import colang.ast.raw.ParserImpl.SingleTokenStrategy
import colang.issues.{Issues, Term}
import colang.tokens.{Keyword, NativeKeyword}
import colang.{SourceCode, StrategyUnion, TokenStream}

/**
  * Represents a list of specifiers that may preceed certain symbol definitions (like 'native', etc.)
  * @param specifiers specifiers list
  */
case class SpecifiersList(specifiers: Seq[Keyword], source: SourceCode) extends Node {

  def has[K <: Keyword](keyword: Class[K]) = specifiers exists { _.getClass == keyword }
  def get[K <: Keyword](keyword: Class[K]) = getOption(keyword).get
  def getOption[K <: Keyword](keyword: Class[K]) = specifiers find { _.getClass == keyword }
}

object SpecifiersList {

  /**
    * A strategy template for parsing specifier lists.
    * @param contextDescription a term that describes the node containing this specifier list.
    * @param allowedSpecifiers specifier types that are allowed in this context
    */
  class Strategy(contextDescription: Term,
                 allowedSpecifiers: Class[_ <: Keyword]*) extends ParserImpl.Strategy[SpecifiersList] {

    private val allSpecifiers = Seq(
      classOf[NativeKeyword])

    private val anySpecifierStrategy = StrategyUnion(allSpecifiers map { SingleTokenStrategy(_) } :_*)

    def apply(stream: TokenStream): Result[TokenStream, SpecifiersList] = {
      ParserImpl.parseSequence(
        stream = stream,
        elementStrategy = anySpecifierStrategy,
        elementDescription = "specifier"
      ) match {
        case (specifiers, parseIssues, streamAfterSpecifiers) =>
          val specifiersByType = specifiers groupBy { _.getClass }

          val issues = specifiersByType flatMap { case (specifierType, specs) =>
            if (!(allowedSpecifiers contains specifierType)) {
              specs map { spec => Issues.MisplacedSpecifier(spec.source, (spec.text, contextDescription)) }
            } else {
              specs.tail map { spec => Issues.RepeatedSpecifier(spec.source, (spec.text, specs.head.source)) }
            }
          }

          val validSpecifiers = (specifiersByType filter { allowedSpecifiers contains _._1 }).values map { _.head } toSeq
          val source = if (validSpecifiers.nonEmpty) {
            validSpecifiers.head.source + validSpecifiers.last.source
          } else {
            streamAfterSpecifiers.beforeNextNonWhitespace
          }

          Success(SpecifiersList(validSpecifiers, source), parseIssues ++ issues, streamAfterSpecifiers)
      }
    }
  }
}