package colang

import java.io.File

import colang.ast.parsed.{Analyzer, AnalyzerImpl}
import colang.ast.raw._
import colang.backend.Backend
import colang.backend.c.{CCodeGenerator, CVerboseCodeWriter}
import colang.tokens.{Lexer, LexerImpl}
import colang.utils.StringImplicits._

/**
  * Compiler command-line configuration representation. Command line options are parsed by scopt.
  * @param out target C file
  * @param source source file
  */
case class Config(out: Option[File] = None, source: Option[File] = None)

/**
  * The main class that ties everything together. Compiler component implementations must be passed to the constructor
  * as dependencies.
  * @param inFile source CO file
  * @param outFile target C file
  * @param lexer lexer implementation to use
  * @param parser parser implementation to use
  */
class Compiler(inFile: File, outFile: File, lexer: Lexer, parser: Parser, analyzer: Analyzer, backend: Backend) {

  /**
    * Compiles the source file, writing any issues to stderr. If no errors were encountered, creates and populates
    * target C file.
    */
  def compile(): Unit = {
    val sourceFile = new RealSourceFile(inFile)
    val preludeFile = new RealSourceFile(locatePrelude)

    val (prelude, preludeIssues) = parseFile(preludeFile)
    val (source, sourceIssues) = parseFile(sourceFile)

    val translationUnit = TranslationUnit(prelude.symbols ++ source.symbols)

    val (rootNamespace, analyzerIssues) = analyzer.analyze(translationUnit.symbols, sourceFile.eof)

    val issues = preludeIssues ++ sourceIssues ++ analyzerIssues

    val sortedIssues = issues sortBy { i => (i.source.startLine, i.source.startChar, -i.source.endLine, -i.source.endChar) }
    sortedIssues foreach printIssue

    if (!(issues exists { _.isInstanceOf[Error] })) {
      backend.process(rootNamespace)
    }
  }

  /**
    * Tries to locate 'prelude.co' from the standard library.
    * @return java.io.File pointing to the Prelude
    */
  private def locatePrelude: File = {
    val homeDir = System.getProperty("user.home")

    val homeFile = new File(s"$homeDir/.colang-libs/prelude.co")
    val usrLocalFile = new File("/usr/local/lib/colang/prelude.co")
    val usrFile = new File("/usr/lib/colang/prelude.co")
    val rootFile = new File("/lib/colang/prelude.co")

    if (homeFile.exists()) {
      homeFile
    } else if (usrLocalFile.exists()) {
      usrLocalFile
    } else if (usrFile.exists()) {
      usrFile
    } else if (rootFile.exists()) {
      rootFile
    } else {
      System.err.println("Error: 'prelude.co' not found. Please install CO standard library.")
      sys.exit(2)
    }
  }

  /**
    * Parses a raw file.
    * @param file raw CO source file
    * @return (parsed translation unit, encountered issues)
    */
  private def parseFile(file: SourceFile): (TranslationUnit, Seq[Issue]) = {
    val (tokens, lexerIssues) = lexer.splitIntoTokens(file)
    val (translationUnit, parserIssues) = parser.parse(tokens)

    (translationUnit, lexerIssues ++ parserIssues)
  }

  /**
    * Prints an issue to stderr.
    * @param issue issue to print
    */
  def printIssue(issue: Issue): Unit = {
    def colorWarning(s: String) = Console.YELLOW + s + Console.RESET
    def colorError(s: String) = Console.RED + s + Console.RESET
    def colorNote(s: String) = Console.WHITE + s + Console.RESET

    val (issueType, color, source, message, notes) = issue match {
      case Warning(s, m, n) => ("warning", colorWarning _, s, m, n)
      case Error(s, m, n) => ("error", colorError _, s, m, n)
      case Note(s, m) => ("note", colorNote _, s, m, Seq.empty)
    }

    val heading = s"${source.file.name}:${source.startLine + 1}:${source.startChar + 1}: ${color(issueType)}: $message"
    System.err.println(heading)

    val codeListing = (source.startLine to source.endLine) map { lineNo =>
      val line = source.file.lines(lineNo)

      val startChar = if (lineNo == source.startLine) {
        source.startChar
      } else {
        line.length - line.trimLeft.length
      }

      val endChar = if (lineNo == source.endLine) {
        source.endChar
      } else {
        line.trimRight.length - 1
      }

      //When line is non-empty
      if (startChar <= endChar) {
        line + "\n" + " " * startChar + color("~" * (endChar - startChar + 1))
      } else {
        line
      }
    } mkString "\n"

    System.err.println(codeListing)

    notes foreach printIssue
  }
}

object Compiler {

  /**
    * Compiler version as specified in build.sbt project definition.
    * BuildInfo is a generated source file under /target/scala-2.11/src_managed/main/sbt-buildinfo
    */
  val VERSION = BuildInfo.version

  def main(args: Array[String]): Unit = {
    val argsParser = new scopt.OptionParser[Config]("colang") {
      head("colang", VERSION)

      opt[File]('o', "out").valueName("<file>")
        .action((f, c) => c.copy(out = Some(f)))
        .text("generated C source file name")

      arg[File]("<source-file>").required()
        .action((f, c) => c.copy(source = Some(f)))
        .text("CO source file name")
    }

    argsParser.parse(args, Config()) match {
      case Some(config) =>
        val inFile = config.source.get
        val outFile = config.out match {
          case Some(f) => f
          case None =>
            val sourceName = inFile.getName
            val outName = if (sourceName.contains(".")) {
              sourceName.replaceAll("""\.[^.]+$""", ".c")
            } else {
              sourceName + ".c"
            }
            new File(outName)
        }

        val lexer = new LexerImpl
        val parser = new ParserImpl
        val analyzer = new AnalyzerImpl
        val backend = new CCodeGenerator(new CVerboseCodeWriter(inFile, outFile))
        new Compiler(inFile, outFile, lexer, parser, analyzer, backend).compile()

      case None => sys.exit(2)
    }
  }
}