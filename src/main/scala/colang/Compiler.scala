package colang

import java.io.File

import colang.ast.parsed.{Analyzer, AnalyzerImpl}
import colang.ast.raw._
import colang.backend.Backend
import colang.backend.c.{CCodeGenerator, CVerboseNameGenerator}
import colang.issues.{Error, Issue, Note, Warning}
import colang.tokens.{Lexer, LexerImpl}
import colang.utils.InternalErrors
import colang.utils.Localization._
import colang.utils.StringImplicits._

/**
  * The main compiler class that ties everything together.
  * Component implementations must be passed as dependencies to this class.
  * @param lexer lexer implementation to use
  * @param parser parser implementation to use
  * @param analyzer semantic analyzer implementation to use
  * @param backend compiler backend to use
  */
class Compiler(lexer: Lexer,
               parser: Parser,
               analyzer: Analyzer,
               backend: Backend) {

  /**
    * Compiles the source file, logging all issues to stderr.
    * If no errors were encountered, passes the compiled file to the backend.
    * @param coSourceFile java.io.File pointing to CO source file to compile
    * @param silent if true, issues will not be logged
    * @return all encountered issues
    */
  def compile(coSourceFile: File, silent: Boolean = false): Seq[Issue] = {
    val sourceFile = new RealSourceFile(coSourceFile)
    val preludeFile = new RealSourceFile(locatePrelude())

    val (prelude, preludeIssues) = parseFile(preludeFile)
    val (source, sourceIssues) = parseFile(sourceFile)

    val translationUnit = TranslationUnit(prelude.symbols ++ source.symbols)

    val (rootNamespace, analyzerIssues) =
      analyzer.analyze(translationUnit.symbols, sourceFile.eof)

    val issues = preludeIssues ++ sourceIssues ++ analyzerIssues

    val sortedIssues = issues sortBy { issue =>
      val source = issue.source
      (source.startLine, source.startChar, -source.endLine, -source.endChar)
    }

    if (!silent) {
      sortedIssues foreach printIssue
    }

    if (!(issues exists { _.isInstanceOf[Error] })) {
      backend.process(rootNamespace)
    }

    sortedIssues
  }

  /**
    * Tries to locate 'prelude.co' from the standard library.
    * @return java.io.File pointing to the Prelude
    */
  private def locatePrelude(): File = {
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
      InternalErrors.missingPrelude
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

    val (issueType, color, source, message, notes) = issue match {
      case Warning(_, s, m, n) => (tr("warning"), colorWarning _, s, m, n)
      case Error(_, s, m, n) => (tr("error"), colorError _, s, m, n)
    }

    val fileName = source.file.name
    val lineNo = source.startLine + 1
    val charNo = source.startChar + 1

    val heading = s"$fileName:$lineNo:$charNo: ${color(issueType)}: $message"
    System.err.println(heading)

    printSourceFragment(source, color)

    notes foreach printNote
  }

  /**
    * Prints a note to stderr
    * @param note note to print
    */
  def printNote(note: Note): Unit = {
    def colorNote(s: String) = Console.WHITE + s + Console.RESET

    note.source match {
      case Some(source) =>
        val fileName = source.file.name
        val lineNo = source.startLine + 1
        val charNo = source.startChar + 1

        val heading =
          s"$fileName:$lineNo:$charNo: ${colorNote(tr("note"))}: ${note.message}"
        System.err.println(heading)

        printSourceFragment(source, colorNote)
      case None =>
        System.err.println(s"${colorNote(tr("note"))}: ${note.message}")
    }
  }

  /**
    * Prints a source code fragment to stderr.
    * All lines at least partially covered by the fragment will be printed,
    * and the actual reported fragment will be underlined by `~` characters
    * of a given color
    * @param source code fragment to print
    * @param color function that adds color to a string
    */
  private def printSourceFragment(source: SourceCode,
                                  color: String => String): Unit = {
    val listing = (source.startLine to source.endLine) map { lineNo =>
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

      // If line is not empty
      if (startChar <= endChar) {
        line + "\n" + " " * startChar + color("~" * (endChar - startChar + 1))
      } else {
        line
      }
    } mkString "\n"

    System.err.println(listing)
  }
}

/**
  * Compiler command-line configuration representation.
  * Command line options are parsed by scopt.
  * @param out target C file
  * @param source source file
  */
case class Config(out: Option[File] = None, source: Option[File] = None)

object Compiler {

  /**
    * Compiler version as specified in build.sbt project definition.
    * BuildInfo is a generated source file under
    * /target/scala-.../src_managed/main/sbt-buildinfo
    */
  val VERSION: String = BuildInfo.version

  def main(args: Array[String]): Unit = {
    val argsParser = new scopt.OptionParser[Config]("colang") {
      head("colang", VERSION)

      opt[File]('o', "out").valueName("<file>")
        .action((file, config) => config.copy(out = Some(file)))
        .text(tr("generated_c_file_name"))

      arg[File]("<source-file>").required()
        .action((file, config) => config.copy(source = Some(file)))
        .text(tr("co_source_file_name"))
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
        val backend = new CCodeGenerator(
          inFile, outFile, new CVerboseNameGenerator())

        val compiler = new Compiler(lexer, parser, analyzer, backend)

        val issues = compiler.compile(inFile)
        if (issues.nonEmpty) sys.exit(1)

      case None => sys.exit(2)
    }
  }
}
