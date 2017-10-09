package colang

import java.io.File
import java.nio.file.Paths

import colang.ast.parsed.AnalyzerImpl
import colang.ast.raw.ParserImpl
import colang.backend.c.{CCodeGenerator, CVerboseNameGenerator}
import colang.issues.Issue
import colang.tokens.LexerImpl

import org.scalatest._

import scala.io.Source
import scala.sys.process._


class EndToEndTest extends FunSpec with Matchers {

  /**
    * Reads the sample file and parses expected issue specifications.
    * @param sample file to read
    * @return Seq(issue code, 0-based line index)
    */
  private def parseExpectedIssues(sample: File): Seq[(String, Int)] = {
    val lines = Source.fromFile(sample).getLines().toSeq

    val eRe = """//(E\d\d\d\d)""".r
    val wRe = """//(W\d\d\d\d)""".r

    lines.zipWithIndex flatMap { case (line, index) =>
      val expectedErrors = eRe findAllMatchIn line map { _.group(1) } toSeq
      val expectedWarnings = wRe findAllMatchIn line map { _.group(1) } toSeq

      val errorsWithLineIndex =
        expectedErrors zip Seq.fill(expectedErrors.size)(index)

      val warningsWithLineIndex =
        expectedWarnings zip Seq.fill(expectedWarnings.size)(index)

      errorsWithLineIndex ++ warningsWithLineIndex
    }
  }

  private def assertThatIssuesMatchExpected(actualIssues: Seq[Issue],
                                            expectedIssues: Seq[(String, Int)])
                                            : Unit = {
    // Extract issue codes and lines.
    val _actualIssues = actualIssues
      .map { issue => (issue.code, issue.source.startLine) }

    val unexpectedIssues = _actualIssues filterNot { expectedIssues contains _ }
    val missingIssues = expectedIssues filterNot { _actualIssues contains _ }

    if (missingIssues.nonEmpty) {
      val missingIssuesStr = missingIssues.map { case (code, line) =>
        s"$code at line ${line + 1}"
      } mkString ", "

      fail(s"Expected issues not found: $missingIssuesStr.")
    }

    if (unexpectedIssues.nonEmpty) {
      val unexpectedIssuesStr = unexpectedIssues map { case (code, line) =>
        s"$code at line ${line + 1}"
      } mkString ", "

      fail(s"Unexpected issues encountered: $unexpectedIssuesStr.")
    }
  }

  private def describeTestSample(sample: File): Unit = {
    val fileName = sample.getAbsolutePath.replaceAll(
      """^.*e2e-test-samples(/|\\)""",
      "")

    describe(fileName) {
      val tmpDir = System.getProperty("java.io.tmpdir")

      val cFile = Paths.get(tmpDir, s"co_e2e_${fileName.hashCode}.c").toFile
      val runFile = Paths.get(tmpDir, s"co_e2e_${fileName.hashCode}.exe").toFile

      val compiler = new Compiler(
        new LexerImpl,
        new ParserImpl,
        new AnalyzerImpl,
        new CCodeGenerator(sample, cFile, new CVerboseNameGenerator))

      it("should behave as expected") {
        val issues = compiler.compile(sample, silent = true)
        val expectedIssues = parseExpectedIssues(sample)

        assertThatIssuesMatchExpected(issues, expectedIssues)

        if (cFile.exists) {
          val gccReturnCode =
            s"gcc -lm -o ${runFile.getAbsolutePath} ${cFile.getAbsolutePath}".!
          gccReturnCode should be (0)
          cFile.delete()
        }

        if (runFile.exists) {
          val programReturnCode = s"${runFile.getAbsolutePath}".!
          programReturnCode should be (0)
          runFile.delete()
        }
      }
    }
  }

  private val samplesRoot =
    new File(getClass.getResource("/e2e-test-samples").toURI)

  private def iterateOverSamplesDirectory(directory: File): Unit = {
    directory.listFiles foreach {
      case testFile if testFile.isFile =>
        describeTestSample(testFile)
      case testDirectory if testDirectory.isDirectory =>
        iterateOverSamplesDirectory(testDirectory)
    }
  }

  iterateOverSamplesDirectory(samplesRoot)
}
