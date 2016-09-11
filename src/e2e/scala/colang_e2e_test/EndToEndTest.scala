package colang_e2e_test

import java.io.File

import colang.Compiler
import colang.ast.parsed.AnalyzerImpl
import colang.ast.raw.ParserImpl
import colang.backend.c.{CCodeGenerator, CVerboseNameGenerator}
import colang.issues.Issue
import colang.tokens.LexerImpl
import org.scalatest._

import scala.io.Source
import scala.sys.process._

class EndToEndTest extends FunSpec {

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
      val errorsWithLineIndex = expectedErrors zip Seq.fill(expectedErrors.size)(index)
      val warningsWithLineIndex = expectedWarnings zip Seq.fill(expectedErrors.size)(index)
      errorsWithLineIndex ++ warningsWithLineIndex
    }
  }

  private def assertThatIssuesMatchExpected(actualIssues: Seq[Issue], expectedIssues: Seq[(String, Int)]): Unit = {
    val oddIssues = actualIssues map { i => (i.code, i.source.startLine) } filterNot { expectedIssues contains _ }
    val missingIssues = expectedIssues filterNot { actualIssues map { i => (i.code, i.source.startLine) } contains _ }

    if (missingIssues.nonEmpty) {
      val missingIssuesStr = missingIssues map { case (code, line) => s"$code at line ${line + 1}"} mkString ", "
      fail(s"Expected issues not found: $missingIssuesStr.")
    }

    if (oddIssues.nonEmpty) {
      val oddIssuesStr = oddIssues map { case (code, line) => s"$code at line ${line + 1}"} mkString ", "
      fail(s"Unexpected issues encountered: $oddIssuesStr.")
    }
  }

  private def describeTestSample(sample: File): Unit = {
    val fileName = sample.getAbsolutePath.replaceAll(""".*e2e\-test\-samples\/""", "")

    describe(fileName) {
      it("should behave as specified") {
        val cFile = new File("co_e2e_test.c")
        val runFile = new File("co_e2e_test")

        val compiler = new Compiler(
          sample,
          cFile,
          new LexerImpl,
          new ParserImpl,
          new AnalyzerImpl,
          new CCodeGenerator(sample, cFile, new CVerboseNameGenerator),
          silent = true)

        val issues = compiler.compile()
        val expectedIssues = parseExpectedIssues(sample)

        assertThatIssuesMatchExpected(issues, expectedIssues)

        try {
          if (cFile.exists()) {
            println("Compiling " + fileName)
            val gccReturnCode = s"gcc -o ${runFile.getName} ${cFile.getName}".!
            println(s"GCC return code: $gccReturnCode")

            if (gccReturnCode != 0) {
              fail("C target file failed to compile.")
            }

            if (runFile.exists()) {
              println("Running " + fileName)
              val programReturnCode = s"./${runFile.getName}".!
              println(s"Program return code: $programReturnCode")

              if (programReturnCode != 0) {
                fail("Program exited with non-zero exit code.")
              }
            }
          }
        } finally {
          if (cFile.exists) cFile.delete()
          if (runFile.exists) runFile.delete()
        }
      }
    }
  }

  private val samplesRoot = new File(getClass.getResource("/e2e-test-samples").toURI)

  private def iterateOverSamplesDirectory(directory: File): Unit = {
    directory.listFiles filter { _.isFile } foreach describeTestSample
    directory.listFiles filter { _.isDirectory } foreach iterateOverSamplesDirectory
  }

  iterateOverSamplesDirectory(samplesRoot)
}
