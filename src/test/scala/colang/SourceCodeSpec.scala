package colang

import colang.TestUtils._

class SourceCodeSpec extends UnitSpec {

  val sourceFile = new InlineSourceFile("sum.co", """void main() {
                                                    |    read int x, y
                                                    |    println(x + y)
                                                    |}""".stripMargin)

  describe("A source code fragment") {
    val singleLineSource = SourceCode(sourceFile, 0, 3, 0, 7)
    val afterEndSource = SourceCode(sourceFile, 0, 13, 0, 13)
    val multiLineSource = SourceCode(sourceFile, 1, 4, 3, 0)

    it("should calculate 'text' property correctly when spanning single line") {
      singleLineSource.text should be ("d mai")
    }

    it("should calculate 'text' property correctly when spanning multiple lines") {
      multiLineSource.text should be ("read int x, y\n    println(x + y)\n}")
    }

    it("should calculate 'text' property correctly when pointing at line continuations") {
      afterEndSource.text should be (" ")
    }

    it("should add with other fragments on the same line correctly") {
      (singleLineSource + afterEndSource) should matchPattern { case SourceCode(`sourceFile`, 0, 3, 0, 13) => }
      (afterEndSource + singleLineSource) should matchPattern { case SourceCode(`sourceFile`, 0, 3, 0, 13) => }
    }

    it("should add with other fragments on different lines correctly") {
      (singleLineSource + multiLineSource) should matchPattern { case SourceCode(`sourceFile`, 0, 3, 3, 0) => }
      (multiLineSource + singleLineSource) should matchPattern { case SourceCode(`sourceFile`, 0, 3, 3, 0) => }
      (afterEndSource + multiLineSource)   should matchPattern { case SourceCode(`sourceFile`, 0, 13, 3, 0) => }
      (multiLineSource + afterEndSource)   should matchPattern { case SourceCode(`sourceFile`, 0, 13, 3, 0) => }
    }

    it("should throw IllegalArgumentException on attempt to add a fragment from another file") {
      val otherFileSource = SourceCode(emptySourceFile, 0, 0, 0, 0)
      an [IllegalArgumentException] should be thrownBy (singleLineSource + otherFileSource)
      an [IllegalArgumentException] should be thrownBy (otherFileSource + singleLineSource)
    }

    it("should have 'before' property point at the first character") {
      singleLineSource.before should matchPattern { case SourceCode(`sourceFile`, 0, 3, 0, 3) => }
      afterEndSource.before   should matchPattern { case SourceCode(`sourceFile`, 0, 13, 0, 13) => }
      multiLineSource.before  should matchPattern { case SourceCode(`sourceFile`, 1, 4, 1, 4) => }
    }

    it("should have 'after' property point at the character after the last") {
      singleLineSource.after should matchPattern { case SourceCode(`sourceFile`, 0, 8, 0, 8) => }
      afterEndSource.after   should matchPattern { case SourceCode(`sourceFile`, 0, 14, 0, 14) => }
      multiLineSource.after  should matchPattern { case SourceCode(`sourceFile`, 3, 1, 3, 1) => }
    }
  }
}
