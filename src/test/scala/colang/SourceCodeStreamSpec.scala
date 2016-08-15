package colang

import colang.TestUtils.InlineSourceFile

class SourceCodeStreamSpec extends UnitSpec {

  val sourceFile = new InlineSourceFile("sum.co", """void main() {
                                                    |    read int x, y
                                                    |    println(x + y)
                                                    |}""".stripMargin)

  val nonEmptyStream = new SourceCodeStream(sourceFile, 5, 0, 5)
  val emptyStream = new SourceCodeStream(sourceFile, 52, 3, 1)

  describe("A source code stream") {
    it("should implement 'charAt' correctly") {
      nonEmptyStream.charAt(0) should be ('m')
      nonEmptyStream.charAt(2) should be ('i')
    }

    it("should implement 'length' correctly") {
      nonEmptyStream.length should be ("main() {\n    read int x, y\n    println(x + y)\n}".length)
    }

    it("should implement 'subSequence' correctly") {
      nonEmptyStream.subSequence(0, 2) should be ("ma")
      nonEmptyStream.subSequence(1, 4) should be ("ain")
    }

    it("should report emptiness correctly") {
      nonEmptyStream.isEmpty should be (false)
      nonEmptyStream.nonEmpty should be (true)
      emptyStream.isEmpty should be (true)
      emptyStream.nonEmpty should be (false)
    }

    it("should extract fragments without line breaks correctly") {
      val (source, newStream) = nonEmptyStream.take("main")
      source should matchPattern { case SourceCode(`sourceFile`, 0, 5, 0, 8) => }
      newStream.file should be (sourceFile)
      newStream.startChar should be (9)
      newStream.lineNo should be (0)
      newStream.charNo should be (9)
    }

    it("should extract fragments with line breaks correctly") {
      val (source, newStream) = nonEmptyStream.take("main() {\n    ")
      source should matchPattern { case SourceCode(`sourceFile`, 0, 5, 1, 3) => }
      newStream.file should be (sourceFile)
      newStream.startChar should be (18)
      newStream.lineNo should be (1)
      newStream.charNo should be (4)
    }
  }
}
