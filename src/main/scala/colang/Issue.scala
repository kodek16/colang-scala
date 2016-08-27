package colang

/**
  * Represents a problem that the compiler should report. An issue is bound to a source code fragment, and can be
  * associated with any number of notes.
  */
sealed trait Issue {
  /**
    * Referenced source code fragment.
    */
  def source: SourceCode

  /**
    * Compiler message.
    */
  def message: String

  /**
    * Associated notes.
    * @return
    */
  def notes: Seq[Note]
}

/**
  * Represents a fatal issue in the source code.
  */
case class Error  (source: SourceCode, message: String, notes: Seq[Note] = Seq.empty) extends Issue

/**
  * Represents a non-fatal, but possibly dangerous issue in the source code.
  */
case class Warning(source: SourceCode, message: String, notes: Seq[Note] = Seq.empty) extends Issue

/**
  * A message optionally bound to a source code fragment referenced by some other issue.
  */
case class Note(source: Option[SourceCode], message: String)