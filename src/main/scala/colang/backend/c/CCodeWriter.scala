package colang.backend.c

/**
  * Represents a C backend components that formats and writes the output to target file.
  */
trait CCodeWriter {

  /**
    * Formats and writes output.
    * @param code C source code object
    */
  def write(code: CSourceFile)
}
