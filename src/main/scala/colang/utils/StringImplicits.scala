package colang.utils

object StringImplicits {

  implicit final class StringOps(val self: String) {
    def trimLeft = self.replaceAll("""^\s+""", "")
    def trimRight = self.replaceAll("""\s+$""", "")

    def paddedSubstring(start: Int): String = {
      if (start < self.length) self.substring(start) else " "
    }

    def paddedSubstring(start: Int, end: Int): String = {
      if (start < self.length) {
        val actualSubstring = self.substring(start, end min self.length)
        val padding = " " * (end - self.length)

        actualSubstring + padding
      } else {
        " " * (end - start)
      }
    }
  }
}
