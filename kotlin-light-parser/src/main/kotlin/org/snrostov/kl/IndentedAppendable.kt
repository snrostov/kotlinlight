package org.snrostov.kl

val Appendable.nested: IndentedAppendable
  get() = when (this) {
    is IndentedAppendable -> IndentedAppendable(out, "$indent$indentIncrement", indentRequired, indentIncrement)
    else -> IndentedAppendable(this, "  ")
  }

fun Appendable.nested(block: (IndentedAppendable) -> Unit) = nested.also(block)

class IndentedAppendable(
  val out: Appendable = StringBuilder(),
  val indent: String = "",
  var indentRequired: Boolean = false,
  val indentIncrement: String = "  "
) : Appendable {
  override fun append(csq: CharSequence): java.lang.Appendable {
    var last = 0
    while (true) {
      val nextLine = csq.indexOf('\n', last)
      if (nextLine == -1) {
        if (last != csq.length) {
          flushIndent()
          out.append(csq, last, csq.length)
        }
        break
      } else {
        if (nextLine != last) {
          flushIndent()
          out.append(csq, last, nextLine)
        }
        out.append('\n')
        indentRequired = true
        last = nextLine + 1
      }
    }
    return this
  }

  private fun flushIndent() {
    if (indentRequired) {
      out.append(indent)
      indentRequired = false
    }
  }

  override fun append(csq: CharSequence, start: Int, end: Int): java.lang.Appendable {
    append(csq.substring(start, end))
    return this
  }

  override fun append(c: Char): java.lang.Appendable {
    append("$c")
    return this
  }
}