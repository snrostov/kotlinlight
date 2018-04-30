import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.parsing.topLevelKotlinParsing
import org.junit.Assert
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.snrostov.kl.*
import java.io.File

val dir = File("/Users/sergey/p/kotlinlight/kotlin-light-parser/src/test/testData/psi")
val dirPath = dir.path + "/"

val regex = Regex(" *PsiWhiteSpace[^\n]+\n")
val regex1 = Regex(" *PsiComment[^\n]+\n")

@RunWith(Parameterized::class)
class TestData(val fileName: String) {
  @Test
  fun assertTree() {
    val file = File(dirPath + fileName)
    val text = file.readText()
    val builderImpl = PsiBuilderImpl(text)
    val kotlinParsing = topLevelKotlinParsing(builderImpl)
    kotlinParsing.parseFile()

    val out = IndentedAppendable(indentIncrement = "  ")
    builderImpl.root.children.single().printLegacyTo(out)
    val assert = File(file.parent, file.nameWithoutExtension + ".txt").readText()

    val expetected = assert
      .replace("KtFile: ${file.name}", "KT_FILE")
      .replace(regex, "")
      .replace(regex1, "")
      .trim()

    val actual = out.out.toString()
      .replace(regex, "")
      .replace(regex1, "")
      .trim()

    Assert.assertEquals(expetected, actual)
  }

  companion object {
    @Parameterized.Parameters(name = "{0}")
    @JvmStatic
    fun data(): Collection<Array<Any>> =
      dir
        .walkTopDown()
        .filter { it.extension == "kt" }
        .map { arrayOf<Any>(it.path.removePrefix(dirPath)) }
        .toList()
  }
}

fun PsiBuilderImpl.Node.printLegacyTo(out: IndentedAppendable) {
  when {
    error != null -> {
      out.appendln("PsiErrorElement:$error")
      printChildrenTo(out)
    }
    lex -> {
      val text = text.replace("\n", "\\n")
      when {
        KtTokens.WHITESPACES.contains(type) -> out.appendln("PsiWhiteSpace('$text')")
        KtTokens.COMMENTS.contains(type) -> out.appendln("PsiComment($type)('$text')")
        else -> out.appendln("PsiElement($type)('$text')")
      }
    }
    else -> {
      out.appendln("$type")
      printChildrenTo(out)
    }
  }
}

private fun PsiBuilderImpl.Node.printChildrenTo(out: IndentedAppendable) {
  out.nested { nested ->
//    val children1 = children.filter { myWhitespacesOrComments.contains(it.type) }
    val children1 = children
    when {
      children.isEmpty() -> nested.appendln("<empty list>")
      children1.size == 1 && children1.single().lex -> children1.single().printLegacyTo(nested)
      else -> children.forEach {
        it.printLegacyTo(nested)
      }
    }
  }
}
