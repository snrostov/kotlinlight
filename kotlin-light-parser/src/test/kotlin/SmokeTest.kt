import org.jetbrains.kotlin.parsing.KotlinParsing
import org.junit.Assert
import org.junit.Test
import org.snrostov.kl.IndentedAppendable
import org.snrostov.kl.PsiBuilderImpl
import java.io.File

class SmokeTest {
  @Test
  fun test() {
    doTest("sample0.kt")
  }

  private fun doTest(fileName: String) {
    val file = File("/Users/sergey/p/kotlinlight/samples/$fileName")
    val text = file.readText()
    val psiBuilderImpl = PsiBuilderImpl(text)
    val kotlinParsing = KotlinParsing.createForTopLevel(psiBuilderImpl)
    kotlinParsing.parseFile()
    val actual = IndentedAppendable()
    psiBuilderImpl.root.children.single().printTo(actual)

    val excepted = File(file.parent, file.nameWithoutExtension + ".ast").readText()
    Assert.assertEquals(excepted, actual.out.toString())
  }
}