import org.jetbrains.kotlin.parsing.KotlinParsing
import org.junit.Test
import org.snrostov.kl.PsiBuilderImpl
import java.io.File

class TestData {
  @Test
  fun test(file: File) {
    val text = file.readText()
    val kotlinParsing = KotlinParsing.createForTopLevel(PsiBuilderImpl(text))
    kotlinParsing.parseFile()

    val assert = File(file.parent, file.nameWithoutExtension + ".txt").readText()
  }
}