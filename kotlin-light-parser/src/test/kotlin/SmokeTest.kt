import org.jetbrains.kotlin.parsing.KotlinParsing
import org.junit.Test
import org.snrostov.kl.PsiBuilderImpl
import java.io.File

class SmokeTest {
  @Test
  fun test() {
    val text = File("/Users/sergey/p/kotlinlight/samples/wrapping.kt").readText()
    val kotlinParsing = KotlinParsing.createForTopLevel(PsiBuilderImpl(text))
    kotlinParsing.parseFile()
    println("test")
  }
}