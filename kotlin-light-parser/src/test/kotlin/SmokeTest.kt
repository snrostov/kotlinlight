import org.jetbrains.kotlin.parsing.KotlinParsing
import org.junit.Test
import org.snrostov.kl.IndentedAppendable
import org.snrostov.kl.PsiBuilderImpl
import java.io.File

class SmokeTest {
  @Test
  fun test() {
    val text = File("/Users/sergey/p/kotlinlight/samples/sample.kts").readText()
    val psiBuilderImpl = PsiBuilderImpl(text)
    val kotlinParsing = KotlinParsing.createForTopLevel(psiBuilderImpl)
    kotlinParsing.parseFile()
    psiBuilderImpl.root.children.single().printTo(IndentedAppendable(System.out))
  }
}