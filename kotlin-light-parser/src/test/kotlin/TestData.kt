import org.jetbrains.kotlin.parsing.KotlinParsing
import org.junit.Assert
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.snrostov.kl.IndentedAppendable
import org.snrostov.kl.PsiBuilderImpl
import java.io.File

val dir = File("/Users/sergey/p/kotlinlight/kotlin-light-parser/src/test/testData/psi")
val dirPath = dir.path + "/"

@RunWith(Parameterized::class)
class TestData(val fileName: String) {
  @Test
  fun assertTree() {
    val file = File(dirPath + fileName)
    val text = file.readText()
    val builderImpl = PsiBuilderImpl(text)
    val kotlinParsing = KotlinParsing.createForTopLevel(builderImpl)
    kotlinParsing.parseFile()

    val out = IndentedAppendable()
    builderImpl.root.children.single().printTo(out)
    val assert = File(file.parent, file.nameWithoutExtension + ".txt").readText()

    Assert.assertEquals(assert, out.out)
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