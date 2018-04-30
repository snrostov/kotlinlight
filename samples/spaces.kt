open class Some {
  private val f: (Int) -> Int = { (a: Int) -> a * 2 }
  fun foo(): Int {
    val test: Int = 12
    for (i in 10..42) {
      println(
        when {
          i < test -> -1
          i > test -> 1
          else -> 0
        }
      )
    }
    if (true) {
    }
    while (true) {
      break
    }
    try {
      when (test) {
        12 -> println("foo")
        else -> println("bar")
      }
    } catch (e: Exception) {
    } finally {
    }
    return test
  }

  private fun <T> foo2(): Int where T : List<T> {
    return 0
  }

  fun multilineMethod(
    foo: String,
    bar: String
  ) {
    foo
      .length
  }

  fun expressionBodyMethod() =
    "abc"
}

class AnotherClass<T : Any> : Some()