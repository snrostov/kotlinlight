class Foo {
  private var field1: Int = 1
  private val field2: String? = null


  init {
    field1 = 2;
  }

  fun foo1() {
    run {


      field1
    }

    when (field1) {
      1 -> println("1")
      2 -> println("2")
      3 ->
        println(
          "3" +
              "4"
        )
    }

    when (field2) {
      1 -> {
        println("1")
      }

      2 -> {
        println("2")
      }
    }
  }


  class InnerClass {
  }
}


class AnotherClass {
}

interface TestInterface {
}

fun run(f: () -> Unit) {
  f()
}