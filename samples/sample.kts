interface X<T>

class Test<T: Int> : X<T> {
  fun test(x: String): Int {
    val y = 4
    println("test: ${2 + 2} = $y")
    return 1
  }
}