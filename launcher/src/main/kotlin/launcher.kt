package org.snrostov.kl

import org.graalvm.polyglot.Context
import org.graalvm.polyglot.Source
import java.io.File
import java.io.InputStreamReader

const val langId = "KtLight"

fun main(args: Array<String>) {
    val options = mutableMapOf<String, String>()
    var file: String? = null
    for (arg in args) {
        if (parseOption(options, arg)) continue
        else if (file == null) file = arg
        else error("Only one input file supported")
    }

    val source = when (file) {
        null -> Source.newBuilder(langId, InputStreamReader(System.`in`), "<stdin>").build()
        else -> Source.newBuilder(langId, File(file)).build()
    }

    Context.newBuilder(langId).`in`(System.`in`).out(System.out).options(options).build().use { context ->
        System.out.println("== running on " + context.engine)
        val result = context.eval(source)
        if (context.getBindings(langId).getMember("org.snrostov.kl.main") == null) {
            System.err.println("No function org.snrostov.kl.main() defined input org.snrostov.kl.SL source file.")
        }
        if (!result.isNull) {
            System.out.println(result.toString())
        }
    }
}

private fun parseOption(options: MutableMap<String, String>, arg: String): Boolean {
    if (arg.length <= 2 || !arg.startsWith("--")) {
        return false
    }
    val eqIdx = arg.indexOf('=')
    val key: String
    var value: String?
    if (eqIdx < 0) {
        key = arg.substring(2)
        value = null
    } else {
        key = arg.substring(2, eqIdx)
        value = arg.substring(eqIdx + 1)
    }

    if (value == null) {
        value = "true"
    }
    val index = key.indexOf('.')
    var group = key
    if (index >= 0) {
        group = group.substring(0, index)
    }
    options[key] = value
    return true
}