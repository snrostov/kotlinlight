package com.intellij.psi.tree

open class IElementType(val debugName: String) {
    val index: Int = list.size

    init {
        list.add(this)
    }

    companion object {
        val list = mutableListOf<IElementType>()
    }
}