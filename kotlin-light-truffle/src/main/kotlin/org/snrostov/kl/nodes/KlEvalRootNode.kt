package org.snrostov.kl.nodes

import com.oracle.truffle.api.CompilerDirectives
import com.oracle.truffle.api.RootCallTarget
import com.oracle.truffle.api.TruffleLanguage
import com.oracle.truffle.api.frame.VirtualFrame
import com.oracle.truffle.api.nodes.DirectCallNode
import com.oracle.truffle.api.nodes.RootNode
import org.snrostov.kl.KlContext
import org.snrostov.kl.KlLanguage

class KlEvalRootNode(
        val lang: KlLanguage,
        val mainCallNode: RootCallTarget?,
        val functions: Map<String, RootCallTarget>
) : RootNode(null /*internal frame*/) {
    private val reference = lang.contextReference;

    @CompilerDirectives.CompilationFinal
    private var registered: Boolean = false

    override fun isInstrumentable() = false

    override fun getName() = "root eval"

    override fun toString() = name

    override fun execute(frame: VirtualFrame): Any {
        /* Lazy registrations of functions on first execution. */
        if (!registered) {
            /* Function registration is a slow-path operation that must not be compiled. */
            CompilerDirectives.transferToInterpreterAndInvalidate()
//            reference.get().getFunctionRegistry().register(functions)
            registered = true
        }

        return if (mainCallNode == null) 0 else mainCallNode.call(frame.arguments)
    }
}