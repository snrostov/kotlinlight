package org.snrostov.kl

import com.oracle.truffle.api.CallTarget
import com.oracle.truffle.api.Truffle
import com.oracle.truffle.api.TruffleLanguage
import com.oracle.truffle.api.debug.DebuggerTags
import com.oracle.truffle.api.instrumentation.ProvidedTags
import com.oracle.truffle.api.instrumentation.StandardTags
import com.oracle.truffle.api.interop.TruffleObject
import com.oracle.truffle.api.source.Source
import org.snrostov.kl.nodes.KlEvalRootNode

const val langId = "KtLight"
const val langMimeType = "application/kotlin-light"

@TruffleLanguage.Registration(
        id = langId,
        name = "KotlinLight",
        version = "0.10",
        mimeType = [langMimeType]
)
@ProvidedTags(
        StandardTags.CallTag::class,
        StandardTags.StatementTag::class,
        StandardTags.RootTag::class,
        StandardTags.ExpressionTag::class,
        DebuggerTags.AlwaysHalt::class
)
class KlLanguage : TruffleLanguage<KlContext>() {
    override fun createContext(env: Env?) = KlContext(this, env)

    override fun isObjectOfLanguage(`object`: Any?): Boolean {
        `object` as TruffleObject ?: return false
        return true
    }

    override fun parse(request: ParsingRequest): CallTarget {
        val source = request.source

        /*
         * Parse the provided source. At this point, we do not have a SLContext yet. Registration of
         * the functions with the SLContext happens lazily in SLEvalRootNode.
         */
        val functions = when {
            request.argumentNames.isEmpty() -> parse(source)
            else -> {
                val src = request.source
                val args = request.argumentNames.joinToString()
                val decoratedSrc = Source.newBuilder("function main($args) { return ${src.characters};}")
                        .language(src.language ?: langId)
                        .mimeType(src.mimeType ?: langMimeType)
                        .name(src.name)
                        .build()

                parse(decoratedSrc)
            }
        }

        val main = functions["main"]
        val evalMain = when {
            main != null -> KlEvalRootNode(this, main, functions)
            else -> KlEvalRootNode(this, null, functions)
        }

        return Truffle.getRuntime().createCallTarget(evalMain)
    }
}