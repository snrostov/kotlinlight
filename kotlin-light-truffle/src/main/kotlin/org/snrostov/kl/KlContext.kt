package org.snrostov.kl

import com.oracle.truffle.api.TruffleLanguage
import org.snrostov.kl.KlLanguage

class KlContext(
        val klLanguage: KlLanguage,
        val env: TruffleLanguage.Env?
)
