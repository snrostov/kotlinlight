/*
 * Copyright 2010-2015 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.jetbrains.kotlin.parsing

import com.intellij.lang.PsiBuilder
import com.intellij.psi.tree.IElementType
import com.intellij.psi.tree.TokenSet
import com.intellij.util.containers.Stack
import org.jetbrains.annotations.TestOnly
import org.jetbrains.kotlin.lexer.KtKeywordToken
import org.jetbrains.kotlin.lexer.KtToken
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.lexer.KtTokens.*
import org.jetbrains.kotlin.parsing.trash.*
import java.util.*

abstract class AbstractKotlinParsing(protected val myBuilder: SemanticWhitespaceAwarePsiBuilder) {

  protected val lastToken: IElementType?
    get() {
      var i = 1
      val currentOffset = myBuilder.currentOffset
      while (i <= currentOffset && WHITE_SPACE_OR_COMMENT_BIT_SET.contains(myBuilder.rawLookup(-i))) {
        i++
      }
      return myBuilder.rawLookup(-i)
    }

  protected fun mark(): PsiBuilder.Marker {
    return myBuilder.mark()
  }

  protected fun error(message: String) {
    myBuilder.error(message)
  }

  @JvmOverloads
  protected fun expect(expectation: KtToken, message: String, recoverySet: TokenSet? = null): Boolean {
    if (at(expectation)) {
      advance() // expectation
      return true
    }

    if (expectation === KtTokens.IDENTIFIER && "`" == myBuilder.tokenText) {
      advance()
    }

    errorWithRecovery(message, recoverySet)

    return false
  }

  protected fun expectNoAdvance(expectation: KtToken, message: String) {
    if (at(expectation)) {
      advance() // expectation
      return
    }

    error(message)
  }

  protected fun errorWithRecovery(message: String, recoverySet: TokenSet?) {
    val tt = tt()
    if (recoverySet == null ||
      recoverySet.contains(tt) ||
      tt === LBRACE || tt === RBRACE ||
      recoverySet.contains(EOL_OR_SEMICOLON) && (eof() || tt === SEMICOLON || myBuilder.newlineBeforeCurrentToken())
    ) {
      error(message)
    } else {
      errorAndAdvance(message)
    }
  }

  @JvmOverloads
  protected fun errorAndAdvance(message: String, advanceTokenCount: Int = 1) {
    val err = mark()
    advance(advanceTokenCount)
    err.error(message)
  }

  protected fun eof(): Boolean {
    return myBuilder.eof()
  }

  protected fun advance() {
    // TODO: how to report errors on bad characters? (Other than highlighting)
    myBuilder.advanceLexer()
  }

  protected fun advance(advanceTokenCount: Int) {
    for (i in 0 until advanceTokenCount) {
      advance() // erroneous token
    }
  }

  protected fun advanceAt(current: IElementType) {
    assert(_at(current))
    myBuilder.advanceLexer()
  }

  protected fun tt(): IElementType? {
    return myBuilder.tokenType
  }

  /**
   * Side-effect-free version of at()
   */
  protected fun _at(expectation: IElementType): Boolean {
    val token = tt()
    return tokenMatches(token, expectation)
  }

  private fun tokenMatches(token: IElementType?, expectation: IElementType): Boolean {
    if (token === expectation) return true
    if (expectation === EOL_OR_SEMICOLON) {
      if (eof()) return true
      if (token === SEMICOLON) return true
      if (myBuilder.newlineBeforeCurrentToken()) return true
    }
    return false
  }

  protected fun at(expectation: IElementType): Boolean {
    if (_at(expectation)) return true
    val token = tt()
    if (token === IDENTIFIER && expectation is KtKeywordToken) {
      if (expectation.isSoft && expectation.value == myBuilder.tokenText) {
        myBuilder.remapCurrentToken(expectation)
        return true
      }
    }
    if (expectation === IDENTIFIER && token is KtKeywordToken) {
      val keywordToken = token as KtKeywordToken?
      if (keywordToken!!.isSoft) {
        myBuilder.remapCurrentToken(IDENTIFIER)
        return true
      }
    }
    return false
  }

  /**
   * Side-effect-free version of atSet()
   */
  protected fun _atSet(vararg tokens: IElementType): Boolean {
    return _atSet(TokenSet.create(*tokens))
  }

  /**
   * Side-effect-free version of atSet()
   */
  private fun _atSet(set: TokenSet): Boolean {
    val token = tt()
    if (set.contains(token)) return true
    if (set.contains(EOL_OR_SEMICOLON)) {
      if (eof()) return true
      if (token === SEMICOLON) return true
      if (myBuilder.newlineBeforeCurrentToken()) return true
    }
    return false
  }

  protected fun atSet(vararg tokens: IElementType): Boolean {
    return atSet(TokenSet.create(*tokens))
  }

  protected fun atSet(set: TokenSet): Boolean {
    if (_atSet(set)) return true
    val token = tt()
    if (token === IDENTIFIER) {
      val keywordToken = SOFT_KEYWORD_TEXTS[myBuilder.tokenText]
      if (keywordToken != null && set.contains(keywordToken)) {
        myBuilder.remapCurrentToken(keywordToken)
        return true
      }
    } else {
      // We know at this point that <code>set</code> does not contain <code>token</code>
      if (set.contains(IDENTIFIER) && token is KtKeywordToken) {
        if (token.isSoft) {
          myBuilder.remapCurrentToken(IDENTIFIER)
          return true
        }
      }
    }
    return false
  }

  protected fun lookahead(k: Int): IElementType? {
    return myBuilder.lookAhead(k)
  }

  protected fun consumeIf(token: KtToken): Boolean {
    if (at(token)) {
      advance() // token
      return true
    }
    return false
  }

  // TODO: Migrate to predicates
  protected fun skipUntil(tokenSet: TokenSet) {
    val stopAtEolOrSemi = tokenSet.contains(EOL_OR_SEMICOLON)
    while (!eof() && !tokenSet.contains(tt()) && !(stopAtEolOrSemi && at(EOL_OR_SEMICOLON))) {
      advance()
    }
  }

  protected fun errorUntil(message: String, tokenSet: TokenSet) {
    assert(tokenSet.contains(LBRACE)) { "Cannot include LBRACE into error element!" }
    assert(tokenSet.contains(RBRACE)) { "Cannot include RBRACE into error element!" }
    val error = mark()
    skipUntil(tokenSet)
    error.error(message)
  }

  protected inner class OptionalMarker(actuallyMark: Boolean) {
    private val marker: PsiBuilder.Marker?
    private val offset: Int

    init {
      marker = if (actuallyMark) mark() else null
      offset = myBuilder.currentOffset
    }

    fun done(elementType: IElementType) {
      if (marker == null) return
      marker.done(elementType)
    }

    fun error(message: String) {
      if (marker == null) return
      if (offset == myBuilder.currentOffset) {
        marker.drop() // no empty errors
      } else {
        marker.error(message)
      }
    }

    fun drop() {
      if (marker == null) return
      marker.drop()
    }
  }

  protected fun matchTokenStreamPredicate(pattern: TokenStreamPattern): Int {
    val currentPosition = mark()
    val opens = Stack<IElementType>()
    var openAngleBrackets = 0
    var openBraces = 0
    var openParentheses = 0
    var openBrackets = 0
    while (!eof()) {
      if (pattern.processToken(
          myBuilder.currentOffset,
          pattern.isTopLevel(openAngleBrackets, openBrackets, openBraces, openParentheses)
        )
      ) {
        break
      }
      if (at(LPAR)) {
        openParentheses++
        opens.push(LPAR)
      } else if (at(LT)) {
        openAngleBrackets++
        opens.push(LT)
      } else if (at(LBRACE)) {
        openBraces++
        opens.push(LBRACE)
      } else if (at(LBRACKET)) {
        openBrackets++
        opens.push(LBRACKET)
      } else if (at(RPAR)) {
        openParentheses--
        if (opens.isEmpty() || opens.pop() !== LPAR) {
          if (pattern.handleUnmatchedClosing(RPAR)) {
            break
          }
        }
      } else if (at(GT)) {
        openAngleBrackets--
      } else if (at(RBRACE)) {
        openBraces--
      } else if (at(RBRACKET)) {
        openBrackets--
      }
      advance() // skip token
    }

    currentPosition.rollbackTo()

    return pattern.result()
  }

  protected fun eol(): Boolean {
    return myBuilder.newlineBeforeCurrentToken() || eof()
  }

  protected abstract fun create(builder: SemanticWhitespaceAwarePsiBuilder): KotlinParsing

  protected fun createTruncatedBuilder(eofPosition: Int): KotlinParsing {
    return create(TruncatedSemanticWhitespaceAwarePsiBuilder(myBuilder, eofPosition))
  }

  protected inner class At @JvmOverloads constructor(
    private val lookFor: IElementType,
    private val topLevelOnly: Boolean = true
  ) : AbstractTokenStreamPredicate() {

    override fun matching(topLevel: Boolean): Boolean {
      return (topLevel || !topLevelOnly) && at(lookFor)
    }

  }

  protected inner class AtSet @JvmOverloads constructor(
    private val lookFor: TokenSet,
    private val topLevelOnly: TokenSet = lookFor
  ) : AbstractTokenStreamPredicate() {

    override fun matching(topLevel: Boolean): Boolean {
      return (topLevel || !atSet(topLevelOnly)) && atSet(lookFor)
    }
  }

  @TestOnly
  fun currentContext(): String {
    return myBuilder.originalText.substringWithContext(myBuilder.currentOffset, myBuilder.currentOffset, 20)
  }

  companion object {
    private val SOFT_KEYWORD_TEXTS = HashMap<String, KtKeywordToken>()

    init {
      for (type in KtTokens.SOFT_KEYWORDS.types) {
        val keywordToken = type as KtKeywordToken
        assert(keywordToken.isSoft)
        SOFT_KEYWORD_TEXTS[keywordToken.value] = keywordToken
      }
    }

    init {
      for (token in KtTokens.KEYWORDS.types) {
        assert(token is KtKeywordToken) { "Must be KtKeywordToken: $token" }
        assert(!(token as KtKeywordToken).isSoft) { "Must not be soft: $token" }
      }
    }

    fun errorIf(marker: PsiBuilder.Marker, condition: Boolean, message: String) {
      if (condition) {
        marker.error(message)
      } else {
        marker.drop()
      }
    }

    fun closeDeclarationWithCommentBinders(
      marker: PsiBuilder.Marker,
      elementType: IElementType,
      precedingNonDocComments: Boolean
    ) {
      marker.done(elementType)
      marker.setCustomEdgeTokenBinders(
        if (precedingNonDocComments) PrecedingCommentsBinder else PrecedingDocCommentsBinder,
        TrailingCommentsBinder
      )
    }
  }
}
