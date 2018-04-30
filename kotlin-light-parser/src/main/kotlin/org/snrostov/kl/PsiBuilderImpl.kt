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

package org.snrostov.kl

import com.intellij.lang.PsiBuilder
import com.intellij.lang.WhitespacesAndCommentsBinder
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType
import com.intellij.psi.tree.TokenSet
import com.intellij.util.containers.Stack
import org.jetbrains.kotlin.lexer.KotlinLexer
import org.jetbrains.kotlin.lexer.KtToken
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.lexer.KtTokens.*
import org.jetbrains.kotlin.parsing.trash.SemanticWhitespaceAwarePsiBuilder

val myWhitespaces: TokenSet = KtTokens.WHITESPACES
val myComments: TokenSet = KtTokens.COMMENTS
val myWhitespacesOrComments: TokenSet = TokenSet.orSet(myComments, myWhitespaces)


class PsiBuilderImpl(
  val text: CharSequence,
  lexer: KotlinLexer = KotlinLexer()
) : SemanticWhitespaceAwarePsiBuilder {
  private val tokens = TokenSequence.Builder(text, lexer).performLexing()

  var pos = -1
  val size = tokens.lexemeCount
  val lexType = tokens.lexTypes
  val lexPos = tokens.lexStarts

  private val complexTokens = TokenSet.create(SAFE_ACCESS, ELVIS, EXCLEXCL)

  private val joinComplexTokens = Stack<Boolean>()

  private val newlinesEnabled = Stack<Boolean>()

  init {
    newlinesEnabled.push(true)
    joinComplexTokens.push(true)

    pos++
    while (myWhitespacesOrComments.contains(lexType[pos])) pos++
  }

  val root = mark()

  private fun doGetTokenType() =
    if (pos >= size) null else lexType[pos]

  private fun doLookAhead(steps: Int): IElementType? {
    var i = 0
    var pos = pos
    while (i < steps) {
      if (!myWhitespacesOrComments.contains(lexType[pos])) i++
      pos++
    }
    return lexType[pos]
  }

  private fun doGetTokenText(): String? {
    val startIndex = lexPos[pos]
    val endIndex = lexPos[pos + 1]
    if (endIndex < startIndex) return null
    return text.subSequence(startIndex, endIndex).toString()
  }

  private fun lexNode() {
    val type = lexType[pos]
    if (type != null) {
      val id = Node(pos, leaf, true).also {
        leaf = it
      }

      pos++

      id.done(type)
    } else {
      pos++
    }
  }

  private fun doAdvanceLexer() {
    lexNode()

    while (!eof() && myWhitespacesOrComments.contains(lexType[pos])) {
      lexNode()
    }
  }

  override fun getOriginalText(): CharSequence? = text

  override fun remapCurrentToken(type: IElementType) {
    lexType[pos] = type
  }

  fun guardPos(pos: Int) = when {
    pos < 0 -> 0
    pos > size - 1 -> size - 1
    else -> pos
  }

  override fun rawLookup(steps: Int): IElementType? = lexType[guardPos(pos + steps)]

  override fun rawTokenTypeStart(steps: Int): Int = lexPos[guardPos(pos + steps)]

  override fun rawTokenIndex(): Int = pos

  override fun getCurrentOffset(): Int = lexPos[pos]

  var leaf: Node? = null

  override fun mark(): Node {
    return Node(pos, leaf).also {
      leaf = it
    }
  }

  inner class Node(val start: Int, var parent: Node?, val lex: Boolean = false) : PsiBuilder.Marker {
    var open = true
    var end = -1

    var type: IElementType? = null

    val children = mutableListOf<Node>()

    init {
      parent?.children?.add(this)
    }

    override fun precede(): Node {
      check(parent!!.children.remove(this))
      return Node(start, parent).also {
        parent = it
        it.children.add(this)
      }
    }

    override fun drop() {
      if (open) {
        val parent = this.parent!!
        check(parent.children.remove(this))

        parent.children.addAll(children)
        children.forEach {
          it.parent = parent
        }

        leaf = parent
        open = false
      }
    }

    override fun rollbackTo() {
      check(open)

      pos = start
      leaf = parent

      val parent = this.parent!!
      check(parent.children.remove(this))

//      dropWithChildren()
    }

    private fun dropWithChildren() {
      check(open)
      open = false
      children.forEach {
        if (it.open) {
          it.dropWithChildren()
        }
      }
    }

    override fun done(type: IElementType) {
      check(open)
//      checkChildClosed()

      end = pos

      this.type = type
      leaf = parent

      dropWithChildren()
    }

    private fun checkChildClosed() {
      children.forEach {
        check(!it.open)
        it.checkChildClosed()
      }
    }

    override fun collapse(type: IElementType) = done(type)

    var error: String? = null

    override fun error(message: String?) {
      error = message
    }

    var left: WhitespacesAndCommentsBinder? = null
    var right: WhitespacesAndCommentsBinder? = null

    override fun setCustomEdgeTokenBinders(left: WhitespacesAndCommentsBinder?, right: WhitespacesAndCommentsBinder?) {
      this.left = left
      this.right = right
    }

    override fun toString(): String {
      return "$type: " +
          (if (error != null) "[ERROR: $error]" else "") +
          text
    }

    val text: String
      get() = if (!open) this@PsiBuilderImpl.text.substring(lexPos[start], lexPos[end])
      else this@PsiBuilderImpl.text.substring(lexPos[start], this@PsiBuilderImpl.text.length)

    fun printTo(out: IndentedAppendable) {
      val typeTitle = type?.debugName?.toLowerCase()
      if (lex) {
        val text = text.replace("\n", "\\n")
        out.appendln("\"$text\" $typeTitle")
      } else {
        out.appendln(typeTitle ?: "<null>")
        out.nested { nested ->
          children.forEach {
            it.printTo(nested)
          }
        }
      }
    }

  }

  override fun error(messageText: String) {

  }

  override fun eof() = pos >= size

  override fun isWhitespaceOrComment(token: IElementType): Boolean {
    return myWhitespaces.contains(token) || myComments.contains(token)
  }

  /////////

  override fun newlineBeforeCurrentToken(): Boolean {
    if (!newlinesEnabled.peek()) return false

    if (eof()) return true

    // TODO: maybe, memoize this somehow?
    for (i in 1..currentOffset) {
      val previousToken = rawLookup(-i)

      if (previousToken === KtTokens.BLOCK_COMMENT
        || previousToken === KtTokens.DOC_COMMENT
        || previousToken === KtTokens.EOL_COMMENT
        || previousToken === SHEBANG_COMMENT
      ) {
        continue
      }

      if (previousToken !== TokenType.WHITE_SPACE) {
        break
      }

      val previousTokenStart = rawTokenTypeStart(-i)
      val previousTokenEnd = rawTokenTypeStart(-i + 1)

      assert(previousTokenStart >= 0)
      assert(previousTokenEnd < originalText!!.length)

      for (j in previousTokenStart until previousTokenEnd) {
        if (originalText!![j] == '\n') {
          return true
        }
      }
    }

    return false
  }

  override fun disableNewlines() {
    newlinesEnabled.push(false)
  }

  override fun enableNewlines() {
    newlinesEnabled.push(true)
  }

  override fun restoreNewlinesState() {
    assert(newlinesEnabled.size > 1)
    newlinesEnabled.pop()
  }

  private fun joinComplexTokens(): Boolean {
    return joinComplexTokens.peek()
  }

  override fun restoreJoiningComplexTokensState() {
    joinComplexTokens.pop()
  }

  override fun enableJoiningComplexTokens() {
    joinComplexTokens.push(true)
  }

  override fun disableJoiningComplexTokens() {
    joinComplexTokens.push(false)
  }


  override fun getTokenType(): IElementType? =
    if (!joinComplexTokens()) doGetTokenType()
    else getJoinedTokenType(doGetTokenType(), 1)

  private fun getJoinedTokenType(rawTokenType: IElementType?, rawLookupSteps: Int): IElementType? {
    if (rawTokenType === QUEST) {
      val nextRawToken = rawLookup(rawLookupSteps)
      if (nextRawToken === DOT) return SAFE_ACCESS
      if (nextRawToken === COLON) return ELVIS
    } else if (rawTokenType === EXCL) {
      val nextRawToken = rawLookup(rawLookupSteps)
      if (nextRawToken === EXCL) return EXCLEXCL
    }
    return rawTokenType
  }

  override fun advanceLexer() {
    if (!joinComplexTokens()) {
      doAdvanceLexer()
      return
    }
    val tokenType = getTokenType()
    if (complexTokens.contains(tokenType)) {
      val mark = mark()
      doAdvanceLexer()
      doAdvanceLexer()
      mark.collapse(tokenType!!)
    } else {
      doAdvanceLexer()
    }
  }


  override fun getTokenText(): String? {
    if (!joinComplexTokens()) return doGetTokenText()
    val tokenType = getTokenType()
    if (complexTokens.contains(tokenType)) {
      if (tokenType === ELVIS) return "?:"
      if (tokenType === SAFE_ACCESS) return "?."
    }
    return doGetTokenText()
  }


  override fun lookAhead(steps: Int): IElementType? {
    if (!joinComplexTokens()) return doLookAhead(steps)

    return if (complexTokens.contains(getTokenType())) {
      doLookAhead(steps + 1)
    } else getJoinedTokenType(doLookAhead(steps), 2)
  }

}