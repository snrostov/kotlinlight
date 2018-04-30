/**
 * Copyright 2010-2017 JetBrains s.r.o.
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
import com.intellij.lang.WhitespacesBinders
import com.intellij.psi.tree.IElementType
import com.intellij.psi.tree.TokenSet
import org.jetbrains.annotations.Contract
import org.jetbrains.kotlin.KtNodeTypes.*
import org.jetbrains.kotlin.lexer.KtKeywordToken
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.lexer.KtTokens.*
import org.jetbrains.kotlin.parsing.KotlinParsing.AnnotationParsingMode.*
import org.jetbrains.kotlin.parsing.trash.*

private val TOP_LEVEL_DECLARATION_FIRST = TokenSet.create(
  TYPE_ALIAS_KEYWORD, INTERFACE_KEYWORD, CLASS_KEYWORD, OBJECT_KEYWORD,
  FUN_KEYWORD, VAL_KEYWORD, PACKAGE_KEYWORD
)
private val DECLARATION_FIRST = TokenSet.orSet(
  TOP_LEVEL_DECLARATION_FIRST,
  TokenSet.create(INIT_KEYWORD, GET_KEYWORD, SET_KEYWORD, CONSTRUCTOR_KEYWORD)
)

private val CLASS_NAME_RECOVERY_SET = TokenSet.orSet(
  TokenSet.create(LT, LPAR, COLON, LBRACE),
  TOP_LEVEL_DECLARATION_FIRST
)
private val TYPE_PARAMETER_GT_RECOVERY_SET = TokenSet.create(WHERE_KEYWORD, LPAR, COLON, LBRACE, GT)
private val PARAMETER_NAME_RECOVERY_SET = TokenSet.create(COLON, EQ, COMMA, RPAR, VAL_KEYWORD, VAR_KEYWORD)
private val PACKAGE_NAME_RECOVERY_SET = TokenSet.create(DOT, EOL_OR_SEMICOLON)
private val IMPORT_RECOVERY_SET = TokenSet.create(AS_KEYWORD, DOT, EOL_OR_SEMICOLON)
private val TYPE_REF_FIRST = TokenSet.create(LBRACKET, IDENTIFIER, LPAR, HASH, DYNAMIC_KEYWORD)
private val RECEIVER_TYPE_TERMINATORS = TokenSet.create(DOT, SAFE_ACCESS)
private val VALUE_PARAMETER_FIRST =
  TokenSet.orSet(TokenSet.create(IDENTIFIER, LBRACKET, VAL_KEYWORD, VAR_KEYWORD), MODIFIER_KEYWORDS)
private val LAMBDA_VALUE_PARAMETER_FIRST = TokenSet.orSet(TokenSet.create(IDENTIFIER, LBRACKET), MODIFIER_KEYWORDS)
private val SOFT_KEYWORDS_AT_MEMBER_START = TokenSet.create(CONSTRUCTOR_KEYWORD, INIT_KEYWORD)
private val ANNOTATION_TARGETS = TokenSet.create(
  FILE_KEYWORD, FIELD_KEYWORD, GET_KEYWORD, SET_KEYWORD, PROPERTY_KEYWORD,
  RECEIVER_KEYWORD, PARAM_KEYWORD, SETPARAM_KEYWORD, DELEGATE_KEYWORD
)

fun topLevelKotlinParsing(builder: SemanticWhitespaceAwarePsiBuilder): KotlinParsing {
  val kotlinParsing = KotlinParsing(builder)
  kotlinParsing.myExpressionParsing = KotlinExpressionParsing(builder, kotlinParsing)
  return kotlinParsing
}

private fun createForByClause(builder: SemanticWhitespaceAwarePsiBuilder): KotlinParsing {
  val builderForByClause = SemanticWhitespaceAwarePsiBuilderForByClause(builder)
  val kotlinParsing = KotlinParsing(builderForByClause)
  kotlinParsing.myExpressionParsing = object : KotlinExpressionParsing(builderForByClause, kotlinParsing) {
    override fun parseCallWithClosure(): Boolean {
      return if (builderForByClause.stackSize > 0) {
        super.parseCallWithClosure()
      } else false
    }

    override fun create(builder: SemanticWhitespaceAwarePsiBuilder): KotlinParsing {
      return createForByClause(builder)
    }
  }
  return kotlinParsing
}

private val NO_MODIFIER_BEFORE_FOR_VALUE_PARAMETER = TokenSet.create(COMMA, COLON, EQ, RPAR)

class KotlinParsing internal constructor(builder: SemanticWhitespaceAwarePsiBuilder) : AbstractKotlinParsing(builder) {
  internal var myExpressionParsing: KotlinExpressionParsing? = null

  /**
   * [start] kotlinFile
   *   : preamble toplevelObject* [eof]
   *   ;
   */
  fun parseFile() {
    val fileMarker = mark()

    parsePreamble()

    while (!eof()) {
      parseTopLevelDeclaration()
    }

    checkUnclosedBlockComment()
    fileMarker.done(KT_FILE)
  }

  private fun checkUnclosedBlockComment() {
    if (TokenSet.create(BLOCK_COMMENT, DOC_COMMENT).contains(myBuilder.rawLookup(-1))) {
      val startOffset = myBuilder.rawTokenTypeStart(-1)
      val endOffset = myBuilder.rawTokenTypeStart(0)
      val tokenChars = myBuilder.originalText.subSequence(startOffset, endOffset)
      if (!(tokenChars.length > 2 && tokenChars.subSequence(
          tokenChars.length - 2,
          tokenChars.length
        ).toString() == "*/")
      ) {
        val marker = myBuilder.mark()
        marker.error("Unclosed comment")
        marker.setCustomEdgeTokenBinders(WhitespacesBinders.GREEDY_RIGHT_BINDER, null)
      }
    }
  }

  fun parseTypeCodeFragment() {
    val marker = mark()
    parseTypeRef()

    checkForUnexpectedSymbols()

    marker.done(TYPE_CODE_FRAGMENT)
  }

  fun parseExpressionCodeFragment() {
    val marker = mark()
    myExpressionParsing!!.parseExpression()

    checkForUnexpectedSymbols()

    marker.done(EXPRESSION_CODE_FRAGMENT)
  }

  fun parseBlockCodeFragment() {
    val marker = mark()
    val blockMarker = mark()

    if (at(PACKAGE_KEYWORD) || at(IMPORT_KEYWORD)) {
      val err = mark()
      parsePreamble()
      err.error("Package directive and imports are forbidden in code fragments")
    }

    myExpressionParsing!!.parseStatements()

    checkForUnexpectedSymbols()

    blockMarker.done(BLOCK)
    marker.done(BLOCK_CODE_FRAGMENT)
  }

  fun parseLambdaExpression() {
    myExpressionParsing!!.parseFunctionLiteral(
      /** preferBlock = */
      false,
      /** collapse = */
      false
    )
  }

  fun parseScript() {
    val fileMarker = mark()

    parsePreamble()

    val scriptMarker = mark()

    val blockMarker = mark()

    myExpressionParsing!!.parseStatements(
      /**isScriptTopLevel = */
      true
    )

    checkForUnexpectedSymbols()

    blockMarker.done(BLOCK)
    blockMarker.setCustomEdgeTokenBinders(PRECEDING_ALL_BINDER, TRAILING_ALL_BINDER)

    scriptMarker.done(SCRIPT)
    scriptMarker.setCustomEdgeTokenBinders(PRECEDING_ALL_BINDER, TRAILING_ALL_BINDER)

    fileMarker.done(KT_FILE)
  }

  private fun checkForUnexpectedSymbols() {
    while (!eof()) {
      errorAndAdvance("Unexpected symbol")
    }
  }

  /**
   *preamble
   *  : fileAnnotationList? packageDirective? import*
   *  ;
   */
  private fun parsePreamble() {
    val firstEntry = mark()

    /**
     * fileAnnotationList
     *   : fileAnnotations*
     */
    parseFileAnnotationList(FILE_ANNOTATIONS_BEFORE_PACKAGE)

    /**
     * packageDirective
     *   : modifiers "package" SimpleName{"."} SEMI?
     *   ;
     */
    var packageDirective = mark()
    parseModifierList(DEFAULT, TokenSet.EMPTY)

    if (at(PACKAGE_KEYWORD)) {
      advance() // PACKAGE_KEYWORD

      parsePackageName()

      firstEntry.drop()

      consumeIf(SEMICOLON)

      packageDirective.done(PACKAGE_DIRECTIVE)
    } else {
      // When package directive is omitted we should not report error on non-file annotations at the beginning of the file.
      // So, we rollback the parsing position and reparse file annotation list without report error on non-file annotations.
      firstEntry.rollbackTo()

      parseFileAnnotationList(FILE_ANNOTATIONS_WHEN_PACKAGE_OMITTED)
      packageDirective = mark()
      packageDirective.done(PACKAGE_DIRECTIVE)
      // Need to skip everything but shebang comment to allow comments at the start of the file to be bound to the first declaration.
      packageDirective.setCustomEdgeTokenBinders(BindFirstShebangWithWhitespaceOnly, null)
    }

    parseImportDirectives()
  }

  /** SimpleName{"."} */
  private fun parsePackageName() {
    var qualifiedExpression = mark()
    var simpleName = true
    while (true) {
      if (myBuilder.newlineBeforeCurrentToken()) {
        errorWithRecovery(
          "Package name must be a '.'-separated identifier list placed on a single line",
          PACKAGE_NAME_RECOVERY_SET
        )
        break
      }

      if (at(DOT)) {
        advance() // DOT
        qualifiedExpression.error("Package name must be a '.'-separated identifier list")
        qualifiedExpression = mark()
        continue
      }

      val nsName = mark()
      val simpleNameFound =
        expect(IDENTIFIER, "Package name must be a '.'-separated identifier list", PACKAGE_NAME_RECOVERY_SET)
      if (simpleNameFound) {
        nsName.done(REFERENCE_EXPRESSION)
      } else {
        nsName.drop()
      }

      if (!simpleName) {
        val precedingMarker = qualifiedExpression.precede()
        qualifiedExpression.done(DOT_QUALIFIED_EXPRESSION)
        qualifiedExpression = precedingMarker
      }

      if (at(DOT)) {
        advance() // DOT

        if (simpleName && !simpleNameFound) {
          qualifiedExpression.drop()
          qualifiedExpression = mark()
        } else {
          simpleName = false
        }
      } else {
        break
      }
    }
    qualifiedExpression.drop()
  }

  /**
   * import
   *   : "import" SimpleName{"."} ("." "*" | "as" SimpleName)? SEMI?
   *   ;
   */
  private fun parseImportDirective() {
    assert(_at(IMPORT_KEYWORD))
    val importDirective = mark()
    advance() // IMPORT_KEYWORD

    if (closeImportWithErrorIfNewline(importDirective, null, "Expecting qualified name")) {
      return
    }

    if (!at(IDENTIFIER)) {
      val error = mark()
      skipUntil(TokenSet.create(EOL_OR_SEMICOLON))
      error.error("Expecting qualified name")
      importDirective.done(IMPORT_DIRECTIVE)
      consumeIf(SEMICOLON)
      return
    }

    var qualifiedName = mark()
    var reference = mark()
    advance() // IDENTIFIER
    reference.done(REFERENCE_EXPRESSION)

    while (at(DOT) && lookahead(1) !== MUL) {
      advance() // DOT

      if (closeImportWithErrorIfNewline(importDirective, null, "Import must be placed on a single line")) {
        qualifiedName.drop()
        return
      }

      reference = mark()
      if (expect(IDENTIFIER, "Qualified name must be a '.'-separated identifier list", IMPORT_RECOVERY_SET)) {
        reference.done(REFERENCE_EXPRESSION)
      } else {
        reference.drop()
      }

      val precede = qualifiedName.precede()
      qualifiedName.done(DOT_QUALIFIED_EXPRESSION)
      qualifiedName = precede
    }
    qualifiedName.drop()

    if (at(DOT)) {
      advance() // DOT
      assert(_at(MUL))
      advance() // MUL
      if (at(AS_KEYWORD)) {
        val `as` = mark()
        advance() // AS_KEYWORD
        if (closeImportWithErrorIfNewline(importDirective, null, "Expecting identifier")) {
          `as`.drop()
          return
        }
        consumeIf(IDENTIFIER)
        `as`.done(IMPORT_ALIAS)
        `as`.precede().error("Cannot rename all imported items to one identifier")
      }
    }
    if (at(AS_KEYWORD)) {
      val alias = mark()
      advance() // AS_KEYWORD
      if (closeImportWithErrorIfNewline(importDirective, alias, "Expecting identifier")) {
        return
      }
      expect(IDENTIFIER, "Expecting identifier", TokenSet.create(SEMICOLON))
      alias.done(IMPORT_ALIAS)
    }
    consumeIf(SEMICOLON)
    importDirective.done(IMPORT_DIRECTIVE)
    importDirective.setCustomEdgeTokenBinders(null, TrailingCommentsBinder)
  }

  private fun closeImportWithErrorIfNewline(
    importDirective: PsiBuilder.Marker,
    importAlias: PsiBuilder.Marker?,
    errorMessage: String
  ): Boolean {
    if (myBuilder.newlineBeforeCurrentToken()) {
      importAlias?.done(IMPORT_ALIAS)
      error(errorMessage)
      importDirective.done(IMPORT_DIRECTIVE)
      return true
    }
    return false
  }

  private fun parseImportDirectives() {
    val importList = mark()
    if (!at(IMPORT_KEYWORD)) {
      // this is necessary to allow comments at the start of the file to be bound to the first declaration
      importList.setCustomEdgeTokenBinders(DoNotBindAnything, null)
    }
    while (at(IMPORT_KEYWORD)) {
      parseImportDirective()
    }
    importList.done(IMPORT_LIST)
  }

  /**
   * toplevelObject
   *   : package
   *   : class
   *   : extension
   *   : function
   *   : property
   *   : typeAlias
   *   : object
   *   ;
   */
  private fun parseTopLevelDeclaration() {
    if (at(SEMICOLON)) {
      advance() // SEMICOLON
      return
    }
    val decl = mark()

    val detector = ModifierDetector()
    parseModifierList(detector, DEFAULT, TokenSet.EMPTY)

    val keywordToken = tt()
    var declType: IElementType? = null

    when (keywordToken) {
      CLASS_KEYWORD, INTERFACE_KEYWORD ->
        declType = parseClass(detector.isEnumDetected)
      FUN_KEYWORD -> declType = parseFunction()
      VAL_KEYWORD, VAR_KEYWORD -> declType = parseProperty()
      TYPE_ALIAS_KEYWORD -> declType = parseTypeAlias()
      OBJECT_KEYWORD -> {
        parseObject(NameParsingMode.REQUIRED, true)
        declType = OBJECT_DECLARATION
      }
      else -> if (at(LBRACE)) {
        error("Expecting a top level declaration")
        parseBlock()
        declType = FUN
      }
    }

    if (declType == null) {
      errorAndAdvance("Expecting a top level declaration")
      decl.drop()
    } else {
      AbstractKotlinParsing.Companion.closeDeclarationWithCommentBinders(decl, declType, true)
    }
  }

  /**
   * (modifier | annotation)*
   */
  fun parseModifierList(
    annotationParsingMode: AnnotationParsingMode,
    noModifiersBefore: TokenSet
  ): Boolean {
    return parseModifierList(null, annotationParsingMode, noModifiersBefore)
  }

  /***
   * (modifier | annotation)*
   *
   *
   * Feeds modifiers (not annotations) into the passed consumer, if it is not null
   *
   * @param noModifiersBefore is a token set with elements indicating when met them
   * that previous token must be parsed as an identifier rather than modifier
   */
  fun parseModifierList(
    tokenConsumer: Consumer<IElementType>?,
    annotationParsingMode: AnnotationParsingMode,
    noModifiersBefore: TokenSet
  ): Boolean {
    return doParseModifierList(tokenConsumer, MODIFIER_KEYWORDS, annotationParsingMode, noModifiersBefore)
  }

  private fun parseFunctionTypeValueParameterModifierList(): Boolean {
    return doParseModifierList(
      null,
      RESERVED_VALUE_PARAMETER_MODIFIER_KEYWORDS,
      NO_ANNOTATIONS,
      NO_MODIFIER_BEFORE_FOR_VALUE_PARAMETER
    )
  }

  private fun parseTypeModifierList(): Boolean {
    return doParseModifierList(null, TYPE_MODIFIER_KEYWORDS, DEFAULT, TokenSet.EMPTY)
  }

  private fun parseTypeArgumentModifierList(): Boolean {
    return doParseModifierList(null, TYPE_ARGUMENT_MODIFIER_KEYWORDS, NO_ANNOTATIONS, TokenSet.create(COMMA, COLON, GT))
  }

  private fun doParseModifierList(
    tokenConsumer: Consumer<IElementType>?,
    modifierKeywords: TokenSet,
    annotationParsingMode: AnnotationParsingMode,
    noModifiersBefore: TokenSet
  ): Boolean {
    val list = mark()
    var empty = true
    while (!eof()) {
      if (at(AT) && annotationParsingMode.allowAnnotations) {
        parseAnnotationOrList(annotationParsingMode)
      } else if (tryParseModifier(tokenConsumer, noModifiersBefore, modifierKeywords)) {
        // modifier advanced
      } else {
        break
      }
      empty = false
    }
    if (empty) {
      list.drop()
    } else {
      list.done(MODIFIER_LIST)
    }
    return !empty
  }

  private fun tryParseModifier(
    tokenConsumer: Consumer<IElementType>?,
    noModifiersBefore: TokenSet,
    modifierKeywords: TokenSet
  ): Boolean {
    val marker = mark()

    if (atSet(modifierKeywords)) {
      val lookahead = lookahead(1)
      if (lookahead != null && !noModifiersBefore.contains(lookahead)) {
        val tt = tt()
        tokenConsumer?.consume(tt)
        advance() // MODIFIER
        marker.collapse(tt!!)
        return true
      }
    }

    marker.rollbackTo()
    return false
  }

  /**
   * fileAnnotationList
   *   : ("[" "file:" annotationEntry+ "]")*
   *   ;
   */
  private fun parseFileAnnotationList(mode: AnnotationParsingMode) {
    if (!mode.isFileAnnotationParsingMode) {
      error("expected file annotation parsing mode, but:$mode")
    }

    val fileAnnotationsList = mark()

    if (parseAnnotations(mode)) {
      fileAnnotationsList.done(FILE_ANNOTATION_LIST)
    } else {
      fileAnnotationsList.drop()
    }
  }

  /**
   * annotations
   *   : (annotation | annotationList)*
   *   ;
   */
  fun parseAnnotations(mode: AnnotationParsingMode): Boolean {
    if (!parseAnnotationOrList(mode)) return false

    while (parseAnnotationOrList(mode)) {
      // do nothing
    }

    return true
  }

  /**
   * annotation
   *   : "@" (annotationUseSiteTarget ":")? unescapedAnnotation
   *   ;
   *
   * annotationList
   *   : "@" (annotationUseSiteTarget ":")? "[" unescapedAnnotation+ "]"
   *   ;
   *
   * annotationUseSiteTarget
   *   : "file"
   *   : "field"
   *   : "property"
   *   : "get"
   *   : "set"
   *   : "param"
   *   : "setparam"
   *   ;
   */
  private fun parseAnnotationOrList(mode: AnnotationParsingMode): Boolean {
    if (at(AT)) {
      val nextRawToken = myBuilder.rawLookup(1)
      var tokenToMatch = nextRawToken
      var isTargetedAnnotation = false// AT
      // AT, (ANNOTATION TARGET KEYWORD), COLON

      // AT, COLON
      when {
        (nextRawToken === IDENTIFIER || ANNOTATION_TARGETS.contains(nextRawToken)) && lookahead(2) === COLON -> {
          tokenToMatch = lookahead(3)
          isTargetedAnnotation = true
        }
        lookahead(1) === COLON -> {
          // recovery for "@:ann"
          isTargetedAnnotation = true
          tokenToMatch = lookahead(2)
        }
      }

      when {
        tokenToMatch === IDENTIFIER -> return parseAnnotation(mode)
        tokenToMatch === LBRACKET -> return parseAnnotationList(mode)
        else -> if (isTargetedAnnotation) {
          if (lookahead(1) === COLON) {
            errorAndAdvance("Expected annotation identifier after ':'", 2) // AT, COLON
          } else {
            errorAndAdvance("Expected annotation identifier after ':'", 3) // AT, (ANNOTATION TARGET KEYWORD), COLON
          }
        } else {
          errorAndAdvance("Expected annotation identifier after '@'", 1) // AT
        }
      }
      return true
    }

    return false
  }

  private fun parseAnnotationList(mode: AnnotationParsingMode): Boolean {
    assert(_at(AT))
    val annotation = mark()

    myBuilder.disableNewlines()

    advance() // AT

    if (!parseAnnotationTargetIfNeeded(mode)) {
      annotation.rollbackTo()
      myBuilder.restoreNewlinesState()
      return false
    }

    assert(_at(LBRACKET))
    advance() // LBRACKET

    if (!at(IDENTIFIER) && !at(AT)) {
      error("Expecting a list of annotations")
    } else {
      while (at(IDENTIFIER) || at(AT)) {
        if (at(AT)) {
          errorAndAdvance("No '@' needed in annotation list") // AT
          continue
        }

        parseAnnotation(DEFAULT)
        while (at(COMMA)) {
          errorAndAdvance("No commas needed to separate annotations")
        }
      }
    }

    expect(RBRACKET, "Expecting ']' to close the annotation list")
    myBuilder.restoreNewlinesState()

    annotation.done(ANNOTATION)
    return true
  }

  // Returns true if we should continue parse annotation
  private fun parseAnnotationTargetIfNeeded(mode: AnnotationParsingMode): Boolean {
    val expectedAnnotationTargetBeforeColon = "Expected annotation target before ':'"

    if (at(COLON)) {
      // recovery for "@:ann"
      errorAndAdvance(expectedAnnotationTargetBeforeColon) // COLON
      return true
    }

    val targetKeyword = atTargetKeyword()
    if (mode == FILE_ANNOTATIONS_WHEN_PACKAGE_OMITTED && !(targetKeyword === FILE_KEYWORD && lookahead(1) === COLON)) {
      return false
    }

    if (lookahead(1) === COLON && targetKeyword == null && at(IDENTIFIER)) {
      // recovery for "@fil:ann"
      errorAndAdvance(expectedAnnotationTargetBeforeColon) // IDENTIFIER
      advance() // COLON
      return true
    }

    if (targetKeyword == null && mode.isFileAnnotationParsingMode) {
      parseAnnotationTarget(FILE_KEYWORD)
    } else if (targetKeyword != null) {
      parseAnnotationTarget(targetKeyword)
    }

    return true
  }

  private fun parseAnnotationTarget(keyword: KtKeywordToken) {
    val message = "Expecting \"" + keyword.value + COLON.value + "\" prefix for " + keyword.value + " annotations"

    val marker = mark()

    if (!expect(keyword, message)) {
      marker.drop()
    } else {
      marker.done(ANNOTATION_TARGET)
    }

    expect(COLON, message, TokenSet.create(IDENTIFIER, RBRACKET, LBRACKET))
  }

  private fun atTargetKeyword(): KtKeywordToken? {
    for (target in ANNOTATION_TARGETS.types) {
      if (at(target)) return target as KtKeywordToken
    }
    return null
  }

  /**
   * annotation
   *   : "@" (annotationUseSiteTarget ":")? unescapedAnnotation
   *   ;
   *
   * unescapedAnnotation
   *   : SimpleName{"."} typeArguments? valueArguments?
   *   ;
   */
  private fun parseAnnotation(mode: AnnotationParsingMode): Boolean {
    assert(_at(IDENTIFIER) || _at(AT) && !WHITE_SPACE_OR_COMMENT_BIT_SET.contains(myBuilder.rawLookup(1)))

    val annotation = mark()

    val atAt = at(AT)
    if (atAt) {
      advance() // AT
    }

    if (atAt && !parseAnnotationTargetIfNeeded(mode)) {
      annotation.rollbackTo()
      return false
    }

    val reference = mark()
    val typeReference = mark()
    parseUserType()
    typeReference.done(TYPE_REFERENCE)
    reference.done(CONSTRUCTOR_CALLEE)

    parseTypeArgumentList()

    if (at(LPAR)) {
      myExpressionParsing!!.parseValueArgumentList()
    }
    annotation.done(ANNOTATION_ENTRY)

    return true
  }

  enum class NameParsingMode {
    REQUIRED,
    ALLOWED,
    PROHIBITED
  }

  enum class DeclarationParsingMode {
    TOP_LEVEL,
    CLASS_MEMBER,
    LOCAL
  }

  /**
   * class
   *   : modifiers ("class" | "interface") SimpleName
   *       typeParameters?
   *       primaryConstructor?
   *       (":" annotations delegationSpecifier{","})?
   *       typeConstraints
   *       (classBody? | enumClassBody)
   *   ;
   *
   * primaryConstructor
   *   : (modifiers "constructor")? ("(" functionParameter{","} ")")
   *   ;
   *
   * object
   *   : "object" SimpleName? primaryConstructor? ":" delegationSpecifier{","}? classBody?
   *   ;
   */
  private fun parseClassOrObject(
    `object`: Boolean,
    nameParsingMode: NameParsingMode,
    optionalBody: Boolean,
    enumClass: Boolean
  ): IElementType {
    if (`object`) {
      assert(_at(OBJECT_KEYWORD))
    } else {
      assert(_atSet(CLASS_KEYWORD, INTERFACE_KEYWORD))
    }
    advance() // CLASS_KEYWORD, INTERFACE_KEYWORD or OBJECT_KEYWORD

    if (nameParsingMode == NameParsingMode.REQUIRED) {
      expect(IDENTIFIER, "Name expected", CLASS_NAME_RECOVERY_SET)
    } else {
      assert(`object`) { "Must be an object to be nameless" }
      if (at(IDENTIFIER)) {
        if (nameParsingMode == NameParsingMode.PROHIBITED) {
          errorAndAdvance("An object expression cannot bind a name")
        } else {
          assert(nameParsingMode == NameParsingMode.ALLOWED)
          advance()
        }
      }
    }

    val typeParametersDeclared = parseTypeParameterList(TYPE_PARAMETER_GT_RECOVERY_SET)

    val beforeConstructorModifiers = mark()
    val primaryConstructorMarker = mark()
    val hasConstructorModifiers = parseModifierList(DEFAULT, TokenSet.EMPTY)

    // Some modifiers found, but no parentheses following: class has already ended, and we are looking at something else
    if (hasConstructorModifiers && !atSet(LPAR, LBRACE, COLON, CONSTRUCTOR_KEYWORD)) {
      beforeConstructorModifiers.rollbackTo()
      return if (`object`) OBJECT_DECLARATION else CLASS
    }

    // We are still inside a class declaration
    beforeConstructorModifiers.drop()

    val hasConstructorKeyword = at(CONSTRUCTOR_KEYWORD)
    if (hasConstructorKeyword) {
      advance() // CONSTRUCTOR_KEYWORD
    }

    if (at(LPAR)) {
      parseValueParameterList(
        false,
        /** typeRequired  = */
        true, TokenSet.create(LBRACE, RBRACE)
      )
      primaryConstructorMarker.done(PRIMARY_CONSTRUCTOR)
    } else if (hasConstructorModifiers || hasConstructorKeyword) {
      // A comprehensive error message for cases like:
      //    class A private : Foo
      // or
      //    class A private {
      primaryConstructorMarker.done(PRIMARY_CONSTRUCTOR)
      if (hasConstructorKeyword) {
        error("Expecting primary constructor parameter list")
      } else {
        error("Expecting 'constructor' keyword")
      }
    } else {
      primaryConstructorMarker.drop()
    }

    if (at(COLON)) {
      advance() // COLON
      parseDelegationSpecifierList()
    }

    val whereMarker = OptionalMarker(`object`)
    parseTypeConstraintsGuarded(typeParametersDeclared)
    whereMarker.error("Where clause is not allowed for objects")

    if (at(LBRACE)) {
      if (enumClass) {
        parseEnumClassBody()
      } else {
        parseClassBody()
      }
    } else if (!optionalBody) {
      val fakeBody = mark()
      error("Expecting a class body")
      fakeBody.done(CLASS_BODY)
    }

    return if (`object`) OBJECT_DECLARATION else CLASS
  }

  fun parseClass(enumClass: Boolean): IElementType {
    return parseClassOrObject(false, NameParsingMode.REQUIRED, true, enumClass)
  }

  fun parseObject(nameParsingMode: NameParsingMode, optionalBody: Boolean) {
    parseClassOrObject(true, nameParsingMode, optionalBody, false)
  }

  /**
   * enumClassBody
   *   : "{" enumEntries (";" members)? "}"
   *   ;
   */
  private fun parseEnumClassBody() {
    if (!at(LBRACE)) return

    val body = mark()
    myBuilder.enableNewlines()

    advance() // LBRACE

    if (!parseEnumEntries() && !at(RBRACE)) {
      error("Expecting ';' after the last enum entry or '}' to close enum class body")
    }
    parseMembers()
    expect(RBRACE, "Expecting '}' to close enum class body")

    myBuilder.restoreNewlinesState()
    body.done(CLASS_BODY)
  }

  /***
   * enumEntries
   * : enumEntry{","}?
   * ;
   *
   * @return true if enum regular members can follow, false otherwise
   */
  private fun parseEnumEntries(): Boolean {
    while (!eof() && !at(RBRACE)) {
      when (parseEnumEntry()) {
        KotlinParsing.ParseEnumEntryResult.FAILED ->
          // Special case without any enum entries but with possible members after semicolon
          if (at(SEMICOLON)) {
            advance()
            return true
          } else {
            return false
          }
        KotlinParsing.ParseEnumEntryResult.NO_DELIMITER -> return false
        KotlinParsing.ParseEnumEntryResult.COMMA_DELIMITER -> {
        }
        KotlinParsing.ParseEnumEntryResult.SEMICOLON_DELIMITER -> return true
      }
    }
    return false
  }

  private enum class ParseEnumEntryResult {
    FAILED,
    NO_DELIMITER,
    COMMA_DELIMITER,
    SEMICOLON_DELIMITER
  }

  /**
   * enumEntry
   *   : modifiers SimpleName ("(" arguments ")")? classBody?
   *   ;
   */
  private fun parseEnumEntry(): ParseEnumEntryResult {
    val entry = mark()

    parseModifierList(DEFAULT, TokenSet.create(COMMA, SEMICOLON, RBRACE))

    if (!atSet(SOFT_KEYWORDS_AT_MEMBER_START) && at(IDENTIFIER)) {
      advance() // IDENTIFIER

      if (at(LPAR)) {
        // Arguments should be parsed here
        // Also, "fake" constructor call tree is created,
        // with empty type name inside
        val initializerList = mark()
        val delegatorSuperCall = mark()

        val callee = mark()
        val typeReference = mark()
        val type = mark()
        val referenceExpr = mark()
        referenceExpr.done(ENUM_ENTRY_SUPERCLASS_REFERENCE_EXPRESSION)
        type.done(USER_TYPE)
        typeReference.done(TYPE_REFERENCE)
        callee.done(CONSTRUCTOR_CALLEE)

        myExpressionParsing!!.parseValueArgumentList()
        delegatorSuperCall.done(SUPER_TYPE_CALL_ENTRY)
        initializerList.done(INITIALIZER_LIST)
      }
      if (at(LBRACE)) {
        parseClassBody()
      }
      val commaFound = at(COMMA)
      if (commaFound) {
        advance()
      }
      val semicolonFound = at(SEMICOLON)
      if (semicolonFound) {
        advance()
      }

      // Probably some helper function
      AbstractKotlinParsing.closeDeclarationWithCommentBinders(entry, ENUM_ENTRY, true)
      return if (semicolonFound)
        ParseEnumEntryResult.SEMICOLON_DELIMITER
      else
        if (commaFound) ParseEnumEntryResult.COMMA_DELIMITER else ParseEnumEntryResult.NO_DELIMITER
    } else {
      entry.rollbackTo()
      return ParseEnumEntryResult.FAILED
    }
  }

  /**
   * classBody
   *   : ("{" members "}")?
   *   ;
   */
  private fun parseClassBody() {
    val body = mark()

    myBuilder.enableNewlines()

    if (expect(LBRACE, "Expecting a class body")) {
      parseMembers()
      expect(RBRACE, "Missing '}")
    }

    myBuilder.restoreNewlinesState()

    body.done(CLASS_BODY)
  }

  /***
   * members
   * : memberDeclaration*
   * ;
   */
  private fun parseMembers() {
    while (!eof() && !at(RBRACE)) {
      parseMemberDeclaration()
    }
  }

  /**
   * memberDeclaration
   *   : modifiers memberDeclaration'
   *   ;
   *
   * memberDeclaration'
   *   : companionObject
   *   : secondaryConstructor
   *   : function
   *   : property
   *   : class
   *   : extension
   *   : typeAlias
   *   : anonymousInitializer
   *   : object
   *   ;
   */
  private fun parseMemberDeclaration() {
    if (at(SEMICOLON)) {
      advance() // SEMICOLON
      return
    }
    val decl = mark()

    val detector = ModifierDetector()
    parseModifierList(detector, DEFAULT, TokenSet.EMPTY)

    val declType = parseMemberDeclarationRest(detector.isEnumDetected, detector.isDefaultDetected)

    if (declType == null) {
      errorWithRecovery("Expecting member declaration", TokenSet.EMPTY)
      decl.drop()
    } else {
      AbstractKotlinParsing.Companion.closeDeclarationWithCommentBinders(decl, declType, true)
    }
  }

  private fun parseMemberDeclarationRest(isEnum: Boolean, isDefault: Boolean): IElementType? {
    val keywordToken = tt()
    var declType: IElementType? = null
    when {
      keywordToken === CLASS_KEYWORD || keywordToken === INTERFACE_KEYWORD -> declType = parseClass(isEnum)
      keywordToken === FUN_KEYWORD -> declType = parseFunction()
      keywordToken === VAL_KEYWORD || keywordToken === VAR_KEYWORD -> declType = parseProperty()
      keywordToken === TYPE_ALIAS_KEYWORD -> declType = parseTypeAlias()
      keywordToken === OBJECT_KEYWORD -> {
        parseObject(if (isDefault) NameParsingMode.ALLOWED else NameParsingMode.REQUIRED, true)
        declType = OBJECT_DECLARATION
      }
      at(INIT_KEYWORD) -> {
        advance() // init
        if (at(LBRACE)) {
          parseBlock()
        } else {
          mark().error("Expecting '{' after 'init'")
        }
        declType = CLASS_INITIALIZER
      }
      at(CONSTRUCTOR_KEYWORD) -> {
        parseSecondaryConstructor()
        declType = SECONDARY_CONSTRUCTOR
      }
      at(LBRACE) -> {
        error("Expecting member declaration")
        parseBlock()
        declType = FUN
      }
    }
    return declType
  }

  /**
   * secondaryConstructor
   *   : modifiers "constructor" valueParameters (":" constructorDelegationCall)? block
   * constructorDelegationCall
   *   : "this" valueArguments
   *   : "super" valueArguments
   */
  private fun parseSecondaryConstructor() {
    assert(_at(CONSTRUCTOR_KEYWORD))

    advance() // CONSTRUCTOR_KEYWORD

    val valueArgsRecoverySet = TokenSet.create(LBRACE, SEMICOLON, RPAR, EOL_OR_SEMICOLON, RBRACE)
    if (at(LPAR)) {
      parseValueParameterList(
        false,
        /**typeRequired = */
        true, valueArgsRecoverySet
      )
    } else {
      errorWithRecovery("Expecting '('", TokenSet.orSet(valueArgsRecoverySet, TokenSet.create(COLON)))
    }

    if (at(COLON)) {
      advance() // COLON

      val delegationCall = mark()

      if (at(THIS_KEYWORD) || at(SUPER_KEYWORD)) {
        parseThisOrSuper()
        myExpressionParsing!!.parseValueArgumentList()
      } else {
        error("Expecting a 'this' or 'super' constructor call")
        var beforeWrongDelegationCallee: PsiBuilder.Marker? = null
        if (!at(LPAR)) {
          beforeWrongDelegationCallee = mark()
          advance() // wrong delegation callee
        }
        myExpressionParsing!!.parseValueArgumentList()

        if (beforeWrongDelegationCallee != null) {
          if (at(LBRACE)) {
            beforeWrongDelegationCallee.drop()
          } else {
            beforeWrongDelegationCallee.rollbackTo()
          }
        }
      }

      delegationCall.done(CONSTRUCTOR_DELEGATION_CALL)
    } else {
      // empty constructor delegation call
      val emptyDelegationCall = mark()
      mark().done(CONSTRUCTOR_DELEGATION_REFERENCE)
      emptyDelegationCall.done(CONSTRUCTOR_DELEGATION_CALL)
    }

    if (at(LBRACE)) {
      parseBlock()
    }
  }

  private fun parseThisOrSuper() {
    assert(_at(THIS_KEYWORD) || _at(SUPER_KEYWORD))
    val mark = mark()

    advance() // THIS_KEYWORD | SUPER_KEYWORD

    mark.done(CONSTRUCTOR_DELEGATION_REFERENCE)
  }

  /**
   * typeAlias
   *   : modifiers "typealias" SimpleName typeParameters? "=" type
   *   ;
   */
  fun parseTypeAlias(): IElementType {
    assert(_at(TYPE_ALIAS_KEYWORD))

    advance() // TYPE_ALIAS_KEYWORD

    expect(
      IDENTIFIER,
      "Type name expected",
      TokenSet.orSet(TokenSet.create(LT, EQ, SEMICOLON), TOP_LEVEL_DECLARATION_FIRST)
    )

    parseTypeParameterList(TYPE_PARAMETER_GT_RECOVERY_SET)

    if (at(WHERE_KEYWORD)) {
      val error = mark()
      parseTypeConstraints()
      error.error("Type alias parameters can't have bounds")
    }

    expect(EQ, "Expecting '='", TokenSet.orSet(TOP_LEVEL_DECLARATION_FIRST, TokenSet.create(SEMICOLON)))

    parseTypeRef()

    consumeIf(SEMICOLON)

    return TYPEALIAS
  }

  /**
   * variableDeclarationEntry
   *   : SimpleName (":" type)?
   *   ;
   *
   * property
   *   : modifiers ("val" | "var")
   *       typeParameters?
   *       (type ".")?
   *       ("(" variableDeclarationEntry{","} ")" | variableDeclarationEntry)
   *       typeConstraints
   *       ("by" | "=" expression SEMI?)?
   *       (getter? setter? | setter? getter?) SEMI?
   *   ;
   */
  private fun parseProperty(): IElementType {
    return parseProperty(PropertyParsingMode.MEMBER_OR_TOPLEVEL)
  }

  fun parseLocalProperty(isScriptTopLevel: Boolean): IElementType {
    return parseProperty(if (isScriptTopLevel) PropertyParsingMode.SCRIPT_TOPLEVEL else PropertyParsingMode.LOCAL)
  }

  enum class PropertyParsingMode private constructor(
    val destructuringAllowed: Boolean,
    val accessorsAllowed: Boolean
  ) {
    MEMBER_OR_TOPLEVEL(false, true),
    LOCAL(true, false),
    SCRIPT_TOPLEVEL(true, true)
  }

  fun parseProperty(mode: PropertyParsingMode): IElementType {
    assert(at(VAL_KEYWORD) || at(VAR_KEYWORD))
    advance()

    val typeParametersDeclared = at(LT) && parseTypeParameterList(TokenSet.create(IDENTIFIER, EQ, COLON, SEMICOLON))

    val propertyNameFollow =
      TokenSet.create(COLON, EQ, LBRACE, RBRACE, SEMICOLON, VAL_KEYWORD, VAR_KEYWORD, FUN_KEYWORD, CLASS_KEYWORD)

    myBuilder.disableJoiningComplexTokens()

    val receiver = mark()
    val receiverTypeDeclared = parseReceiverType("property", propertyNameFollow)

    val multiDeclaration = at(LPAR)

    AbstractKotlinParsing.errorIf(
      receiver,
      multiDeclaration && receiverTypeDeclared,
      "Receiver type is not allowed on a destructuring declaration"
    )

    val isNameOnTheNextLine = eol()
    val beforeName = mark()

    if (multiDeclaration) {
      val multiDecl = mark()
      parseMultiDeclarationName(propertyNameFollow)
      errorIf(
        multiDecl,
        !mode.destructuringAllowed,
        "Destructuring declarations are only allowed for local variables/values"
      )
    } else {
      parseFunctionOrPropertyName(
        receiverTypeDeclared, "property", propertyNameFollow,
        /**nameRequired = */
        true
      )
    }

    myBuilder.restoreJoiningComplexTokensState()

    var noTypeReference = true
    if (at(COLON)) {
      noTypeReference = false
      val type = mark()
      advance() // COLON
      parseTypeRef()
      AbstractKotlinParsing.Companion.errorIf(
        type,
        multiDeclaration,
        "Type annotations are not allowed on destructuring declarations"
      )
    }

    parseTypeConstraintsGuarded(typeParametersDeclared)

    if (!parsePropertyDelegateOrAssignment() && isNameOnTheNextLine && noTypeReference && !receiverTypeDeclared) {
      // Do not parse property identifier on the next line if declaration is invalid
      // In most cases this identifier relates to next statement/declaration
      beforeName.rollbackTo()
      error("Expecting property name or receiver type")
      return PROPERTY
    }

    beforeName.drop()

    if (mode.accessorsAllowed) {
      // It's only needed for non-local properties, because in local ones:
      // "val a = 1; b" must not be an infix call of b on "val ...;"

      myBuilder.enableNewlines()
      val hasNewLineWithSemicolon = consumeIf(SEMICOLON) && myBuilder.newlineBeforeCurrentToken()
      myBuilder.restoreNewlinesState()

      if (!hasNewLineWithSemicolon) {
        val accessorKind = parsePropertyGetterOrSetter(null)
        if (accessorKind != null) {
          parsePropertyGetterOrSetter(accessorKind)
        }

        if (!atSet(EOL_OR_SEMICOLON, RBRACE)) {
          if (lastToken !== SEMICOLON) {
            errorUntil(
              "Property getter or setter expected",
              TokenSet.orSet(DECLARATION_FIRST, TokenSet.create(EOL_OR_SEMICOLON, LBRACE, RBRACE))
            )
          }
        } else {
          consumeIf(SEMICOLON)
        }
      }
    }

    return if (multiDeclaration) DESTRUCTURING_DECLARATION else PROPERTY
  }

  private fun parsePropertyDelegateOrAssignment(): Boolean {
    if (at(BY_KEYWORD)) {
      parsePropertyDelegate()
      return true
    } else if (at(EQ)) {
      advance() // EQ
      myExpressionParsing!!.parseExpression()
      return true
    }

    return false
  }

  /**
   * propertyDelegate
   *   : "by" expression
   *   ;
   */
  private fun parsePropertyDelegate() {
    assert(_at(BY_KEYWORD))
    val delegate = mark()
    advance() // BY_KEYWORD
    myExpressionParsing!!.parseExpression()
    delegate.done(PROPERTY_DELEGATE)
  }

  /**
   * (SimpleName (":" type){","})
   */
  fun parseMultiDeclarationName(follow: TokenSet) {
    // Parsing multi-name, e.g.
    //   val (a, b) = foo()
    myBuilder.disableNewlines()
    advance() // LPAR

    val recoverySet = TokenSet.orSet(PARAMETER_NAME_RECOVERY_SET, follow)
    if (!atSet(follow)) {
      while (true) {
        if (at(COMMA)) {
          errorAndAdvance("Expecting a name")
        } else if (at(RPAR)) {
          error("Expecting a name")
          break
        }
        val property = mark()

        parseModifierList(DEFAULT, TokenSet.create(COMMA, RPAR, COLON, EQ))

        expect(IDENTIFIER, "Expecting a name", recoverySet)

        if (at(COLON)) {
          advance() // COLON
          parseTypeRef(follow)
        }
        property.done(DESTRUCTURING_DECLARATION_ENTRY)

        if (!at(COMMA)) break
        advance() // COMMA
      }
    }

    expect(RPAR, "Expecting ')'", follow)
    myBuilder.restoreNewlinesState()
  }

  private enum class AccessorKind {
    GET, SET
  }

  /**
   * getterOrSetter
   *   : modifiers ("get" | "set")
   *   :
   *        (     "get" "(" ")"
   *           |
   *              "set" "(" modifiers parameter ")"
   *        ) functionBody
   *   ;
   */
  private fun parsePropertyGetterOrSetter(notAllowedKind: AccessorKind?): AccessorKind? {
    val getterOrSetter = mark()

    parseModifierList(DEFAULT, TokenSet.EMPTY)

    val accessorKind: AccessorKind
    if (at(GET_KEYWORD)) {
      accessorKind = AccessorKind.GET
    } else if (at(SET_KEYWORD)) {
      accessorKind = AccessorKind.SET
    } else {
      getterOrSetter.rollbackTo()
      return null
    }

    if (accessorKind == notAllowedKind) {
      getterOrSetter.rollbackTo()
      return null
    }

    advance() // GET_KEYWORD or SET_KEYWORD

    if (!at(LPAR)) {
      // Account for Jet-114 (val a : int get {...})
      val ACCESSOR_FIRST_OR_PROPERTY_END =
        TokenSet.orSet(MODIFIER_KEYWORDS, TokenSet.create(AT, GET_KEYWORD, SET_KEYWORD, EOL_OR_SEMICOLON, RBRACE))
      if (!atSet(ACCESSOR_FIRST_OR_PROPERTY_END)) {
        errorUntil(
          "Accessor body expected",
          TokenSet.orSet(ACCESSOR_FIRST_OR_PROPERTY_END, TokenSet.create(LBRACE, LPAR, EQ))
        )
      } else {
        AbstractKotlinParsing.Companion.closeDeclarationWithCommentBinders(getterOrSetter, PROPERTY_ACCESSOR, false)
        return accessorKind
      }
    }

    myBuilder.disableNewlines()
    expect(LPAR, "Expecting '('", TokenSet.create(RPAR, IDENTIFIER, COLON, LBRACE, EQ))
    if (accessorKind == AccessorKind.SET) {
      val parameterList = mark()
      val setterParameter = mark()
      parseModifierList(DEFAULT, TokenSet.create(COMMA, COLON, RPAR))
      expect(IDENTIFIER, "Expecting parameter name", TokenSet.create(RPAR, COLON, LBRACE, EQ))

      if (at(COLON)) {
        advance()  // COLON
        parseTypeRef()
      }
      setterParameter.done(VALUE_PARAMETER)
      parameterList.done(VALUE_PARAMETER_LIST)
    }
    if (!at(RPAR)) {
      errorUntil("Expecting ')'", TokenSet.create(RPAR, COLON, LBRACE, RBRACE, EQ, EOL_OR_SEMICOLON))
    }
    if (at(RPAR)) {
      advance()
    }
    myBuilder.restoreNewlinesState()

    if (at(COLON)) {
      advance()

      parseTypeRef()
    }

    parseFunctionBody()

    closeDeclarationWithCommentBinders(getterOrSetter, PROPERTY_ACCESSOR, false)

    return accessorKind
  }

  fun parseFunction(): IElementType? {
    return parseFunction(false)
  }

  /**
   * function
   *   : modifiers "fun" typeParameters?
   *       (type ".")?
   *       SimpleName
   *       typeParameters? functionParameters (":" type)?
   *       typeConstraints
   *       functionBody?
   *   ;
   */
  @Contract("false -> !null")
  fun parseFunction(failIfIdentifierExists: Boolean): IElementType? {
    assert(_at(FUN_KEYWORD))

    advance() // FUN_KEYWORD

    // Recovery for the case of class A { fun| }
    if (at(RBRACE)) {
      error("Function body expected")
      return FUN
    }

    var typeParameterListOccurred = false
    if (at(LT)) {
      parseTypeParameterList(TokenSet.create(LBRACKET, LBRACE, RBRACE, LPAR))
      typeParameterListOccurred = true
    }

    myBuilder.disableJoiningComplexTokens()

    val functionNameFollow = TokenSet.create(LT, LPAR, RPAR, COLON, EQ)
    val receiverFound = parseReceiverType("function", functionNameFollow)

    if (at(IDENTIFIER) && failIfIdentifierExists) {
      myBuilder.restoreJoiningComplexTokensState()
      return null
    }

    // function as expression has no name
    parseFunctionOrPropertyName(
      receiverFound, "function", functionNameFollow,
      /**nameRequired = */
      false
    )

    myBuilder.restoreJoiningComplexTokensState()

    val valueParametersFollow = TokenSet.create(EQ, LBRACE, RBRACE, SEMICOLON, RPAR)

    if (at(LT)) {
      var error = mark()
      parseTypeParameterList(TokenSet.orSet(TokenSet.create(LPAR), valueParametersFollow))
      if (typeParameterListOccurred) {
        val offset = myBuilder.currentOffset
        error.rollbackTo()
        error = mark()
        advance(offset - myBuilder.currentOffset)
        error.error("Only one type parameter list is allowed for a function")
      } else {
        error.drop()
      }
      typeParameterListOccurred = true
    }

    if (at(LPAR)) {
      parseValueParameterList(
        false,
        /** typeRequired  = */
        false, valueParametersFollow
      )
    } else {
      error("Expecting '('")
    }

    if (at(COLON)) {
      advance() // COLON

      parseTypeRef()
    }

    parseTypeConstraintsGuarded(typeParameterListOccurred)

    if (at(SEMICOLON)) {
      advance() // SEMICOLON
    } else if (at(EQ) || at(LBRACE)) {
      parseFunctionBody()
    }

    return FUN
  }

  /**
   *   (type "." | annotations)?
   */
  private fun parseReceiverType(title: String, nameFollow: TokenSet): Boolean {
    val annotations = mark()
    val annotationsPresent = parseAnnotations(DEFAULT)
    val lastDot = lastDotAfterReceiver()
    val receiverPresent = lastDot != -1
    if (annotationsPresent) {
      if (receiverPresent) {
        annotations.rollbackTo()
      } else {
        annotations.error("Annotations are not allowed in this position")
      }
    } else {
      annotations.drop()
    }

    if (!receiverPresent) return false

    createTruncatedBuilder(lastDot).parseTypeRef()

    if (atSet(RECEIVER_TYPE_TERMINATORS)) {
      advance() // expectation
    } else {
      errorWithRecovery("Expecting '.' before a $title name", nameFollow)
    }
    return true
  }

  private fun lastDotAfterReceiver(): Int {
    return if (at(LPAR)) {
      matchTokenStreamPredicate(
        FirstBefore(
          AtSet(RECEIVER_TYPE_TERMINATORS),
          object : AbstractTokenStreamPredicate() {
            override fun matching(topLevel: Boolean): Boolean {
              return if (topLevel && definitelyOutOfReceiver()) {
                true
              } else topLevel && !at(QUEST) && !at(LPAR) && !at(RPAR)
            }
          }
        ))
    } else {
      matchTokenStreamPredicate(
        LastBefore(
          AtSet(RECEIVER_TYPE_TERMINATORS),
          object : AbstractTokenStreamPredicate() {
            override fun matching(topLevel: Boolean): Boolean {
              if (topLevel && (definitelyOutOfReceiver() || at(LPAR))) return true
              if (topLevel && at(IDENTIFIER)) {
                val lookahead = lookahead(1)
                return lookahead !== LT && lookahead !== DOT && lookahead !== SAFE_ACCESS && lookahead !== QUEST
              }
              return false
            }
          })
      )
    }
  }

  private fun definitelyOutOfReceiver(): Boolean {
    return atSet(EQ, COLON, LBRACE, RBRACE, BY_KEYWORD) || atSet(TOP_LEVEL_DECLARATION_FIRST)
  }

  /**
   * IDENTIFIER
   */
  private fun parseFunctionOrPropertyName(
    receiverFound: Boolean,
    title: String,
    nameFollow: TokenSet,
    nameRequired: Boolean
  ): Boolean {
    if (!nameRequired && atSet(nameFollow)) return true // no name

    val recoverySet = TokenSet.orSet(nameFollow, TokenSet.create(LBRACE, RBRACE), TOP_LEVEL_DECLARATION_FIRST)
    return if (!receiverFound) {
      expect(IDENTIFIER, "Expecting $title name or receiver type", recoverySet)
    } else {
      expect(IDENTIFIER, "Expecting $title name", recoverySet)
    }
  }

  /**
   * functionBody
   *   : block
   *   : "=" element
   *   ;
   */
  private fun parseFunctionBody() {
    if (at(LBRACE)) {
      parseBlock()
    } else if (at(EQ)) {
      advance() // EQ
      myExpressionParsing!!.parseExpression()
      consumeIf(SEMICOLON)
    } else {
      error("Expecting function body")
    }
  }

  /***
   * block
   *   : "{" (expressions)* "}"
   *   ;
   */
  fun parseBlock() {
    val block = mark()

    myBuilder.enableNewlines()
    expect(LBRACE, "Expecting '{' to open a block")

    myExpressionParsing!!.parseStatements()

    expect(RBRACE, "Expecting '}'")
    myBuilder.restoreNewlinesState()

    block.done(BLOCK)
  }

  /**
   * delegationSpecifier{","}
   */
  private fun parseDelegationSpecifierList() {
    val list = mark()

    while (true) {
      if (at(COMMA)) {
        errorAndAdvance("Expecting a delegation specifier")
        continue
      }
      parseDelegationSpecifier()
      if (!at(COMMA)) break
      advance() // COMMA
    }

    list.done(SUPER_TYPE_LIST)
  }

  /**
   * delegationSpecifier
   *   : constructorInvocation // type and constructor arguments
   *   : userType
   *   : explicitDelegation
   *   ;
   *
   * explicitDelegation
   *   : userType "by" element
   *   ;
   */
  private fun parseDelegationSpecifier() {
    val delegator = mark()
    val reference = mark()
    parseTypeRef()

    when {
      at(BY_KEYWORD) -> {
        reference.drop()
        advance() // BY_KEYWORD
        createForByClause(myBuilder).myExpressionParsing!!.parseExpression()
        delegator.done(DELEGATED_SUPER_TYPE_ENTRY)
      }
      at(LPAR) -> {
        reference.done(CONSTRUCTOR_CALLEE)
        myExpressionParsing!!.parseValueArgumentList()
        delegator.done(SUPER_TYPE_CALL_ENTRY)
      }
      else -> {
        reference.drop()
        delegator.done(SUPER_TYPE_ENTRY)
      }
    }
  }

  /**
   * typeParameters
   *   : ("<" typeParameter{","} ">"
   *   ;
   */
  private fun parseTypeParameterList(recoverySet: TokenSet): Boolean {
    var result = false
    if (at(LT)) {
      val list = mark()

      myBuilder.disableNewlines()
      advance() // LT

      while (true) {
        if (at(COMMA)) errorAndAdvance("Expecting type parameter declaration")
        parseTypeParameter()

        if (!at(COMMA)) break
        advance() // COMMA
      }

      expect(GT, "Missing '>'", recoverySet)
      myBuilder.restoreNewlinesState()
      result = true

      list.done(TYPE_PARAMETER_LIST)
    }
    return result
  }

  /**
   * typeConstraints
   *   : ("where" typeConstraint{","})?
   *   ;
   */
  private fun parseTypeConstraintsGuarded(typeParameterListOccurred: Boolean) {
    val error = mark()
    val constraints = parseTypeConstraints()
    AbstractKotlinParsing.Companion.errorIf(
      error,
      constraints && !typeParameterListOccurred,
      "Type constraints are not allowed when no type parameters declared"
    )
  }

  private fun parseTypeConstraints(): Boolean {
    if (at(WHERE_KEYWORD)) {
      parseTypeConstraintList()
      return true
    }
    return false
  }

  /**
   * typeConstraint{","}
   */
  private fun parseTypeConstraintList() {
    assert(_at(WHERE_KEYWORD))

    advance() // WHERE_KEYWORD

    val list = mark()

    while (true) {
      if (at(COMMA)) errorAndAdvance("Type constraint expected")
      parseTypeConstraint()
      if (!at(COMMA)) break
      advance() // COMMA
    }

    list.done(TYPE_CONSTRAINT_LIST)
  }

  /**
   * typeConstraint
   *   : annotations SimpleName ":" type
   *   ;
   */
  private fun parseTypeConstraint() {
    val constraint = mark()

    parseAnnotations(DEFAULT)

    val reference = mark()
    if (expect(
        IDENTIFIER,
        "Expecting type parameter name",
        TokenSet.orSet(TokenSet.create(COLON, COMMA, LBRACE, RBRACE), TYPE_REF_FIRST)
      )
    ) {
      reference.done(REFERENCE_EXPRESSION)
    } else {
      reference.drop()
    }

    expect(
      COLON,
      "Expecting ':' before the upper bound",
      TokenSet.orSet(TokenSet.create(LBRACE, RBRACE), TYPE_REF_FIRST)
    )

    parseTypeRef()

    constraint.done(TYPE_CONSTRAINT)
  }

  /**
   * typeParameter
   *   : modifiers SimpleName (":" userType)?
   *   ;
   */
  private fun parseTypeParameter() {
    if (atSet(TYPE_PARAMETER_GT_RECOVERY_SET)) {
      error("Type parameter declaration expected")
      return
    }

    val mark = mark()

    parseModifierList(DEFAULT, TokenSet.create(GT, COMMA, COLON))

    expect(IDENTIFIER, "Type parameter name expected", TokenSet.EMPTY)

    if (at(COLON)) {
      advance() // COLON
      parseTypeRef()
    }

    mark.done(TYPE_PARAMETER)

  }

  @JvmOverloads
  fun parseTypeRef(extraRecoverySet: TokenSet = TokenSet.EMPTY) {
    val typeRefMarker = parseTypeRefContents(extraRecoverySet)
    typeRefMarker.done(TYPE_REFERENCE)
  }

  // The extraRecoverySet is needed for the foo(bar<x, 1, y>(z)) case, to tell whether we should stop
  // on expression-indicating symbols or not
  private fun parseTypeRefContents(extraRecoverySet: TokenSet): PsiBuilder.Marker {
    val typeRefMarker = mark()

    parseTypeModifierList()

    var typeElementMarker = mark()

    val lookahead = lookahead(1)
    val lookahead2 = lookahead(2)
    var typeBeforeDot = true
    if (at(IDENTIFIER) && !(lookahead === DOT && lookahead2 === IDENTIFIER) && lookahead !== LT && at(DYNAMIC_KEYWORD)) {
      val dynamicType = mark()
      advance() // DYNAMIC_KEYWORD
      dynamicType.done(DYNAMIC_TYPE)
    } else if (at(IDENTIFIER) || at(PACKAGE_KEYWORD) || atParenthesizedMutableForPlatformTypes(0)) {
      parseUserType()
    } else if (at(LPAR)) {
      val functionOrParenthesizedType = mark()

      // This may be a function parameter list or just a prenthesized type
      advance() // LPAR
      parseTypeRefContents(TokenSet.EMPTY).drop() // parenthesized types, no reference element around it is needed

      if (at(RPAR)) {
        advance() // RPAR
        if (at(ARROW)) {
          // It's a function type with one parameter specified
          //    (A) -> B
          functionOrParenthesizedType.rollbackTo()
          parseFunctionType()
        } else {
          // It's a parenthesized type
          //    (A)
          functionOrParenthesizedType.drop()
        }
      } else {
        // This must be a function type
        //   (A, B) -> C
        // or
        //   (a : A) -> C
        functionOrParenthesizedType.rollbackTo()
        parseFunctionType()
      }

    } else {
      errorWithRecovery(
        "Type expected",
        TokenSet.orSet(
          TOP_LEVEL_DECLARATION_FIRST,
          TokenSet.create(EQ, COMMA, GT, RBRACKET, DOT, RPAR, RBRACE, LBRACE, SEMICOLON),
          extraRecoverySet
        )
      )
      typeBeforeDot = false
    }

    // Disabling token merge is required for cases like
    //    Int?.(Foo) -> Bar
    myBuilder.disableJoiningComplexTokens()
    typeElementMarker = parseNullableTypeSuffix(typeElementMarker)
    myBuilder.restoreJoiningComplexTokensState()

    if (typeBeforeDot && at(DOT)) {
      // This is a receiver for a function type
      //  A.(B) -> C
      //   ^

      val functionType = typeElementMarker.precede()

      val receiverTypeRef = typeElementMarker.precede()
      val receiverType = receiverTypeRef.precede()
      receiverTypeRef.done(TYPE_REFERENCE)
      receiverType.done(FUNCTION_TYPE_RECEIVER)

      advance() // DOT

      if (at(LPAR)) {
        parseFunctionTypeContents().drop()
      } else {
        error("Expecting function type")
      }

      functionType.done(FUNCTION_TYPE)
    }

    typeElementMarker.drop()
    return typeRefMarker
  }

  private fun parseNullableTypeSuffix(typeElementMarker: PsiBuilder.Marker): PsiBuilder.Marker {
    var typeElementMarker = typeElementMarker
    // ?: is joined regardless of joining state
    while (at(QUEST) && myBuilder.rawLookup(1) !== COLON) {
      val precede = typeElementMarker.precede()
      advance() // QUEST
      typeElementMarker.done(NULLABLE_TYPE)
      typeElementMarker = precede
    }
    return typeElementMarker
  }

  /**
   * userType
   *   : simpleUserType{"."}
   *   ;
   *
   *   recovers on platform types:
   *    - Foo!
   *    - (Mutable)List<Foo>!
   *    - Array<(out) Foo>!
   */
  private fun parseUserType() {
    var userType = mark()

    if (at(PACKAGE_KEYWORD)) {
      val keyword = mark()
      advance() // PACKAGE_KEYWORD
      keyword.error("Expecting an element")
      expect(DOT, "Expecting '.'", TokenSet.create(IDENTIFIER, LBRACE, RBRACE))
    }

    var reference = mark()
    while (true) {
      recoverOnParenthesizedWordForPlatformTypes(0, "Mutable", true)

      if (expect(
          IDENTIFIER, "Expecting type name",
          TokenSet.orSet(
            KotlinExpressionParsing.EXPRESSION_FIRST,
            KotlinExpressionParsing.EXPRESSION_FOLLOW,
            DECLARATION_FIRST
          )
        )
      ) {
        reference.done(REFERENCE_EXPRESSION)
      } else {
        reference.drop()
        break
      }

      parseTypeArgumentList()

      recoverOnPlatformTypeSuffix()

      if (!at(DOT)) {
        break
      }
      if (lookahead(1) === LPAR && !atParenthesizedMutableForPlatformTypes(1)) {
        // This may be a receiver for a function type
        //   Int.(Int) -> Int
        break
      }

      val precede = userType.precede()
      userType.done(USER_TYPE)
      userType = precede

      advance() // DOT
      reference = mark()
    }

    userType.done(USER_TYPE)
  }

  private fun atParenthesizedMutableForPlatformTypes(offset: Int): Boolean {
    return recoverOnParenthesizedWordForPlatformTypes(offset, "Mutable", false)
  }

  private fun recoverOnParenthesizedWordForPlatformTypes(offset: Int, word: String, consume: Boolean): Boolean {
    // Array<(out) Foo>! or (Mutable)List<Bar>!
    if (lookahead(offset) === LPAR && lookahead(offset + 1) === IDENTIFIER && lookahead(offset + 2) === RPAR && lookahead(
        offset + 3
      ) === IDENTIFIER
    ) {
      val error = mark()

      advance(offset)

      advance() // LPAR
      if (word != myBuilder.tokenText) {
        // something other than "out" / "Mutable"
        error.rollbackTo()
        return false
      } else {
        advance() // IDENTIFIER('out')
        advance() // RPAR

        if (consume) {
          error.error("Unexpected tokens")
        } else {
          error.rollbackTo()
        }

        return true
      }
    }
    return false
  }

  private fun recoverOnPlatformTypeSuffix() {
    // Recovery for platform types
    if (at(EXCL)) {
      val error = mark()
      advance() // EXCL
      error.error("Unexpected token")
    }
  }

  /**
   *  (optionalProjection type){","}
   */
  private fun parseTypeArgumentList(): PsiBuilder.Marker? {
    if (!at(LT)) return null

    val list = mark()

    tryParseTypeArgumentList(TokenSet.EMPTY)

    list.done(TYPE_ARGUMENT_LIST)
    return list
  }

  fun tryParseTypeArgumentList(extraRecoverySet: TokenSet): Boolean {
    myBuilder.disableNewlines()
    advance() // LT

    while (true) {
      val projection = mark()

      recoverOnParenthesizedWordForPlatformTypes(0, "out", true)

      // Currently we do not allow annotations on star projections and probably we should not
      // Annotations on other kinds of type arguments should be parsed as common type annotations (within parseTypeRef call)
      parseTypeArgumentModifierList()

      if (at(MUL)) {
        advance() // MUL
      } else {
        parseTypeRef(extraRecoverySet)
      }
      projection.done(TYPE_PROJECTION)
      if (!at(COMMA)) break
      advance() // COMMA
    }

    val atGT = at(GT)
    if (!atGT) {
      error("Expecting a '>'")
    } else {
      advance() // GT
    }
    myBuilder.restoreNewlinesState()
    return atGT
  }

  /**
   * functionType
   *   : (type ".")? "(" parameter{","}? ")" "->" type?
   *   ;
   */
  private fun parseFunctionType() {
    parseFunctionTypeContents().done(FUNCTION_TYPE)
  }

  private fun parseFunctionTypeContents(): PsiBuilder.Marker {
    assert(_at(LPAR)) { tt() ?: "null" }
    val functionType = mark()

    parseValueParameterList(
      true,
      /** typeRequired  = */
      true, TokenSet.EMPTY
    )

    expect(ARROW, "Expecting '->' to specify return type of a function type", TYPE_REF_FIRST)
    parseTypeRef()

    return functionType
  }

  /**
   * functionParameters
   *   : "(" functionParameter{","}? ")"
   *   ;
   *
   * functionParameter
   *   : modifiers functionParameterRest
   *   ;
   *
   * functionParameterRest
   *   : parameter ("=" element)?
   *   ;
   */
  private fun parseValueParameterList(isFunctionTypeContents: Boolean, typeRequired: Boolean, recoverySet: TokenSet) {
    assert(_at(LPAR))
    val parameters = mark()

    myBuilder.disableNewlines()
    advance() // LPAR

    if (!at(RPAR) && !atSet(recoverySet)) {
      while (true) {
        if (at(COMMA)) {
          errorAndAdvance("Expecting a parameter declaration")
        } else if (at(RPAR)) {
          error("Expecting a parameter declaration")
          break
        }

        if (isFunctionTypeContents) {
          if (!tryParseValueParameter(typeRequired)) {
            val valueParameter = mark()
            parseFunctionTypeValueParameterModifierList()
            parseTypeRef()
            AbstractKotlinParsing.Companion.closeDeclarationWithCommentBinders(valueParameter, VALUE_PARAMETER, false)
          }
        } else {
          parseValueParameter(typeRequired)
        }

        if (at(COMMA)) {
          advance() // COMMA
        } else {
          if (!at(RPAR)) error("Expecting comma or ')'")
          if (!atSet(if (isFunctionTypeContents) LAMBDA_VALUE_PARAMETER_FIRST else VALUE_PARAMETER_FIRST)) break
        }
      }
    }

    expect(RPAR, "Expecting ')'", recoverySet)
    myBuilder.restoreNewlinesState()

    parameters.done(VALUE_PARAMETER_LIST)
  }

  /**
   * functionParameter
   *   : modifiers ("val" | "var")? parameter ("=" element)?
   *   ;
   */
  private fun tryParseValueParameter(typeRequired: Boolean): Boolean {
    return parseValueParameter(true, typeRequired)
  }

  fun parseValueParameter(typeRequired: Boolean) {
    parseValueParameter(false, typeRequired)
  }

  private fun parseValueParameter(rollbackOnFailure: Boolean, typeRequired: Boolean): Boolean {
    val parameter = mark()

    parseModifierList(DEFAULT, NO_MODIFIER_BEFORE_FOR_VALUE_PARAMETER)

    if (at(VAR_KEYWORD) || at(VAL_KEYWORD)) {
      advance() // VAR_KEYWORD | VAL_KEYWORD
    }

    if (!parseFunctionParameterRest(typeRequired) && rollbackOnFailure) {
      parameter.rollbackTo()
      return false
    }

    AbstractKotlinParsing.Companion.closeDeclarationWithCommentBinders(parameter, VALUE_PARAMETER, false)
    return true
  }

  /**
   * functionParameterRest
   *   : parameter ("=" element)?
   *   ;
   */
  private fun parseFunctionParameterRest(typeRequired: Boolean): Boolean {
    var noErrors = true

    // Recovery for the case 'fun foo(Array<String>) {}'
    // Recovery for the case 'fun foo(: Int) {}'
    if (at(IDENTIFIER) && lookahead(1) === LT || at(COLON)) {
      error("Parameter name expected")
      if (at(COLON)) {
        // We keep noErrors == true so that unnamed parameters starting with ":" are not rolled back during parsing of functional types
        advance() // COLON
      } else {
        noErrors = false
      }
      parseTypeRef()
    } else {
      expect(IDENTIFIER, "Parameter name expected", PARAMETER_NAME_RECOVERY_SET)

      if (at(COLON)) {
        advance() // COLON
        parseTypeRef()
      } else if (typeRequired) {
        errorWithRecovery("Parameters must have type annotation", PARAMETER_NAME_RECOVERY_SET)
        noErrors = false
      }
    }

    if (at(EQ)) {
      advance() // EQ
      myExpressionParsing!!.parseExpression()
    }

    return noErrors
  }

  public override fun create(builder: SemanticWhitespaceAwarePsiBuilder): KotlinParsing {
    return topLevelKotlinParsing(builder)
  }

  /**package*/
  class ModifierDetector : Consumer<IElementType> {
    var isEnumDetected = false
      private set
    var isDefaultDetected = false
      private set

    override fun consume(item: IElementType) {
      if (item === KtTokens.ENUM_KEYWORD) {
        isEnumDetected = true
      } else if (item === KtTokens.COMPANION_KEYWORD) {
        isDefaultDetected = true
      }
    }
  }

  enum class AnnotationParsingMode private constructor(
    var isFileAnnotationParsingMode: Boolean,
    var allowAnnotations: Boolean
  ) {
    DEFAULT(false, true),
    FILE_ANNOTATIONS_BEFORE_PACKAGE(true, true),
    FILE_ANNOTATIONS_WHEN_PACKAGE_OMITTED(true, true),
    NO_ANNOTATIONS(false, false)
  }
}
/**
 * type
 *   : typeModifiers typeReference
 *   ;
 *
 * typeReference
 *   : functionType
 *   : userType
 *   : nullableType
 *   : "dynamic"
 *   ;
 *
 * nullableType
 *   : typeReference "?"
 *   ;
 */
