/*
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
import com.intellij.psi.tree.IElementType
import com.intellij.psi.tree.TokenSet
import org.jetbrains.kotlin.KtNodeType
import org.jetbrains.kotlin.KtNodeTypes
import org.jetbrains.kotlin.lexer.KtToken
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.parsing.KotlinParsing.NameParsingMode
import org.jetbrains.kotlin.parsing.trash.SemanticWhitespaceAwarePsiBuilder

import java.util.*

import org.jetbrains.kotlin.KtNodeTypes.*
import org.jetbrains.kotlin.lexer.KtTokens.*
import org.jetbrains.kotlin.parsing.KotlinParsing.AnnotationParsingMode.DEFAULT
import org.jetbrains.kotlin.parsing.trash.PRECEDING_ALL_COMMENTS_BINDER
import org.jetbrains.kotlin.parsing.trash.TRAILING_ALL_COMMENTS_BINDER

open class KotlinExpressionParsing(
  builder: SemanticWhitespaceAwarePsiBuilder,
  private val myKotlinParsing: KotlinParsing
) : AbstractKotlinParsing(builder) {

  private val isAtLabelDefinitionOrMissingIdentifier: Boolean
    get() = at(IDENTIFIER) && myBuilder.rawLookup(1) === AT || at(AT)

  enum class Precedence(vararg operations: IElementType) {
    POSTFIX(
      PLUSPLUS, MINUSMINUS, EXCLEXCL,
      DOT, SAFE_ACCESS
    ), // typeArguments? valueArguments : typeArguments : arrayAccess

    PREFIX(MINUS, PLUS, MINUSMINUS, PLUSPLUS, EXCL) { // annotations

      override fun parseHigherPrecedence(parser: KotlinExpressionParsing) {
        throw IllegalStateException("Don't call this method")
      }
    },

    AS(AS_KEYWORD, `AS_SAFE`) {
      override fun parseRightHandSide(operation: IElementType?, parser: KotlinExpressionParsing): KtNodeType {
        parser.myKotlinParsing.parseTypeRef()
        return BINARY_WITH_TYPE
      }

      override fun parseHigherPrecedence(parser: KotlinExpressionParsing) {
        parser.parsePrefixExpression()
      }
    },

    MULTIPLICATIVE(MUL, DIV, PERC),
    ADDITIVE(PLUS, MINUS),
    RANGE(KtTokens.RANGE),
    SIMPLE_NAME(IDENTIFIER),
    ELVIS(KtTokens.ELVIS),
    IN_OR_IS(IN_KEYWORD, NOT_IN, IS_KEYWORD, NOT_IS) {
      override fun parseRightHandSide(operation: IElementType?, parser: KotlinExpressionParsing): KtNodeType {
        if (operation === IS_KEYWORD || operation === NOT_IS) {
          parser.myKotlinParsing.parseTypeRef()
          return IS_EXPRESSION
        }

        return super.parseRightHandSide(operation, parser)
      }
    },
    COMPARISON(LT, GT, LTEQ, GTEQ),
    EQUALITY(EQEQ, EXCLEQ, EQEQEQ, EXCLEQEQEQ),
    CONJUNCTION(ANDAND),
    DISJUNCTION(OROR),
    //        ARROW(KtTokens.ARROW),
    ASSIGNMENT(EQ, PLUSEQ, MINUSEQ, MULTEQ, DIVEQ, PERCEQ);

    private var higher: Precedence? = null
    val operations: TokenSet

    init {
      this.operations = TokenSet.create(*operations)
    }

    open fun parseHigherPrecedence(parser: KotlinExpressionParsing) {
      assert(higher != null)
      parser.parseBinaryExpression(higher!!)
    }

    /**
     * @param operation the operation sign (e.g. PLUS or IS)
     * @param parser    the parser object
     * @return node type of the result
     */
    open fun parseRightHandSide(operation: IElementType?, parser: KotlinExpressionParsing): KtNodeType {
      parseHigherPrecedence(parser)
      return BINARY_EXPRESSION
    }

    companion object {

      init {
        val values = Precedence.values()
        for (precedence in values) {
          val ordinal = precedence.ordinal
          precedence.higher = if (ordinal > 0) values[ordinal - 1] else null
        }
      }
    }
  }

  /*
     * element
     *   : annotations element
     *   : "(" element ")" // see tupleLiteral
     *   : literalConstant
     *   : functionLiteral
     *   : tupleLiteral
     *   : "null"
     *   : "this" ("<" type ">")?
     *   : expressionWithPrecedences
     *   : if
     *   : try
     *   : "typeof" "(" element ")"
     *   : "new" constructorInvocation
     *   : objectLiteral
     *   : declaration
     *   : jump
     *   : loop
     *   // block is syntactically equivalent to a functionLiteral with no parameters
     *   ;
     */
  fun parseExpression() {
    if (!atSet(EXPRESSION_FIRST)) {
      error("Expecting an expression")
      return
    }
    parseBinaryExpression(Precedence.ASSIGNMENT)
  }

  /*
     * element (operation element)*
     *
     * see the precedence table
     */
  private fun parseBinaryExpression(precedence: Precedence) {
    var expression = mark()

    precedence.parseHigherPrecedence(this)

    while (!interruptedWithNewLine() && atSet(precedence.operations)) {
      val operation = tt()

      parseOperationReference()

      val resultType = precedence.parseRightHandSide(operation, this)
      expression.done(resultType)
      expression = expression.precede()
    }

    expression.drop()
  }

  /*
     * label prefixExpression
     */
  private fun parseLabeledExpression() {
    val expression = mark()
    parseLabelDefinition()
    parsePrefixExpression()
    expression.done(LABELED_EXPRESSION)
  }

  /*
     * operation? prefixExpression
     */
  private fun parsePrefixExpression() {
    if (at(AT)) {
      if (!/* rollbackIfDefinitelyNotExpression = */parseLocalDeclaration(false, false)) {
        val expression = mark()
        myKotlinParsing.parseAnnotations(DEFAULT)
        parsePrefixExpression()
        expression.done(ANNOTATED_EXPRESSION)
      }
    } else {
      myBuilder.disableJoiningComplexTokens()
      if (isAtLabelDefinitionOrMissingIdentifier) {
        myBuilder.restoreJoiningComplexTokensState()
        parseLabeledExpression()
      } else if (atSet(Precedence.PREFIX.operations)) {
        val expression = mark()

        parseOperationReference()

        myBuilder.restoreJoiningComplexTokensState()

        parsePrefixExpression()
        expression.done(PREFIX_EXPRESSION)
      } else {
        myBuilder.restoreJoiningComplexTokensState()
        parsePostfixExpression()
      }
    }
  }

  /*
     * doubleColonSuffix
     *   : "::" SimpleName typeArguments?
     *   ;
     */
  private fun parseDoubleColonSuffix(expression: PsiBuilder.Marker): Boolean {
    if (!at(COLONCOLON)) return false

    advance() // COLONCOLON

    if (at(CLASS_KEYWORD)) {
      advance() // CLASS_KEYWORD

      expression.done(CLASS_LITERAL_EXPRESSION)
      return true
    }

    parseSimpleNameExpression()

    if (at(LT)) {
      val typeArgumentList = mark()
      if (myKotlinParsing.tryParseTypeArgumentList(TYPE_ARGUMENT_LIST_STOPPERS)) {
        typeArgumentList.error("Type arguments are not allowed")
      } else {
        typeArgumentList.rollbackTo()
      }
    }

    if (at(LPAR) && !myBuilder.newlineBeforeCurrentToken()) {
      val lpar = mark()
      parseCallSuffix()
      lpar.error("This syntax is reserved for future use; to call a reference, enclose it in parentheses: (foo::bar)(args)")
    }

    expression.done(CALLABLE_REFERENCE_EXPRESSION)
    return true
  }

  private fun skipQuestionMarksBeforeDoubleColon() {
    if (at(QUEST)) {
      var k = 1
      while (lookahead(k) === QUEST) k++
      if (lookahead(k) === COLONCOLON) {
        while (k > 0) {
          advance() // QUEST
          k--
        }
      }
    }
  }

  /*
     * postfixUnaryExpression
     *   : atomicExpression postfixUnaryOperation*
     *   ;
     *
     * postfixUnaryOperation
     *   : "++" : "--" : "!!"
     *   : typeArguments? valueArguments (getEntryPoint? functionLiteral)
     *   : typeArguments (getEntryPoint? functionLiteral)
     *   : arrayAccess
     *   : memberAccessOperation postfixUnaryExpression // TODO: Review
     *   ;
     */
  private fun parsePostfixExpression() {
    var expression = mark()

    var firstExpressionParsed = if (at(COLONCOLON)) parseDoubleColonSuffix(mark()) else parseAtomicExpression()

    while (true) {
      if (interruptedWithNewLine()) {
        break
      } else if (at(LBRACKET)) {
        parseArrayAccess()
        expression.done(ARRAY_ACCESS_EXPRESSION)
      } else if (parseCallSuffix()) {
        expression.done(CALL_EXPRESSION)
      } else if (at(DOT) || at(SAFE_ACCESS)) {
        val expressionType = if (at(DOT)) DOT_QUALIFIED_EXPRESSION else SAFE_ACCESS_EXPRESSION
        advance() // DOT or SAFE_ACCESS

        if (!firstExpressionParsed) {
          expression.drop()
          expression = mark()
        }

        parseSelectorCallExpression()

        if (firstExpressionParsed) {
          expression.done(expressionType)
        } else {
          firstExpressionParsed = true
          continue
        }
      } else if (atSet(Precedence.POSTFIX.operations)) {
        parseOperationReference()
        expression.done(POSTFIX_EXPRESSION)
      } else {
        skipQuestionMarksBeforeDoubleColon()
        if (!parseDoubleColonSuffix(expression)) {
          break
        }
      }
      expression = expression.precede()
    }
    expression.drop()
  }

  /*
     * callSuffix
     *   : typeArguments? valueArguments annotatedLambda
     *   : typeArguments annotatedLambda
     *   ;
     */
  private fun parseCallSuffix(): Boolean {
    if (parseCallWithClosure()) {
      // do nothing
    } else if (at(LPAR)) {
      parseValueArgumentList()
      parseCallWithClosure()
    } else if (at(LT)) {
      val typeArgumentList = mark()
      if (myKotlinParsing.tryParseTypeArgumentList(TYPE_ARGUMENT_LIST_STOPPERS)) {
        typeArgumentList.done(TYPE_ARGUMENT_LIST)
        if (!myBuilder.newlineBeforeCurrentToken() && at(LPAR)) parseValueArgumentList()
        parseCallWithClosure()
      } else {
        typeArgumentList.rollbackTo()
        return false
      }
    } else {
      return false
    }

    return true
  }

  /*
     * atomicExpression typeParameters? valueParameters? functionLiteral*
     */
  private fun parseSelectorCallExpression() {
    val mark = mark()
    parseAtomicExpression()
    if (!myBuilder.newlineBeforeCurrentToken() && parseCallSuffix()) {
      mark.done(CALL_EXPRESSION)
    } else {
      mark.drop()
    }
  }

  private fun parseOperationReference() {
    val operationReference = mark()
    advance() // operation
    operationReference.done(OPERATION_REFERENCE)
  }

  /*
     * annotatedLambda*
     */
  protected open fun parseCallWithClosure(): Boolean {
    var success = false

    while (true) {
      val argument = mark()

      if (!/* preferBlock = */parseAnnotatedLambda(false)) {
        argument.drop()
        break
      }

      argument.done(LAMBDA_ARGUMENT)
      success = true
    }

    return success
  }

  /*
     * annotatedLambda
     *  : ("@" annotationEntry)* labelDefinition? functionLiteral
     */
  private fun parseAnnotatedLambda(preferBlock: Boolean): Boolean {
    val annotated = mark()

    val wereAnnotations = myKotlinParsing.parseAnnotations(DEFAULT)
    val labeled = mark()

    val wasLabel = isAtLabelDefinitionOrMissingIdentifier
    if (wasLabel) {
      parseLabelDefinition()
    }

    if (!at(LBRACE)) {
      annotated.rollbackTo()
      return false
    }

    parseFunctionLiteral(preferBlock, /* collapse = */true)

    doneOrDrop(labeled, LABELED_EXPRESSION, wasLabel)
    doneOrDrop(annotated, ANNOTATED_EXPRESSION, wereAnnotations)

    return true
  }

  /*
     * atomicExpression
     *   : "this" label?
     *   : "super" ("<" type ">")? label?
     *   : objectLiteral
     *   : jump
     *   : if
     *   : when
     *   : try
     *   : loop
     *   : literalConstant
     *   : functionLiteral
     *   : declaration
     *   : SimpleName
     *   : collectionLiteral
     *   ;
     */
  private fun parseAtomicExpression(): Boolean {
    var ok = true

    when {
      at(LPAR) -> parseParenthesizedExpression()
      at(LBRACKET) -> parseCollectionLiteralExpression()
      at(THIS_KEYWORD) -> parseThisExpression()
      at(SUPER_KEYWORD) -> parseSuperExpression()
      at(OBJECT_KEYWORD) -> parseObjectLiteral()
      at(THROW_KEYWORD) -> parseThrow()
      at(RETURN_KEYWORD) -> parseReturn()
      at(CONTINUE_KEYWORD) -> parseJump(CONTINUE)
      at(BREAK_KEYWORD) -> parseJump(BREAK)
      at(IF_KEYWORD) -> parseIf()
      at(WHEN_KEYWORD) -> parseWhen()
      at(TRY_KEYWORD) -> parseTry()
      at(FOR_KEYWORD) -> parseFor()
      at(WHILE_KEYWORD) -> parseWhile()
      at(DO_KEYWORD) -> parseDoWhile()
      atSet(*LOCAL_DECLARATION_FIRST) && parseLocalDeclaration(/* rollbackIfDefinitelyNotExpression = */
        myBuilder.newlineBeforeCurrentToken(),
        false
      )
      -> {
        // declaration was parsed, do nothing
      }
      at(IDENTIFIER) -> parseSimpleNameExpression()
      at(LBRACE) -> parseFunctionLiteral()
      at(OPEN_QUOTE) -> parseStringTemplate()
      !parseLiteralConstant() -> {
        ok = false
        // TODO: better recovery if FIRST(element) did not match
        errorWithRecovery("Expecting an element", EXPRESSION_FOLLOW)
      }
    }

    return ok
  }

  /*
     * stringTemplate
     *   : OPEN_QUOTE stringTemplateElement* CLOSING_QUOTE
     *   ;
     */
  private fun parseStringTemplate() {
    assert(_at(OPEN_QUOTE))

    val template = mark()

    advance() // OPEN_QUOTE

    while (!eof()) {
      if (at(CLOSING_QUOTE) || at(DANGLING_NEWLINE)) {
        break
      }
      parseStringTemplateElement()
    }

    if (at(DANGLING_NEWLINE)) {
      errorAndAdvance("Expecting '\"'")
    } else {
      expect(CLOSING_QUOTE, "Expecting '\"'")
    }
    template.done(STRING_TEMPLATE)
  }

  /*
     * stringTemplateElement
     *   : RegularStringPart
     *   : ShortTemplateEntrySTART (SimpleName | "this")
     *   : EscapeSequence
     *   : longTemplate
     *   ;
     *
     * longTemplate
     *   : "${" expression "}"
     *   ;
     */
  private fun parseStringTemplateElement() {
    when {
      at(REGULAR_STRING_PART) -> {
        val mark = mark()
        advance() // REGULAR_STRING_PART
        mark.done(LITERAL_STRING_TEMPLATE_ENTRY)
      }
      at(ESCAPE_SEQUENCE) -> {
        val mark = mark()
        advance() // ESCAPE_SEQUENCE
        mark.done(ESCAPE_STRING_TEMPLATE_ENTRY)
      }
      at(SHORT_TEMPLATE_ENTRY_START) -> {
        val entry = mark()
        advance() // SHORT_TEMPLATE_ENTRY_START

        if (at(THIS_KEYWORD)) {
          val thisExpression = mark()
          val reference = mark()
          advance() // THIS_KEYWORD
          reference.done(REFERENCE_EXPRESSION)
          thisExpression.done(THIS_EXPRESSION)
        } else {
          val keyword = KEYWORD_TEXTS[myBuilder.tokenText]
          if (keyword != null) {
            myBuilder.remapCurrentToken(keyword)
            errorAndAdvance("Keyword cannot be used as a reference")
          } else {
            val reference = mark()
            expect(IDENTIFIER, "Expecting a name")
            reference.done(REFERENCE_EXPRESSION)
          }
        }

        entry.done(SHORT_STRING_TEMPLATE_ENTRY)
      }
      at(LONG_TEMPLATE_ENTRY_START) -> {
        val longTemplateEntry = mark()

        advance() // LONG_TEMPLATE_ENTRY_START

        while (!eof()) {
          val offset = myBuilder.currentOffset

          parseExpression()

          if (_at(LONG_TEMPLATE_ENTRY_END)) {
            advance()
            break
          } else {
            error("Expecting '}'")
            if (offset == myBuilder.currentOffset) {
              // Prevent hang if can't advance with parseExpression()
              advance()
            }
          }
        }

        longTemplateEntry.done(LONG_STRING_TEMPLATE_ENTRY)
      }
      else -> errorAndAdvance("Unexpected token in a string template")
    }
  }

  /*
     * literalConstant
     *   : "true" | "false"
     *   : stringTemplate
     *   : NoEscapeString
     *   : IntegerLiteral
     *   : CharacterLiteral
     *   : FloatLiteral
     *   : "null"
     *   ;
     */
  private fun parseLiteralConstant(): Boolean {
    when {
      at(TRUE_KEYWORD) || at(FALSE_KEYWORD) -> parseOneTokenExpression(BOOLEAN_CONSTANT)
      at(INTEGER_LITERAL) -> parseOneTokenExpression(INTEGER_CONSTANT)
      at(CHARACTER_LITERAL) -> parseOneTokenExpression(CHARACTER_CONSTANT)
      at(FLOAT_LITERAL) -> parseOneTokenExpression(FLOAT_CONSTANT)
      at(NULL_KEYWORD) -> parseOneTokenExpression(NULL)
      else -> return false
    }
    return true
  }

  /*
     * when
     *   : "when" ("(" (modifiers "val" SimpleName "=")? element ")")? "{"
     *         whenEntry*
     *     "}"
     *   ;
     */
  private fun parseWhen() {
    assert(_at(WHEN_KEYWORD))

    val `when` = mark()

    advance() // WHEN_KEYWORD

    // Parse condition
    myBuilder.disableNewlines()
    if (at(LPAR)) {
      advanceAt(LPAR)

      val property = mark()
      myKotlinParsing.parseModifierList(DEFAULT, TokenSet.create(EQ, RPAR))
      if (at(VAL_KEYWORD) || at(VAR_KEYWORD)) {
        myKotlinParsing.parseLocalProperty(false)
        property.done(PROPERTY)
      } else {
        property.rollbackTo()
        parseExpression()
      }

      expect(RPAR, "Expecting ')'")
    }
    myBuilder.restoreNewlinesState()

    // Parse when block
    myBuilder.enableNewlines()
    if (expect(LBRACE, "Expecting '{'")) {
      while (!eof() && !at(RBRACE)) {
        parseWhenEntry()
      }

      expect(RBRACE, "Expecting '}'")
    }
    myBuilder.restoreNewlinesState()

    `when`.done(WHEN)
  }

  /*
     * whenEntry
     *   // TODO : consider empty after ->
     *   : whenCondition{","} "->" element SEMI
     *   : "else" "->" element SEMI
     *   ;
     */
  private fun parseWhenEntry() {
    val entry = mark()

    if (at(ELSE_KEYWORD)) {
      advance() // ELSE_KEYWORD

      if (!at(ARROW)) {
        errorUntil("Expecting '->'", TokenSet.create(ARROW, LBRACE, RBRACE, EOL_OR_SEMICOLON))
      }

      if (at(ARROW)) {
        advance() // ARROW

        if (atSet(WHEN_CONDITION_RECOVERY_SET)) {
          error("Expecting an element")
        } else {
          parseControlStructureBody()
        }
      } else if (at(LBRACE)) { // no arrow, probably it's simply missing
        parseControlStructureBody()
      } else if (!atSet(WHEN_CONDITION_RECOVERY_SET)) {
        errorAndAdvance("Expecting '->'")
      }
    } else {
      parseWhenEntryNotElse()
    }

    entry.done(WHEN_ENTRY)
    consumeIf(SEMICOLON)
  }

  /*
     * : whenCondition{","} "->" element SEMI
     */
  private fun parseWhenEntryNotElse() {
    while (true) {
      while (at(COMMA)) errorAndAdvance("Expecting a when-condition")
      parseWhenCondition()
      if (!at(COMMA)) break
      advance() // COMMA
    }

    expect(ARROW, "Expecting '->'", WHEN_CONDITION_RECOVERY_SET)
    if (atSet(WHEN_CONDITION_RECOVERY_SET)) {
      error("Expecting an element")
    } else {
      parseControlStructureBody()
    }
    // SEMI is consumed in parseWhenEntry
  }

  /*
     * whenCondition
     *   : expression
     *   : ("in" | "!in") expression
     *   : ("is" | "!is") isRHS
     *   ;
     */
  private fun parseWhenCondition() {
    val condition = mark()
    myBuilder.disableNewlines()
    when {
      at(IN_KEYWORD) || at(NOT_IN) -> {
        val mark = mark()
        advance() // IN_KEYWORD or NOT_IN
        mark.done(OPERATION_REFERENCE)


        when {
          atSet(WHEN_CONDITION_RECOVERY_SET_WITH_ARROW) -> error("Expecting an element")
          else -> parseExpression()
        }
        condition.done(WHEN_CONDITION_IN_RANGE)
      }
      at(IS_KEYWORD) || at(NOT_IS) -> {
        advance() // IS_KEYWORD or NOT_IS

        when {
          atSet(WHEN_CONDITION_RECOVERY_SET_WITH_ARROW) -> error("Expecting a type")
          else -> myKotlinParsing.parseTypeRef()
        }
        condition.done(WHEN_CONDITION_IS_PATTERN)
      }
      else -> {
        when {
          atSet(WHEN_CONDITION_RECOVERY_SET_WITH_ARROW) -> error("Expecting an expression, is-condition or in-condition")
          else -> parseExpression()
        }
        condition.done(WHEN_CONDITION_EXPRESSION)
      }
    }
    myBuilder.restoreNewlinesState()
  }

  /*
     * arrayAccess
     *   : "[" element{","} "]"
     *   ;
     */
  private fun parseArrayAccess() {
    parseAsCollectionLiteralExpression(INDICES, false, "Expecting an index element")
  }

  /*
     * collectionLiteral
     *   : "[" element{","}? "]"
     *   ;
     */
  private fun parseCollectionLiteralExpression() {
    parseAsCollectionLiteralExpression(COLLECTION_LITERAL_EXPRESSION, true, "Expecting an element")
  }

  private fun parseAsCollectionLiteralExpression(
    nodeType: KtNodeType,
    canBeEmpty: Boolean,
    missingElementErrorMessage: String
  ) {
    assert(_at(LBRACKET))

    val innerExpressions = mark()

    myBuilder.disableNewlines()
    advance() // LBRACKET

    if (!canBeEmpty && at(RBRACKET)) {
      error(missingElementErrorMessage)
    } else {
      parseInnerExpressions(missingElementErrorMessage)
    }

    expect(RBRACKET, "Expecting ']'")
    myBuilder.restoreNewlinesState()

    innerExpressions.done(nodeType)
  }

  private fun parseInnerExpressions(missingElementErrorMessage: String) {
    var firstElement = true
    while (true) {
      if (at(COMMA)) errorAndAdvance(missingElementErrorMessage)
      if (at(RBRACKET)) {
        if (firstElement) {
          break
        } else {
          error(missingElementErrorMessage)
        }
        break
      }
      parseExpression()

      firstElement = false

      if (!at(COMMA)) break
      advance() // COMMA
    }
  }

  /*
     * SimpleName
     */
  fun parseSimpleNameExpression() {
    val simpleName = mark()
    expect(IDENTIFIER, "Expecting an identifier")
    simpleName.done(REFERENCE_EXPRESSION)
  }

  /*
     * modifiers declarationRest
     */
  private fun parseLocalDeclaration(rollbackIfDefinitelyNotExpression: Boolean, isScriptTopLevel: Boolean): Boolean {
    val decl = mark()
    val detector = KotlinParsing.ModifierDetector()
    myKotlinParsing.parseModifierList(detector, DEFAULT, TokenSet.EMPTY)

    val declType =
      parseLocalDeclarationRest(detector.isEnumDetected, rollbackIfDefinitelyNotExpression, isScriptTopLevel)

    if (declType != null) {
      // we do not attach preceding comments (non-doc) to local variables because they are likely commenting a few statements below
      AbstractKotlinParsing.Companion.closeDeclarationWithCommentBinders(
        decl, declType,
        declType !== KtNodeTypes.PROPERTY && declType !== KtNodeTypes.DESTRUCTURING_DECLARATION
      )
      return true
    } else {
      decl.rollbackTo()
      return false
    }
  }

  /*
     * functionLiteral  // one can use "it" as a parameter name
     *   : "{" expressions "}"
     *   : "{" (modifiers SimpleName (":" type)?){","} "->" statements "}"
     *   ;
     */
  private fun parseFunctionLiteral() {
    parseFunctionLiteral(/* preferBlock = */false, /* collapse = */true)
  }

  /**
   * If it has no ->, it's a block, otherwise a function literal
   */
  fun parseFunctionLiteral(preferBlock: Boolean, collapse: Boolean) {
    assert(_at(LBRACE))

    val literalExpression = mark()

    val literal = mark()

    myBuilder.enableNewlines()
    advance() // LBRACE

    var paramsFound = false

    if (at(ARROW)) {
      //   { -> ...}
      mark().done(VALUE_PARAMETER_LIST)
      advance() // ARROW
      paramsFound = true
    } else if (at(IDENTIFIER) || at(COLON) || at(LPAR)) {
      // Try to parse a simple name list followed by an ARROW
      //   {a -> ...}
      //   {a, b -> ...}
      //   {(a, b) -> ... }
      val rollbackMarker = mark()
      val nextToken = lookahead(1)
      val preferParamsToExpressions = nextToken === COMMA || nextToken === COLON
      parseFunctionLiteralParameterList()

      paramsFound = if (preferParamsToExpressions)
        rollbackOrDrop(rollbackMarker, ARROW, "An -> is expected", RBRACE)
      else
        rollbackOrDropAt(rollbackMarker, ARROW)
    }

    if (!paramsFound && preferBlock) {
      literal.drop()
      parseStatements()
      expect(RBRACE, "Expecting '}'")
      literalExpression.done(BLOCK)
      myBuilder.restoreNewlinesState()

      return
    }

    if (collapse) {
      advanceLambdaBlock()
      literal.done(FUNCTION_LITERAL)
      literalExpression.collapse(LAMBDA_EXPRESSION)
    } else {
      val body = mark()
      parseStatements()

      body.done(BLOCK)
      body.setCustomEdgeTokenBinders(PRECEDING_ALL_COMMENTS_BINDER, TRAILING_ALL_COMMENTS_BINDER)

      expect(RBRACE, "Expecting '}'")
      literal.done(FUNCTION_LITERAL)
      literalExpression.done(LAMBDA_EXPRESSION)
    }

    myBuilder.restoreNewlinesState()
  }

  private fun advanceLambdaBlock() {
    var braceCount = 1
    while (!eof()) {
      if (_at(LBRACE)) {
        braceCount++
      } else if (_at(RBRACE)) {
        braceCount--
      }

      advance()

      if (braceCount == 0) {
        break
      }
    }
  }

  private fun rollbackOrDropAt(rollbackMarker: PsiBuilder.Marker, dropAt: IElementType): Boolean {
    if (at(dropAt)) {
      advance() // dropAt
      rollbackMarker.drop()
      return true
    }
    rollbackMarker.rollbackTo()
    return false
  }

  private fun rollbackOrDrop(
    rollbackMarker: PsiBuilder.Marker,
    expected: KtToken, expectMessage: String,
    validForDrop: IElementType
  ): Boolean {
    if (at(expected)) {
      advance() // dropAt
      rollbackMarker.drop()
      return true
    } else if (at(validForDrop)) {
      rollbackMarker.drop()
      expect(expected, expectMessage)
      return true
    }

    rollbackMarker.rollbackTo()
    return false
  }


  /*
     * lambdaParameter{","}
     *
     * lambdaParameter
     *   : variableDeclarationEntry
     *   : multipleVariableDeclarations (":" type)?
     */
  private fun parseFunctionLiteralParameterList() {
    val parameterList = mark()

    while (!eof()) {
      val parameter = mark()

      when {
        at(COLON) -> error("Expecting parameter name")
        at(LPAR) -> {
          val destructuringDeclaration = mark()
          myKotlinParsing.parseMultiDeclarationName(TOKEN_SET_TO_FOLLOW_AFTER_DESTRUCTURING_DECLARATION_IN_LAMBDA)
          destructuringDeclaration.done(DESTRUCTURING_DECLARATION)
        }
        else -> expect(IDENTIFIER, "Expecting parameter name", TokenSet.create(ARROW))
      }

      if (at(COLON)) {
        advance() // COLON
        myKotlinParsing.parseTypeRef(TokenSet.create(ARROW, COMMA))
      }
      parameter.done(VALUE_PARAMETER)

      if (at(ARROW)) {
        break
      } else if (at(COMMA)) {
        advance() // COMMA
      } else {
        error("Expecting '->' or ','")
        break
      }
    }

    parameterList.done(VALUE_PARAMETER_LIST)
  }

  /*
     * expressions
     *   : SEMI* statement{SEMI+} SEMI*
     */
  @JvmOverloads
  fun parseStatements(isScriptTopLevel: Boolean = false) {
    while (at(SEMICOLON)) advance() // SEMICOLON
    while (!eof() && !at(RBRACE)) {
      if (!atSet(STATEMENT_FIRST)) {
        errorAndAdvance("Expecting an element")
      }
      if (atSet(STATEMENT_FIRST)) {
        parseStatement(isScriptTopLevel)
      }
      if (at(SEMICOLON)) {
        while (at(SEMICOLON)) advance() // SEMICOLON
      } else if (at(RBRACE)) {
        break
      } else if (!myBuilder.newlineBeforeCurrentToken()) {
        val severalStatementsError = "Unexpected tokens (use ';' to separate expressions on the same line)"

        if (atSet(STATEMENT_NEW_LINE_QUICK_RECOVERY_SET)) {
          error(severalStatementsError)
        } else {
          errorUntil(severalStatementsError, TokenSet.create(EOL_OR_SEMICOLON, LBRACE, RBRACE))
        }
      }
    }
  }

  /*
     * statement
     *  : declaration
     *  : blockLevelExpression
     *  ;
     */
  private fun parseStatement(isScriptTopLevel: Boolean) {
    if (!/* rollbackIfDefinitelyNotExpression = *//* isScriptTopLevel = */parseLocalDeclaration(
        false,
        isScriptTopLevel
      )
    ) {
      if (!atSet(EXPRESSION_FIRST)) {
        errorAndAdvance("Expecting a statement")
      } else if (isScriptTopLevel) {
        val scriptInitializer = mark()
        parseBlockLevelExpression()
        scriptInitializer.done(SCRIPT_INITIALIZER)
      } else {
        parseBlockLevelExpression()
      }
    }
  }

  /*
     * blockLevelExpression
     *  : annotations + ("\n")+ expression
     *  ;
     */
  private fun parseBlockLevelExpression() {
    if (at(AT)) {
      val expression = mark()
      myKotlinParsing.parseAnnotations(DEFAULT)

      if (!myBuilder.newlineBeforeCurrentToken()) {
        expression.rollbackTo()
        parseExpression()
        return
      }

      parseBlockLevelExpression()
      expression.done(ANNOTATED_EXPRESSION)
      return
    }

    parseExpression()
  }

  /*
     * declaration
     *   : function
     *   : property
     *   : extension
     *   : class
     *   : typeAlias
     *   : object
     *   ;
     */
  private fun parseLocalDeclarationRest(
    isEnum: Boolean,
    failIfDefinitelyNotExpression: Boolean,
    isScriptTopLevel: Boolean
  ): IElementType? {
    val keywordToken = tt()
    var declType: IElementType? = null

    if (failIfDefinitelyNotExpression) {
      return if (keywordToken !== FUN_KEYWORD) null else myKotlinParsing.parseFunction(/* failIfIdentifierExists = */
        true
      )

    }

    when {
      keywordToken === CLASS_KEYWORD || keywordToken === INTERFACE_KEYWORD -> declType = myKotlinParsing.parseClass(isEnum)
      keywordToken === FUN_KEYWORD -> declType = myKotlinParsing.parseFunction()
      keywordToken === VAL_KEYWORD || keywordToken === VAR_KEYWORD -> declType = myKotlinParsing.parseLocalProperty(isScriptTopLevel)
      keywordToken === TYPE_ALIAS_KEYWORD -> declType = myKotlinParsing.parseTypeAlias()
      keywordToken === OBJECT_KEYWORD -> {
        // Object expression may appear at the statement position: should parse it
        // as expression instead of object declaration
        // sample:
        // {
        //   object : Thread() {
        //   }
        // }
        val lookahead = lookahead(1)
        if (lookahead === COLON || lookahead === LBRACE) {
          return null
        }

        myKotlinParsing.parseObject(NameParsingMode.REQUIRED, true)
        declType = OBJECT_DECLARATION
      }
    }
    return declType
  }

  /*
     * doWhile
     *   : "do" element "while" "(" element ")"
     *   ;
     */
  private fun parseDoWhile() {
    assert(_at(DO_KEYWORD))

    val loop = mark()

    advance() // DO_KEYWORD

    if (!at(WHILE_KEYWORD)) {
      parseLoopBody()
    }

    if (expect(WHILE_KEYWORD, "Expecting 'while' followed by a post-condition")) {
      parseCondition()
    }

    loop.done(DO_WHILE)
  }

  /*
     * while
     *   : "while" "(" element ")" element
     *   ;
     */
  private fun parseWhile() {
    assert(_at(WHILE_KEYWORD))

    val loop = mark()

    advance() // WHILE_KEYWORD

    parseCondition()

    parseLoopBody()

    loop.done(WHILE)
  }

  /*
     * for
     *   : "for" "(" annotations ("val" | "var")? (multipleVariableDeclarations | variableDeclarationEntry) "in" expression ")" expression
     *   ;
     *
     *   TODO: empty loop body (at the end of the block)?
     */
  private fun parseFor() {
    assert(_at(FOR_KEYWORD))

    val loop = mark()

    advance() // FOR_KEYWORD

    if (expect(LPAR, "Expecting '(' to open a loop range", EXPRESSION_FIRST)) {
      myBuilder.disableNewlines()

      if (!at(RPAR)) {
        val parameter = mark()

        if (!at(IN_KEYWORD)) {
          myKotlinParsing.parseModifierList(DEFAULT, TokenSet.create(IN_KEYWORD, RPAR, COLON))
        }

        if (at(VAL_KEYWORD) || at(VAR_KEYWORD)) advance() // VAL_KEYWORD or VAR_KEYWORD

        if (at(LPAR)) {
          val destructuringDeclaration = mark()
          myKotlinParsing.parseMultiDeclarationName(TokenSet.create(IN_KEYWORD, LBRACE))
          destructuringDeclaration.done(DESTRUCTURING_DECLARATION)
        } else {
          expect(IDENTIFIER, "Expecting a variable name", TokenSet.create(COLON, IN_KEYWORD))

          if (at(COLON)) {
            advance() // COLON
            myKotlinParsing.parseTypeRef(TokenSet.create(IN_KEYWORD))
          }
        }
        parameter.done(VALUE_PARAMETER)

        if (expect(IN_KEYWORD, "Expecting 'in'", TokenSet.create(LPAR, LBRACE, RPAR))) {
          val range = mark()
          parseExpression()
          range.done(LOOP_RANGE)
        }
      } else {
        error("Expecting a variable name")
      }

      expectNoAdvance(RPAR, "Expecting ')'")
      myBuilder.restoreNewlinesState()
    }

    parseLoopBody()

    loop.done(FOR)
  }

  private fun parseControlStructureBody() {
    if (!/* preferBlock = */parseAnnotatedLambda(true)) {
      parseBlockLevelExpression()
    }
  }

  /*
     * element
     */
  private fun parseLoopBody() {
    val body = mark()
    if (!at(SEMICOLON)) {
      parseControlStructureBody()
    }
    body.done(BODY)
  }

  /*
     * try
     *   : "try" block catchBlock* finallyBlock?
     *   ;
     * catchBlock
     *   : "catch" "(" annotations SimpleName ":" userType ")" block
     *   ;
     *
     * finallyBlock
     *   : "finally" block
     *   ;
     */
  private fun parseTry() {
    assert(_at(TRY_KEYWORD))

    val tryExpression = mark()

    advance() // TRY_KEYWORD

    myKotlinParsing.parseBlock()

    var catchOrFinally = false
    while (at(CATCH_KEYWORD)) {
      catchOrFinally = true
      val catchBlock = mark()
      advance() // CATCH_KEYWORD

      val recoverySet = TokenSet.create(LBRACE, RBRACE, FINALLY_KEYWORD, CATCH_KEYWORD)
      if (atSet(recoverySet)) {
        error("Expecting exception variable declaration")
      } else {
        val parameters = mark()
        expect(LPAR, "Expecting '('", recoverySet)
        if (!atSet(recoverySet)) {
          myKotlinParsing.parseValueParameter(/*typeRequired = */true)
          expect(RPAR, "Expecting ')'", recoverySet)
        } else {
          error("Expecting exception variable declaration")
        }
        parameters.done(VALUE_PARAMETER_LIST)
      }

      if (at(LBRACE)) {
        myKotlinParsing.parseBlock()
      } else {
        error("Expecting a block: { ... }")
      }
      catchBlock.done(CATCH)
    }

    if (at(FINALLY_KEYWORD)) {
      catchOrFinally = true
      val finallyBlock = mark()

      advance() // FINALLY_KEYWORD

      myKotlinParsing.parseBlock()

      finallyBlock.done(FINALLY)
    }

    if (!catchOrFinally) {
      error("Expecting 'catch' or 'finally'")
    }

    tryExpression.done(TRY)
  }

  /*
     * if
     *   : "if" "(" element ")" element SEMI? ("else" element)?
     *   ;
     */
  private fun parseIf() {
    assert(_at(IF_KEYWORD))

    val marker = mark()

    advance() //IF_KEYWORD

    parseCondition()

    val thenBranch = mark()
    if (!at(ELSE_KEYWORD) && !at(SEMICOLON)) {
      parseControlStructureBody()
    }
    if (at(SEMICOLON) && lookahead(1) === ELSE_KEYWORD) {
      advance() // SEMICOLON
    }
    thenBranch.done(THEN)

    // lookahead for arrow is needed to prevent capturing of whenEntry like "else -> "
    if (at(ELSE_KEYWORD) && lookahead(1) !== ARROW) {
      advance() // ELSE_KEYWORD

      val elseBranch = mark()
      if (!at(SEMICOLON)) {
        parseControlStructureBody()
      }
      elseBranch.done(ELSE)
    }

    marker.done(IF)
  }

  /*
     * "(" element ")"
     */
  private fun parseCondition() {
    myBuilder.disableNewlines()

    if (expect(LPAR, "Expecting a condition in parentheses '(...)'", EXPRESSION_FIRST)) {
      val condition = mark()
      parseExpression()
      condition.done(CONDITION)
      expect(RPAR, "Expecting ')")
    }

    myBuilder.restoreNewlinesState()
  }

  /*
     * : "continue" getEntryPoint?
     * : "break" getEntryPoint?
     */
  private fun parseJump(type: KtNodeType) {
    assert(_at(BREAK_KEYWORD) || _at(CONTINUE_KEYWORD))

    val marker = mark()

    advance() // BREAK_KEYWORD or CONTINUE_KEYWORD

    parseLabelReferenceWithNoWhitespace()

    marker.done(type)
  }

  /*
     * "return" getEntryPoint? element?
     */
  private fun parseReturn() {
    assert(_at(RETURN_KEYWORD))

    val returnExpression = mark()

    advance() // RETURN_KEYWORD

    parseLabelReferenceWithNoWhitespace()

    if (atSet(EXPRESSION_FIRST) && !at(EOL_OR_SEMICOLON)) parseExpression()

    returnExpression.done(RETURN)
  }

  /*
     * labelReference?
     */
  private fun parseLabelReferenceWithNoWhitespace() {
    if (at(AT) && !myBuilder.newlineBeforeCurrentToken()) {
      if (WHITE_SPACE_OR_COMMENT_BIT_SET.contains(myBuilder.rawLookup(-1))) {
        error("There should be no space or comments before '@' in label reference")
      }
      parseLabelReference()
    }
  }

  /*
     * IDENTIFIER "@"
     */
  private fun parseLabelDefinition() {
    val labelWrap = mark()
    val mark = mark()

    assert(_at(IDENTIFIER) && myBuilder.rawLookup(1) === AT) { "Callers must check that current token is IDENTIFIER followed with '@'" }

    advance() // IDENTIFIER
    advance() // AT

    mark.done(LABEL)

    labelWrap.done(LABEL_QUALIFIER)
  }

  /*
     * "@" IDENTIFIER
     */
  private fun parseLabelReference() {
    assert(_at(AT))

    val labelWrap = mark()

    val mark = mark()

    if (myBuilder.rawLookup(1) !== IDENTIFIER) {
      errorAndAdvance("Label must be named") // AT
      labelWrap.drop()
      mark.drop()
      return
    }

    advance() // AT
    advance() // IDENTIFIER

    mark.done(LABEL)

    labelWrap.done(LABEL_QUALIFIER)
  }

  /*
     * : "throw" element
     */
  private fun parseThrow() {
    assert(_at(THROW_KEYWORD))

    val marker = mark()

    advance() // THROW_KEYWORD

    parseExpression()

    marker.done(THROW)
  }

  /*
     * "(" expression ")"
     */
  private fun parseParenthesizedExpression() {
    assert(_at(LPAR))

    val mark = mark()

    myBuilder.disableNewlines()
    advance() // LPAR
    if (at(RPAR)) {
      error("Expecting an expression")
    } else {
      parseExpression()
    }

    expect(RPAR, "Expecting ')'")
    myBuilder.restoreNewlinesState()

    mark.done(PARENTHESIZED)
  }

  /*
     * "this" label?
     */
  private fun parseThisExpression() {
    assert(_at(THIS_KEYWORD))
    val mark = mark()

    val thisReference = mark()
    advance() // THIS_KEYWORD
    thisReference.done(REFERENCE_EXPRESSION)

    parseLabelReferenceWithNoWhitespace()

    mark.done(THIS_EXPRESSION)
  }

  /*
     * "this" ("<" type ">")? label?
     */
  private fun parseSuperExpression() {
    assert(_at(SUPER_KEYWORD))
    val mark = mark()

    val superReference = mark()
    advance() // SUPER_KEYWORD
    superReference.done(REFERENCE_EXPRESSION)

    if (at(LT)) {
      // This may be "super < foo" or "super<foo>", thus the backtracking
      val supertype = mark()

      myBuilder.disableNewlines()
      advance() // LT

      myKotlinParsing.parseTypeRef()

      if (at(GT)) {
        advance() // GT
        supertype.drop()
      } else {
        supertype.rollbackTo()
      }
      myBuilder.restoreNewlinesState()
    }
    parseLabelReferenceWithNoWhitespace()

    mark.done(SUPER_EXPRESSION)
  }

  /*
     * valueArguments
     *   : "(" (SimpleName "=")? "*"? element{","} ")"
     *   ;
     */
  fun parseValueArgumentList() {
    val list = mark()

    myBuilder.disableNewlines()

    if (expect(LPAR, "Expecting an argument list", EXPRESSION_FOLLOW)) {
      if (!at(RPAR)) {
        while (true) {
          while (at(COMMA)) errorAndAdvance("Expecting an argument")
          parseValueArgument()
          if (at(COLON) && lookahead(1) === IDENTIFIER) {
            errorAndAdvance("Unexpected type specification", 2)
          }
          if (!at(COMMA)) break
          advance() // COMMA
          if (at(RPAR)) {
            error("Expecting an argument")
            break
          }
        }
      }

      expect(RPAR, "Expecting ')'", EXPRESSION_FOLLOW)
    }

    myBuilder.restoreNewlinesState()

    list.done(VALUE_ARGUMENT_LIST)
  }

  /*
     * (SimpleName "=")? "*"? element
     */
  private fun parseValueArgument() {
    val argument = mark()
    if (at(IDENTIFIER) && lookahead(1) === EQ) {
      val argName = mark()
      val reference = mark()
      advance() // IDENTIFIER
      reference.done(REFERENCE_EXPRESSION)
      argName.done(VALUE_ARGUMENT_NAME)
      advance() // EQ
    }
    if (at(MUL)) {
      advance() // MUL
    }
    parseExpression()
    argument.done(VALUE_ARGUMENT)
  }

  /*
     * "object" (":" delegationSpecifier{","})? classBody // Cannot make class body optional: foo(object : F, A)
     */
  fun parseObjectLiteral() {
    val literal = mark()
    val declaration = mark()
    myKotlinParsing.parseObject(NameParsingMode.PROHIBITED, false) // Body is not optional because of foo(object : A, B)
    declaration.done(OBJECT_DECLARATION)
    literal.done(OBJECT_LITERAL)
  }

  private fun parseOneTokenExpression(type: KtNodeType) {
    val mark = mark()
    advance()
    mark.done(type)
  }

  override fun create(builder: SemanticWhitespaceAwarePsiBuilder): KotlinParsing {
    return myKotlinParsing.create(builder)
  }

  private fun interruptedWithNewLine(): Boolean {
    return !ALLOW_NEWLINE_OPERATIONS.contains(tt()) && myBuilder.newlineBeforeCurrentToken()
  }

  companion object {
    private val WHEN_CONDITION_RECOVERY_SET =
      TokenSet.create(RBRACE, IN_KEYWORD, NOT_IN, IS_KEYWORD, NOT_IS, ELSE_KEYWORD)
    private val WHEN_CONDITION_RECOVERY_SET_WITH_ARROW =
      TokenSet.create(RBRACE, IN_KEYWORD, NOT_IN, IS_KEYWORD, NOT_IS, ELSE_KEYWORD, ARROW, DOT)


    private val KEYWORD_TEXTS = tokenSetToMap(KEYWORDS)

    private val LOCAL_DECLARATION_FIRST =
      arrayOf<IElementType>(CLASS_KEYWORD, INTERFACE_KEYWORD, FUN_KEYWORD, VAL_KEYWORD, VAR_KEYWORD, TYPE_ALIAS_KEYWORD)
    private val TOKEN_SET_TO_FOLLOW_AFTER_DESTRUCTURING_DECLARATION_IN_LAMBDA = TokenSet.create(ARROW, COMMA, COLON)

    private fun tokenSetToMap(tokens: TokenSet): Map<String, KtToken> {
      val builder = HashMap<String, KtToken>()
      for (token in tokens.types) {
        builder[token.toString()] = token as KtToken
      }
      return builder
    }

    private val TYPE_ARGUMENT_LIST_STOPPERS = TokenSet.create(
      INTEGER_LITERAL,
      FLOAT_LITERAL,
      CHARACTER_LITERAL,
      OPEN_QUOTE,
      PACKAGE_KEYWORD,
      AS_KEYWORD,
      TYPE_ALIAS_KEYWORD,
      INTERFACE_KEYWORD,
      CLASS_KEYWORD,
      THIS_KEYWORD,
      VAL_KEYWORD,
      VAR_KEYWORD,
      FUN_KEYWORD,
      FOR_KEYWORD,
      NULL_KEYWORD,
      TRUE_KEYWORD,
      FALSE_KEYWORD,
      IS_KEYWORD,
      THROW_KEYWORD,
      RETURN_KEYWORD,
      BREAK_KEYWORD,
      CONTINUE_KEYWORD,
      OBJECT_KEYWORD,
      IF_KEYWORD,
      TRY_KEYWORD,
      ELSE_KEYWORD,
      WHILE_KEYWORD,
      DO_KEYWORD,
      WHEN_KEYWORD,
      RBRACKET,
      RBRACE,
      RPAR,
      PLUSPLUS,
      MINUSMINUS,
      EXCLEXCL,
      //            MUL,
      PLUS,
      MINUS,
      EXCL,
      DIV,
      PERC,
      LTEQ,
      // TODO GTEQ,   foo<bar, baz>=x
      EQEQEQ,
      EXCLEQEQEQ,
      EQEQ,
      EXCLEQ,
      ANDAND,
      OROR,
      SAFE_ACCESS,
      ELVIS,
      SEMICOLON,
      RANGE,
      EQ,
      MULTEQ,
      DIVEQ,
      PERCEQ,
      PLUSEQ,
      MINUSEQ,
      NOT_IN,
      NOT_IS,
      COLONCOLON,
      COLON
    )

    /*package*/ internal val EXPRESSION_FIRST = TokenSet.create(
      // Prefix
      MINUS, PLUS, MINUSMINUS, PLUSPLUS,
      EXCL, EXCLEXCL, // Joining complex tokens makes it necessary to put EXCLEXCL here
      // Atomic

      COLONCOLON, // callable reference

      LPAR, // parenthesized

      // literal constant
      TRUE_KEYWORD, FALSE_KEYWORD,
      OPEN_QUOTE,
      INTEGER_LITERAL, CHARACTER_LITERAL, FLOAT_LITERAL,
      NULL_KEYWORD,

      LBRACE, // functionLiteral
      FUN_KEYWORD, // expression function

      THIS_KEYWORD, // this
      SUPER_KEYWORD, // super

      IF_KEYWORD, // if
      WHEN_KEYWORD, // when
      TRY_KEYWORD, // try
      OBJECT_KEYWORD, // object

      // jump
      THROW_KEYWORD,
      RETURN_KEYWORD,
      CONTINUE_KEYWORD,
      BREAK_KEYWORD,

      // loop
      FOR_KEYWORD,
      WHILE_KEYWORD,
      DO_KEYWORD,

      IDENTIFIER, // SimpleName

      AT, // Just for better recovery and maybe for annotations

      LBRACKET // Collection literal expression
    )

    val STATEMENT_FIRST = TokenSet.orSet(
      EXPRESSION_FIRST,
      TokenSet.create(
        // declaration
        FUN_KEYWORD,
        VAL_KEYWORD, VAR_KEYWORD,
        INTERFACE_KEYWORD,
        CLASS_KEYWORD,
        TYPE_ALIAS_KEYWORD
      ),
      MODIFIER_KEYWORDS
    )

    private val STATEMENT_NEW_LINE_QUICK_RECOVERY_SET = TokenSet.orSet(
      TokenSet.andSet(STATEMENT_FIRST, TokenSet.andNot(KEYWORDS, TokenSet.create(IN_KEYWORD))),
      TokenSet.create(EOL_OR_SEMICOLON)
    )

    /*package*/ internal val EXPRESSION_FOLLOW = TokenSet.create(
      EOL_OR_SEMICOLON, ARROW, COMMA, RBRACE, RPAR, RBRACKET
    )

    val ALLOW_NEWLINE_OPERATIONS = TokenSet.create(
      DOT, SAFE_ACCESS,
      COLON, AS_KEYWORD, `AS_SAFE`,
      ELVIS,
      // Can't allow `is` and `!is` because of when entry conditions: IS_KEYWORD, NOT_IS,
      ANDAND,
      OROR
    )

    val ALL_OPERATIONS: TokenSet

    init {
      val operations = HashSet<IElementType>()
      val values = Precedence.values()
      for (precedence in values) {
        operations.addAll(Arrays.asList(*precedence.operations.types))
      }
      ALL_OPERATIONS = TokenSet.create(*operations.toTypedArray())
    }

    init {
      val operations = OPERATIONS.types
      val opSet = HashSet(Arrays.asList(*operations))
      val usedOperations = ALL_OPERATIONS.types
      val usedSet = HashSet(Arrays.asList(*usedOperations))

      if (opSet.size > usedSet.size) {
        opSet.removeAll(usedSet)
        assert(false) { opSet }
      }
      assert(usedSet.size == opSet.size) { "Either some ops are unused, or something a non-op is used" }

      usedSet.removeAll(opSet)

      assert(usedSet.isEmpty()) { usedSet.toString() }
    }

    private fun doneOrDrop(
      marker: PsiBuilder.Marker,
      type: IElementType,
      condition: Boolean
    ) {
      if (condition) {
        marker.done(type)
      } else {
        marker.drop()
      }
    }
  }
}/*
     * expressions
     *   : SEMI* statement{SEMI+} SEMI*
     */
