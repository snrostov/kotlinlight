/*
 * Copyright 2000-2015 JetBrains s.r.o.
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
package com.intellij.lang;

import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * The IDEA side of a custom language parser. Provides lexical analysis results to the
 * plugin and allows the plugin to build the AST tree.
 */
public interface PsiBuilder {
    /**
     * Returns the complete text being parsed.
     *
     * @return the text being parsed
     */
    CharSequence getOriginalText();

    /**
     * Advances the lexer to the next token, skipping whitespace and comment tokens.
     */
    void advanceLexer();

    /**
     * Returns the type of current token from the lexer.
     *
     * @return the token type, or null when the token stream is over.
     */
    @Nullable
    IElementType getTokenType();

    /**
     * Slightly easier way to what ITokenTypeRemapper does (i.e. it just remaps current token to a given type).
     *
     * @param type new type for the current token.
     */
    void remapCurrentToken(IElementType type);

    /**
     * See what token type is in {@code steps} ahead
     *
     * @param steps 0 is current token (i.e. the same {@link PsiBuilder#getTokenType()} returns)
     * @return type element which getTokenType() will return if we call advance {@code steps} times in a row
     */
    @Nullable
    IElementType lookAhead(int steps);

    /**
     * See what token type is in {@code steps} ahead / behind
     *
     * @param steps 0 is current token (i.e. the same {@link PsiBuilder#getTokenType()} returns)
     * @return type element ahead or behind, including whitespace / comment tokens
     */
    @Nullable
    IElementType rawLookup(int steps);

    /**
     * See what token type is in {@code steps} ahead / behind current position
     *
     * @param steps 0 is current token (i.e. the same {@link PsiBuilder#getTokenType()} returns)
     * @return offset type element ahead or behind, including whitespace / comment tokens, -1 if first token,
     * getOriginalText().getLength() at end
     */
    int rawTokenTypeStart(int steps);

    /**
     * Returns the index of the current token in the original sequence.
     *
     * @return token index
     */
    int rawTokenIndex();

    /**
     * Returns the text of the current token from the lexer.
     *
     * @return the token text, or null when the token stream is over.
     */
    @NonNls
    @Nullable
    String getTokenText();

    /**
     * Returns the start offset of the current token, or the file length when the token stream is over.
     *
     * @return the token offset.
     */
    int getCurrentOffset();

    /**
     * A marker defines a range in the document text which becomes a node in the AST
     * tree. The ranges defined by markers within the text range of the current marker
     * become child nodes of the node defined by the current marker.
     */
    interface Marker {
        /**
         * Creates and returns a new marker starting immediately before the start of
         * this marker and extending after its end. Can be called on a completed or
         * a currently active marker.
         *
         * @return the new marker instance.
         */
        @NotNull
        Marker precede();

        /**
         * Drops this marker. Can be called after other markers have been added and completed
         * after this marker. Does not affect lexer position or markers added after this marker.
         */
        void drop();

        /**
         * Drops this marker and all markers added after it, and reverts the lexer position to the
         * position of this marker.
         */
        void rollbackTo();

        /**
         * Completes this marker and labels it with the specified AST node type. Before calling this method,
         * all markers added after the beginning of this marker must be either dropped or completed.
         *
         * @param type the type of the node in the AST tree.
         */
        void done(@NotNull IElementType type);

        /**
         * Like {@linkplain #done(IElementType)}, but collapses all tokens between start and end markers
         * into single leaf node of given type.
         *
         * @param type the type of the node in the AST tree.
         */
        void collapse(@NotNull IElementType type);

        /**
         * Completes this marker and labels it as error element with specified message. Before calling this method,
         * all markers added after the beginning of this marker must be either dropped or completed.
         *
         * @param message for error element.
         */
        void error(String message);

        /**
         * Allows to define custom edge token binders instead of default ones. If any of parameters is null
         * then corresponding token binder won't be changed (keeping previously set or default token binder).
         * It is an error to set right token binder for not-done marker.
         *
         * @param left  new left edge token binder.
         * @param right new right edge token binder.
         */
        void setCustomEdgeTokenBinders(@Nullable WhitespacesAndCommentsBinder left, @Nullable WhitespacesAndCommentsBinder right);
    }

    /**
     * Creates a marker at the current parsing position.
     *
     * @return the new marker instance.
     */
    @NotNull
    Marker mark();

    /**
     * Adds an error marker with the specified message text at the current position in the tree.
     * <br><b>Note</b>: from series of subsequent errors messages only first will be part of resulting tree.
     *
     * @param messageText the text of the error message displayed to the user.
     */
    void error(String messageText);

    /**
     * Checks if the lexer has reached the end of file.
     *
     * @return true if the lexer is at end of file, false otherwise.
     */
    boolean eof();
}
