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

package org.snrostov.kl;

import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.kotlin.parsing.trash.SemanticWhitespaceAwarePsiBuilder;

public class SemanticWhitespaceAwarePsiBuilderAdapter implements SemanticWhitespaceAwarePsiBuilder {
    @Override
    public CharSequence getOriginalText() {
        return myBuilder.getOriginalText();
    }

    @Override
    public void advanceLexer() {
        myBuilder.advanceLexer();
    }

    @Override
    @Nullable
    public IElementType getTokenType() {
        return myBuilder.getTokenType();
    }

    @Override
    public void remapCurrentToken(IElementType type) {
        myBuilder.remapCurrentToken(type);
    }

    @Override
    @Nullable
    public IElementType lookAhead(int steps) {
        return myBuilder.lookAhead(steps);
    }

    @Override
    @Nullable
    public IElementType rawLookup(int steps) {
        return myBuilder.rawLookup(steps);
    }

    @Override
    public int rawTokenTypeStart(int steps) {
        return myBuilder.rawTokenTypeStart(steps);
    }

    @Override
    public int rawTokenIndex() {
        return myBuilder.rawTokenIndex();
    }

    @Override
    @Nullable
    @NonNls
    public String getTokenText() {
        return myBuilder.getTokenText();
    }

    @Override
    public int getCurrentOffset() {
        return myBuilder.getCurrentOffset();
    }

    @Override
    @NotNull
    public Marker mark() {
        return myBuilder.mark();
    }

    @Override
    public void error(String messageText) {
        myBuilder.error(messageText);
    }

    @Override
    public boolean eof() {
        return myBuilder.eof();
    }

    private final SemanticWhitespaceAwarePsiBuilder myBuilder;

    public SemanticWhitespaceAwarePsiBuilderAdapter(SemanticWhitespaceAwarePsiBuilder builder) {
        this.myBuilder = builder;
    }

    @Override
    public boolean newlineBeforeCurrentToken() {
        return myBuilder.newlineBeforeCurrentToken();
    }

    @Override
    public void disableNewlines() {
        myBuilder.disableNewlines();
    }

    @Override
    public void enableNewlines() {
        myBuilder.enableNewlines();
    }

    @Override
    public void restoreNewlinesState() {
        myBuilder.restoreNewlinesState();
    }

    @Override
    public void restoreJoiningComplexTokensState() {
        myBuilder.restoreJoiningComplexTokensState();
    }

    @Override
    public void enableJoiningComplexTokens() {
        myBuilder.enableJoiningComplexTokens();
    }

    @Override
    public void disableJoiningComplexTokens() {
        myBuilder.disableJoiningComplexTokens();
    }

    @Override
    public boolean isWhitespaceOrComment(@NotNull IElementType elementType) {
        return myBuilder.isWhitespaceOrComment(elementType);
    }


}
