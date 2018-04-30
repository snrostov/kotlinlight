/*
 * Copyright 2000-2017 JetBrains s.r.o.
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

import com.intellij.lexer.Lexer;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

class TokenSequence {
    final int[] lexStarts;
    final IElementType[] lexTypes;
    final int lexemeCount;

    TokenSequence(int[] lexStarts, IElementType[] lexTypes, int lexemeCount) {
        this.lexStarts = lexStarts;
        this.lexTypes = lexTypes;
        this.lexemeCount = lexemeCount;
        assert lexemeCount < lexStarts.length;
        assert lexemeCount < lexTypes.length;
    }

    void assertMatches(@NotNull CharSequence text, @NotNull Lexer lexer) {
        TokenSequence sequence = new Builder(text, lexer).performLexing();
        assert lexemeCount == sequence.lexemeCount;
        for (int j = 0; j <= lexemeCount; ++j) {
            if (sequence.lexStarts[j] != lexStarts[j] || sequence.lexTypes[j] != lexTypes[j]) {
                assert false;
            }
        }
    }

    static class Builder {
        private final CharSequence myText;
        private final Lexer myLexer;
        private int[] myLexStarts;
        private IElementType[] myLexTypes;

        Builder(@NotNull CharSequence text, @NotNull Lexer lexer) {
            myText = text;
            myLexer = lexer;

            int approxLexCount = Math.max(10, myText.length() / 5);

            myLexStarts = new int[approxLexCount];
            myLexTypes = new IElementType[approxLexCount];
        }

        @NotNull
        TokenSequence performLexing() {
            myLexer.start(myText);
            int i = 0;
            int offset = 0;
            while (true) {
                IElementType type = myLexer.getTokenType();
                if (type == null) break;

                int tokenStart = myLexer.getTokenStart();
                if (tokenStart < offset) {
                    reportDescendingOffsets(i, offset, tokenStart);
                }

//                if (!PsiBuilderImplKt.getMyWhitespacesOrComments().contains(type)) {
                    if (i >= myLexTypes.length - 1) {
                        resizeLexemes(i * 3 / 2);
                    }
                    myLexStarts[i] = offset = tokenStart;
                    myLexTypes[i] = type;
                    i++;
//                }

                myLexer.advance();
            }

            myLexStarts[i] = myText.length();

            return new TokenSequence(myLexStarts, myLexTypes, i);
        }

        private void reportDescendingOffsets(int tokenIndex, int offset, int tokenStart) {
            StringBuilder sb = new StringBuilder();
            IElementType tokenType = myLexer.getTokenType();
            sb.append("Token sequence broken")
                    .append("\n  this: '").append(myLexer.getTokenText()).append("' (").append(tokenType).append(':')
                    .append(tokenType).append(") ").append(tokenStart).append(":")
                    .append(myLexer.getTokenEnd());
            if (tokenIndex > 0) {
                int prevStart = myLexStarts[tokenIndex - 1];
                sb.append("\n  prev: '").append(myText.subSequence(prevStart, offset)).append("' (").append(myLexTypes[tokenIndex - 1]).append(':')
                        .append(myLexTypes[tokenIndex - 1]).append(") ").append(prevStart).append(":").append(offset);
            }
            int quoteStart = Math.max(tokenStart - 256, 0);
            int quoteEnd = Math.min(tokenStart + 256, myText.length());
            sb.append("\n  quote: [").append(quoteStart).append(':').append(quoteEnd)
                    .append("] '").append(myText.subSequence(quoteStart, quoteEnd)).append('\'');
        }

        private void resizeLexemes(final int newSize) {
            myLexStarts = realloc(myLexStarts, newSize);
            myLexTypes = realloc(myLexTypes, newSize);
        }


        public static final int[] EMPTY_INT_ARRAY = new int[0];

        @NotNull
        @Contract(pure = true)
        public static int[] realloc(@NotNull int[] array, final int newSize) {
            if (newSize == 0) {
                return EMPTY_INT_ARRAY;
            }

            final int oldSize = array.length;
            if (oldSize == newSize) {
                return array;
            }

            final int[] result = new int[newSize];
            System.arraycopy(array, 0, result, 0, Math.min(oldSize, newSize));
            return result;
        }

        @NotNull
        @Contract(pure = true)
        public static IElementType[] realloc(@NotNull IElementType[] array, final int newSize) {
            final int oldSize = array.length;
            if (oldSize == newSize) {
                return array;
            }

            IElementType[] result = new IElementType[newSize];
            if (newSize == 0) {
                return result;
            }

            System.arraycopy(array, 0, result, 0, Math.min(oldSize, newSize));
            return result;
        }
    }
}
