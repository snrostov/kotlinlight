/*
 * Copyright 2000-2013 JetBrains s.r.o.
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

public class WhitespacesBinders {
    private WhitespacesBinders() {
    }

    public static final WhitespacesAndCommentsBinder DEFAULT_RIGHT_BINDER = (tokens, atStreamEdge, getter) -> 0;
    public static final WhitespacesAndCommentsBinder DEFAULT_LEFT_BINDER = (tokens, atStreamEdge, getter) -> tokens.size();

    public static final WhitespacesAndCommentsBinder GREEDY_LEFT_BINDER = DEFAULT_RIGHT_BINDER;
    public static final WhitespacesAndCommentsBinder GREEDY_RIGHT_BINDER = DEFAULT_LEFT_BINDER;
}
