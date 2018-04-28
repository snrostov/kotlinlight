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

package org.jetbrains.kotlin;

import com.intellij.psi.tree.IElementType;

public interface KtNodeTypes {
    IElementType KT_FILE = new IElementType("KT_FILE");

    IElementType CLASS = new IElementType("CLASS");
    IElementType FUN = new IElementType("FUNCTION");
    IElementType PROPERTY = new IElementType("PROPERTY");
    IElementType DESTRUCTURING_DECLARATION = new KtNodeType("DESTRUCTURING_DECLARATION");
    IElementType DESTRUCTURING_DECLARATION_ENTRY = new KtNodeType("DESTRUCTURING_DECLARATION_ENTRY");

    IElementType OBJECT_DECLARATION = new IElementType("OBJECT_DECLARATION");
    IElementType TYPEALIAS = new IElementType("TYPEALIAS");

    IElementType ENUM_ENTRY = new IElementType("ENUM_ENTRY");
    IElementType CLASS_INITIALIZER = new IElementType("CLASS_INITIALIZER");
    IElementType SCRIPT_INITIALIZER = new KtNodeType("SCRIPT_INITIALIZER");
    IElementType SECONDARY_CONSTRUCTOR = new IElementType("SECONDARY_CONSTRUCTOR");
    IElementType PRIMARY_CONSTRUCTOR = new IElementType("PRIMARY_CONSTRUCTOR");

    IElementType TYPE_PARAMETER_LIST = new IElementType("TYPE_PARAMETER_LIST");
    IElementType TYPE_PARAMETER = new IElementType("TYPE_PARAMETER");
    IElementType SUPER_TYPE_LIST = new IElementType("SUPER_TYPE_LIST");
    IElementType DELEGATED_SUPER_TYPE_ENTRY = new IElementType("DELEGATED_SUPER_TYPE_ENTRY");
    IElementType SUPER_TYPE_CALL_ENTRY = new IElementType("SUPER_TYPE_CALL_ENTRY");
    IElementType SUPER_TYPE_ENTRY = new IElementType("SUPER_TYPE_ENTRY");
    KtNodeType PROPERTY_DELEGATE = new KtNodeType("PROPERTY_DELEGATE");
    IElementType CONSTRUCTOR_CALLEE = new IElementType("CONSTRUCTOR_CALLEE");
    IElementType VALUE_PARAMETER_LIST = new IElementType("VALUE_PARAMETER_LIST");
    IElementType VALUE_PARAMETER = new IElementType("VALUE_PARAMETER");

    IElementType CLASS_BODY = new IElementType("CLASS_BODY");
    IElementType IMPORT_LIST = new IElementType("IMPORT_LIST");
    IElementType FILE_ANNOTATION_LIST = new IElementType("FILE_ANNOTATION_LIST");
    IElementType IMPORT_DIRECTIVE = new IElementType("IMPORT_DIRECTIVE");
    IElementType IMPORT_ALIAS = new IElementType("IMPORT_ALIAS");
    IElementType MODIFIER_LIST = new IElementType("MODIFIER_LIST");
    IElementType ANNOTATION = new IElementType("ANNOTATION");
    IElementType ANNOTATION_ENTRY = new IElementType("ANNOTATION_ENTRY");
    IElementType ANNOTATION_TARGET = new IElementType("ANNOTATION_TARGET");

    IElementType TYPE_ARGUMENT_LIST = new IElementType("TYPE_ARGUMENT_LIST");
    KtNodeType VALUE_ARGUMENT_LIST = new KtNodeType("VALUE_ARGUMENT_LIST");
    KtNodeType VALUE_ARGUMENT = new KtNodeType("VALUE_ARGUMENT");
    KtNodeType LAMBDA_ARGUMENT = new KtNodeType("LAMBDA_ARGUMENT");
    KtNodeType VALUE_ARGUMENT_NAME = new KtNodeType("VALUE_ARGUMENT_NAME");
    IElementType TYPE_REFERENCE = new IElementType("TYPE_REFERENCE");

    IElementType USER_TYPE = new IElementType("USER_TYPE");
    IElementType DYNAMIC_TYPE = new IElementType("DYNAMIC_TYPE");
    IElementType FUNCTION_TYPE = new IElementType("FUNCTION_TYPE");
    IElementType FUNCTION_TYPE_RECEIVER = new IElementType("FUNCTION_TYPE_RECEIVER");
    KtNodeType SELF_TYPE = new KtNodeType("SELF_TYPE");
    IElementType NULLABLE_TYPE = new IElementType("NULLABLE_TYPE");
    IElementType TYPE_PROJECTION = new IElementType("TYPE_PROJECTION");

    // TODO: review
    IElementType PROPERTY_ACCESSOR = new IElementType("PROPERTY_ACCESSOR");
    IElementType INITIALIZER_LIST = new IElementType("INITIALIZER_LIST");
    IElementType TYPE_CONSTRAINT_LIST = new IElementType("TYPE_CONSTRAINT_LIST");
    IElementType TYPE_CONSTRAINT = new IElementType("TYPE_CONSTRAINT");

    IElementType CONSTRUCTOR_DELEGATION_CALL = new KtNodeType.KtLeftBoundNodeType("CONSTRUCTOR_DELEGATION_CALL");
    KtNodeType CONSTRUCTOR_DELEGATION_REFERENCE = new KtNodeType.KtLeftBoundNodeType("CONSTRUCTOR_DELEGATION_REFERENCE");

    // TODO: Not sure if we need separate NT for each kind of constants
    KtNodeType NULL = new KtNodeType("NULL");
    KtNodeType BOOLEAN_CONSTANT = new KtNodeType("BOOLEAN_CONSTANT");
    KtNodeType FLOAT_CONSTANT = new KtNodeType("FLOAT_CONSTANT");
    KtNodeType CHARACTER_CONSTANT = new KtNodeType("CHARACTER_CONSTANT");
    KtNodeType INTEGER_CONSTANT = new KtNodeType("INTEGER_CONSTANT");

    KtNodeType STRING_TEMPLATE = new KtNodeType("STRING_TEMPLATE");
    KtNodeType LONG_STRING_TEMPLATE_ENTRY = new KtNodeType("LONG_STRING_TEMPLATE_ENTRY");
    KtNodeType SHORT_STRING_TEMPLATE_ENTRY = new KtNodeType("SHORT_STRING_TEMPLATE_ENTRY");
    KtNodeType LITERAL_STRING_TEMPLATE_ENTRY = new KtNodeType("LITERAL_STRING_TEMPLATE_ENTRY");
    KtNodeType ESCAPE_STRING_TEMPLATE_ENTRY = new KtNodeType("ESCAPE_STRING_TEMPLATE_ENTRY");

    KtNodeType PARENTHESIZED = new KtNodeType("PARENTHESIZED");
    KtNodeType RETURN = new KtNodeType("RETURN");
    KtNodeType THROW = new KtNodeType("THROW");
    KtNodeType CONTINUE = new KtNodeType("CONTINUE");
    KtNodeType BREAK = new KtNodeType("BREAK");
    KtNodeType IF = new KtNodeType("IF");
    KtNodeType CONDITION = new KtNodeType("CONDITION");
    KtNodeType THEN = new KtNodeType("THEN");
    KtNodeType ELSE = new KtNodeType("ELSE");
    KtNodeType TRY = new KtNodeType("TRY");
    KtNodeType CATCH = new KtNodeType("CATCH");
    KtNodeType FINALLY = new KtNodeType("FINALLY");
    KtNodeType FOR = new KtNodeType("FOR");
    KtNodeType WHILE = new KtNodeType("WHILE");
    KtNodeType DO_WHILE = new KtNodeType("DO_WHILE");
    KtNodeType LOOP_RANGE = new KtNodeType("LOOP_RANGE");
    KtNodeType BODY = new KtNodeType("BODY");
    KtNodeType BLOCK = new KtNodeType("BLOCK");

    IElementType LAMBDA_EXPRESSION = new KtNodeType("LAMBDA_EXPRESSION");

    KtNodeType FUNCTION_LITERAL = new KtNodeType("FUNCTION_LITERAL");
    KtNodeType ANNOTATED_EXPRESSION = new KtNodeType("ANNOTATED_EXPRESSION");

    IElementType REFERENCE_EXPRESSION = new IElementType("REFERENCE_EXPRESSION");
    IElementType ENUM_ENTRY_SUPERCLASS_REFERENCE_EXPRESSION = new IElementType("ENUM_ENTRY_SUPERCLASS_REFERENCE_EXPRESSION");
    KtNodeType OPERATION_REFERENCE = new KtNodeType("OPERATION_REFERENCE");
    KtNodeType LABEL = new KtNodeType("LABEL");

    KtNodeType LABEL_QUALIFIER = new KtNodeType("LABEL_QUALIFIER");

    KtNodeType THIS_EXPRESSION = new KtNodeType("THIS_EXPRESSION");
    KtNodeType SUPER_EXPRESSION = new KtNodeType("SUPER_EXPRESSION");
    KtNodeType BINARY_EXPRESSION = new KtNodeType("BINARY_EXPRESSION");
    KtNodeType BINARY_WITH_TYPE = new KtNodeType("BINARY_WITH_TYPE");
    KtNodeType IS_EXPRESSION = new KtNodeType("IS_EXPRESSION"); // TODO:
    KtNodeType PREFIX_EXPRESSION = new KtNodeType("PREFIX_EXPRESSION");
    KtNodeType POSTFIX_EXPRESSION = new KtNodeType("POSTFIX_EXPRESSION");
    KtNodeType LABELED_EXPRESSION = new KtNodeType("LABELED_EXPRESSION");
    KtNodeType CALL_EXPRESSION = new KtNodeType("CALL_EXPRESSION");
    KtNodeType ARRAY_ACCESS_EXPRESSION = new KtNodeType("ARRAY_ACCESS_EXPRESSION");
    KtNodeType INDICES = new KtNodeType("INDICES");
    IElementType DOT_QUALIFIED_EXPRESSION = new IElementType("DOT_QUALIFIED_EXPRESSION");
    KtNodeType CALLABLE_REFERENCE_EXPRESSION = new KtNodeType("CALLABLE_REFERENCE_EXPRESSION");
    KtNodeType CLASS_LITERAL_EXPRESSION = new KtNodeType("CLASS_LITERAL_EXPRESSION");
    KtNodeType SAFE_ACCESS_EXPRESSION = new KtNodeType("SAFE_ACCESS_EXPRESSION");

    KtNodeType OBJECT_LITERAL = new KtNodeType("OBJECT_LITERAL");

    KtNodeType WHEN = new KtNodeType("WHEN");
    KtNodeType WHEN_ENTRY = new KtNodeType("WHEN_ENTRY");

    KtNodeType WHEN_CONDITION_IN_RANGE = new KtNodeType("WHEN_CONDITION_IN_RANGE");
    KtNodeType WHEN_CONDITION_IS_PATTERN = new KtNodeType("WHEN_CONDITION_IS_PATTERN");
    KtNodeType WHEN_CONDITION_EXPRESSION = new KtNodeType("WHEN_CONDITION_WITH_EXPRESSION");

    KtNodeType COLLECTION_LITERAL_EXPRESSION = new KtNodeType("COLLECTION_LITERAL_EXPRESSION");

    IElementType PACKAGE_DIRECTIVE = new IElementType("PACKAGE_DIRECTIVE");

    IElementType SCRIPT = new IElementType("SCRIPT");

    IElementType TYPE_CODE_FRAGMENT = new IElementType("TYPE_CODE_FRAGMENT");
    IElementType EXPRESSION_CODE_FRAGMENT = new IElementType("EXPRESSION_CODE_FRAGMENT");
    IElementType BLOCK_CODE_FRAGMENT = new IElementType("BLOCK_CODE_FRAGMENT");
}
