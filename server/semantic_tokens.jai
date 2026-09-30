Semantic_Token_Type :: enum u32 {
    NAMESPACE;
    TYPE;
    STRUCT;
    ENUM;
    TYPE_PARAMETER;
    PARAMETER;
    VARIABLE;
    PROPERTY;
    ENUM_MEMBER;
    FUNCTION;
}

Semantic_Token_Modifiers :: enum_flags u32 {
    DECLARATION;
    READONLY;
    STATIC;
    DEFAULT_LIBRARY;
}

Semantic_Token :: struct {
    line: int;
    column: int;
    length: int;
    type: Semantic_Token_Type;
    modifiers: Semantic_Token_Modifiers;
}

classify_polymorphic_constant :: (node: *Node) -> Semantic_Token_Type {
    expression := (cast(*Polymorphic_Constant) node).expression;
    if !expression || expression.kind != .DECLARATION return .TYPE_PARAMETER;

    type_inst := (cast(*Declaration) expression).type_inst;
    if type_inst && type_inst.kind == .IDENTIFIER && (cast(*Identifier) type_inst).name == "Type" return .TYPE_PARAMETER;
    return .PARAMETER;
}

classify_declaration :: (decl: *Declaration, depth := 0) -> Semantic_Token_Type, Semantic_Token_Modifiers {
    library: Semantic_Token_Modifiers;
    if decl.location.file == BUILTINS_PATH || begins_with(decl.location.file, server.jai_path_prefix) {
        library = .DEFAULT_LIBRARY;
    }

    if decl.expression == *empty_identifier return .ENUM_MEMBER, library;

    expression := decl.expression;
    if decl.const && expression {
        if expression.kind == {
            case .PROCEDURE;                return .FUNCTION, library;
            case .DIRECTIVE_BAKE_ARGUMENTS; return .FUNCTION, library;
            case .STRUCT;                   return .STRUCT, library;
            case .ENUM;                     return .ENUM, library;
            case .UNION;                    return .TYPE, library;
            case .ARRAY_TYPE;               return .TYPE, library;
            case .DIRECTIVE_TYPE;           return .TYPE, library;
            case .DIRECTIVE_IMPORT;         return .NAMESPACE, library;
        }

        is_call := false;
        while expression {
            if expression.kind == .BINARY_OPERATION && (cast(*Binary_Operation) expression).operation == .DOT {
                expression = (cast(*Binary_Operation) expression).right;
            } else if expression.kind == .PROCEDURE_CALL {
                expression = (cast(*Procedure_Call) expression).procedure;
                is_call = true;
            } else {
                break;
            }
        }

        if expression && expression.kind == .IDENTIFIER && depth < 8 {
            ok, type := classify_identifier(cast(*Identifier) expression, depth + 1);
            if ok && type == .STRUCT && is_call return .TYPE, library;
            if ok && type == .FUNCTION && !is_call return .FUNCTION, library;
            if ok && (type == .TYPE || type == .STRUCT || type == .ENUM || type == .NAMESPACE || type == .TYPE_PARAMETER) {
                return type, library;
            }
        }
    }

    if decl.type_inst && decl.type_inst.kind == .PROCEDURE return .FUNCTION, library;

    modifiers: Semantic_Token_Modifiers;
    if decl.const modifiers = .READONLY;

    if !decl.parent return .VARIABLE, modifiers;

    parent := decl.parent;
    if parent.kind == .USING parent = parent.parent;

    if parent && parent.kind == .POLYMORPHIC_CONSTANT return classify_polymorphic_constant(parent), modifiers;

    if parent && (parent.kind == .PROCEDURE || parent.kind == .STRUCT || parent.kind == .UNION) {
        return .PARAMETER, modifiers;
    }

    while parent {
        if parent.kind != .BLOCK && parent.kind != .IF && parent.kind != .CASE && parent.kind != .USING && parent.kind != .DIRECTIVE_SCOPE &&
           parent.kind != .COMPOUND_DECLARATION && parent.kind != .COMPOUND_DECLARATION_ITEM {
            break;
        }

        parent = parent.parent;
    }

    if !parent return .VARIABLE, modifiers | .STATIC | library;
    if parent.kind == .STRUCT || parent.kind == .UNION || parent.kind == .DIRECTIVE_ADD_CONTEXT return .PROPERTY, modifiers | library;
    if parent.kind == .ENUM return .ENUM_MEMBER, modifiers | library;

    return .VARIABLE, modifiers;
}

classify_identifier :: (ident: *Identifier, depth := 0) -> ok: bool, Semantic_Token_Type, Semantic_Token_Modifiers {
    parent := ident.parent;
    if parent && parent.kind == .POLYMORPHIC_CONSTANT return true, classify_polymorphic_constant(parent), .DECLARATION;

    if parent && parent.kind == .BINARY_OPERATION && parent.parent && parent.parent.kind == .PROCEDURE_CALL {
        binary_op := cast(*Binary_Operation) parent;
        if binary_op.operation == .ASSIGN && binary_op.left == ident return true, .PARAMETER, 0;
    }

    decls := resolve_identifier_decls(ident, resolve_procedure_calls=false);
    if decls.count > 0 {
        decl := decls[decls.count-1];
        type, modifiers := classify_declaration(decl, depth);
        if decl.location.l0 == ident.location.l0 && decl.location.c0 == ident.location.c0 modifiers |= .DECLARATION;
        return true, type, modifiers;
    }

    if parent && parent.kind == .UNARY_OPERATION && (cast(*Unary_Operation) parent).operation == .DOT return true, .ENUM_MEMBER, 0;

    if ident.name == "context" return true, .VARIABLE, .DEFAULT_LIBRARY;

    for BUILTIN_TYPES if it == ident.name return true, .TYPE, .DEFAULT_LIBRARY;

    while parent {
        polymorphic_constant: *Node;
        if parent.kind == .PROCEDURE polymorphic_constant = find_polymorphic_constant(parent, ident.name);
        if parent.kind == .STRUCT {
            for (cast(*Struct) parent).polymorphic_arguments {
                polymorphic_constant = find_polymorphic_constant(it, ident.name);
                if polymorphic_constant break;
            }
        }

        if polymorphic_constant return true, classify_polymorphic_constant(polymorphic_constant), 0;
        parent = parent.parent;
    }

    return false, 0, 0;
}

handle_semantic_tokens :: (request: LSP_Request_Message_Semantic_Tokens) {
    push_allocator(temp);

    file_path := normalize_path(request.params.textDocument.uri);
    file := get_file(file_path);
    if !file {
        lsp_respond(request.id, null);
        return;
    }

    range := request.params.range;
    tokens: [..]Semantic_Token;

    for node: file.nodes {
        if range {
            l0, c0 := node.location.l0, node.location.c0;
            if l0 < range.start.line || (l0 == range.start.line && c0 < range.start.character) continue;
            if l0 > range.end.line || (l0 == range.end.line && c0 >= range.end.character) continue;
        }

        if node.kind == .IDENTIFIER {
            ident := cast(*Identifier) node;
            if !ident.name continue;

            ok, type, modifiers := classify_identifier(ident);
            if ok array_add(*tokens, .{ident.location.l0, ident.location.c0, ident.name.count, type, modifiers});
        }

        if node.kind == .DECLARATION {
            decl := cast(*Declaration) node;
            if !decl.name continue;

            type, modifiers := classify_declaration(decl);
            array_add(*tokens, .{decl.location.l0, decl.location.c0, decl.name.count, type, modifiers | .DECLARATION});
        }
    }

    quick_sort(tokens, (a: Semantic_Token, b: Semantic_Token) -> int {
        if a.line != b.line return a.line - b.line;
        return a.column - b.column;
    });

    content := cast(string) get_memory_file(file_path);
    line_starts := get_line_starts(content);

    data: [..]u32;
    previous: Semantic_Token;
    previous_column: int;
    for tokens {
        if it_index > 0 && it.line == previous.line && it.column < previous.column + previous.length continue;

        line: string;
        if it.line < line_starts.count line = slice(content, line_starts[it.line], content.count - line_starts[it.line]);
        column := utf16_column(line, it.column);
        length := utf16_column(line, it.column + it.length) - column;

        delta_column := ifx it.line == previous.line then column - previous_column else column;
        array_add(*data, cast(u32) (it.line - previous.line), cast(u32) delta_column, cast(u32) length, cast(u32) it.type, cast(u32) it.modifiers);
        previous = it;
        previous_column = column;
    }

    result: struct {
        data: []u32;
    };
    result.data = data;
    lsp_respond(request.id, result);
}
