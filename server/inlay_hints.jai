holds_procedure_or_type :: (expression: *Node, depth := 0) -> bool {
    while expression && expression.kind == .BINARY_OPERATION && (cast(*Binary_Operation) expression).operation == .DOT {
        expression = (cast(*Binary_Operation) expression).right;
    }

    if !expression || depth > 8 return false;

    if expression.kind == {
        case .PROCEDURE;                return true;
        case .DIRECTIVE_BAKE_ARGUMENTS; return true;
        case .STRUCT;                   return true;
        case .ENUM;                     return true;
        case .UNION;                    return true;
        case .ARRAY_TYPE;               return true;
        case .DIRECTIVE_TYPE;           return true;
        case .DIRECTIVE_IMPORT;         return true;
        case .IDENTIFIER;
        case;                           return false;
    }

    ident := cast(*Identifier) expression;
    decls := resolve_identifier_decls(ident, resolve_procedure_calls=false);
    if !decls.count {
        builtin_type := array_find(BUILTIN_TYPES, ident.name);
        return builtin_type;
    }

    decl := decls[decls.count-1];
    if decl.type_inst {
        if decl.type_inst.kind == .PROCEDURE_CALL return false;
        type := get_node_type(decl.type_inst, stop_at_procedure=true);
        return type && type.kind == .PROCEDURE;
    }

    return holds_procedure_or_type(decl.expression, depth + 1);
}

handle_inlay_hint :: (request: LSP_Request_Message_Inlay_Hint) {
    push_allocator(temp);

    file_path := normalize_path(request.params.textDocument.uri);
    file := get_file(file_path);
    if !file {
        lsp_respond(request.id, null);
        return;
    }

    range := request.params.range;
    content := cast(string) get_memory_file(file_path);
    line_starts := get_line_starts(content);
    hints: [..]LSP_Inlay_Hint;

    add_hint :: (location: Node.Location, byte_column: int, label: string, kind: LSP_Inlay_Hint_Kind) #expand {
        if location.l0 < xx range.start.line || location.l0 > xx range.end.line return;

        line: string;
        if location.l0 < line_starts.count line = slice(content, line_starts[location.l0], content.count - line_starts[location.l0]);
        position := LSP_Position.{cast(u32) location.l0, cast(u32) utf16_column(line, byte_column)};
        if position.line == range.start.line && position.character < range.start.character return;
        if position.line == range.end.line && position.character > range.end.character return;
        array_add(*hints, .{position = position, label = label, kind = xx kind, paddingRight = kind == .PARAMETER});
    }

    add_type_hint :: (decl: *Declaration) #expand {
        type, _, bindings, depth := get_node_type(decl);
        name, resolved := type_name(type, depth, bindings);
        if resolved add_hint(decl.location, decl.location.c0 + decl.name.count, tprint(": %", name), .TYPE);
    }

    is_inferred_value :: (decl: *Declaration) -> bool {
        if !decl.name || decl.const || decl.type_inst || !decl.expression || decl.expression == *empty_identifier return false;
        return !holds_procedure_or_type(decl.expression);
    }

    for node: file.nodes {
        if node.location.l1 < xx range.start.line || node.location.l0 > xx range.end.line continue;

        if node.kind == .DECLARATION && is_inferred_value(cast(*Declaration) node) add_type_hint(cast(*Declaration) node);

        if node.kind == .COMPOUND_DECLARATION {
            decls: [..]*Declaration;
            compound_declaration_to_declarations(*decls, cast(*Compound_Declaration) node);
            for decls if is_inferred_value(it) add_type_hint(it);
        }

        if node.kind == .FOR {
            _for := cast(*For) node;
            if !_for.value || _for.value.kind != .IDENTIFIER continue;

            decls: [..]*Declaration;
            get_declarations_from_for(*decls, _for, null, (cast(*Identifier) _for.value).name);
            if decls.count add_type_hint(decls[0]);
        }

        if node.kind == .PROCEDURE_CALL {
            call := cast(*Procedure_Call) node;

            procedure: *Procedure;
            params: []*Declaration;
            for arg: call.arguments {
                value := arg;
                if value && value.kind == .UNARY_OPERATION && (cast(*Unary_Operation) value).operation == .MINUS value = (cast(*Unary_Operation) value).expression;
                if !value || value.kind != .LITERAL continue;
                value_type := (cast(*Literal) value).value_type;
                if value_type != .INT && value_type != .FLOAT && value_type != .BOOL continue;

                if !procedure {
                    matched: bool;
                    _, procedure, matched = get_procedure_call_decl(call);
                    if !procedure || !matched break;
                    params = get_procedure_parameters(procedure);
                }

                index := get_argument_parameter(params, arg, it_index);
                if index < 0 || is_varargs(params[index]) || !params[index].name continue;

                add_hint(arg.location, arg.location.c0, tprint("%:", params[index].name), .PARAMETER);
            }
        }
    }

    lsp_respond(request.id, hints);
}
