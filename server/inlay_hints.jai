inlay_type_name :: (type: *Node, depth: int, bindings: []Poly_Binding) -> string {
    if !type return "";

    name: string;
    if type.kind == {
        case .IDENTIFIER;
            name = (cast(*Identifier) type).name;
            if !array_find(BUILTIN_TYPES, name) return "";

        case .LITERAL;
            literal := cast(*Literal) type;
            if literal.value_type == {
                case .INT;    name = "int";
                case .FLOAT;  name = "float";
                case .STRING; name = "string";
                case .BOOL;   name = "bool";
                case;         return "";
            }

        case .STRUCT; #through;
        case .UNION;  #through;
        case .ENUM;
            if !type.parent || type.parent.kind != .DECLARATION return "";
            name = (cast(*Declaration) type.parent).name;

        case .ARRAY_TYPE;
            array_type := cast(*Array_Type) type;
            element, _, element_bindings, element_depth := get_node_type(array_type.element_type, root_bindings = bindings);
            element_name := inlay_type_name(element, element_depth, element_bindings);
            if !element_name return "";

            if array_type.resizable {
                name = tprint("[..]%", element_name);
            } else if !array_type.dimension {
                name = tprint("[]%", element_name);
            } else {
                dimension := array_type.dimension;
                if dimension.kind != .LITERAL dimension = get_node_type(dimension, root_bindings = bindings);
                if !dimension || dimension.kind != .LITERAL || (cast(*Literal) dimension).value_type != .INT return "";
                name = tprint("[%]%", (cast(*Literal) dimension)._int, element_name);
            }

        case;
            return "";
    }

    builder: String_Builder;
    for 1..depth append(*builder, "*");
    append(*builder, name);
    return builder_to_string(*builder);
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

    add_hint :: (location: Node.Location, byte_column: int, label: string, kind: int) #expand {
        line := get_line(content, line_starts, location.l0);
        position := LSP_Position.{cast(u32) location.l0, cast(u32) utf16_column(line, byte_column)};
        if position.line == range.start.line && position.character < range.start.character return;
        if position.line == range.end.line && position.character > range.end.character return;
        array_add(*hints, .{position = position, label = label, kind = kind, paddingRight = kind == 2});
    }

    add_type_hint :: (decl: *Declaration) #expand {
        type, _, bindings, depth := get_node_type(decl);
        name := inlay_type_name(type, depth, bindings);
        if name add_hint(decl.location, decl.location.c0 + decl.name.count, tprint(": %", name), 1);
    }

    is_inferred_value :: (decl: *Declaration) -> bool {
        if !decl.name || decl.type_inst || !decl.expression return false;

        token_type := classify_declaration(decl);
        if token_type != .VARIABLE && token_type != .PROPERTY && token_type != .PARAMETER return false;

        if decl.expression.kind == .IDENTIFIER {
            ok, expression_type := classify_identifier(cast(*Identifier) decl.expression);
            if ok && expression_type != .VARIABLE && expression_type != .PROPERTY && expression_type != .PARAMETER return false;
        }

        return true;
    }

    for node: file.nodes {
        if node.location.l0 < xx range.start.line || node.location.l0 > xx range.end.line continue;

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
                    _, procedure = get_procedure_call_decl(call);
                    if !procedure break;
                    params = get_procedure_parameters(procedure);
                }

                index := get_argument_parameter(params, arg, it_index);
                if index < 0 || is_varargs(params[index]) || !params[index].name continue;

                add_hint(arg.location, arg.location.c0, tprint("%:", params[index].name), 2);
            }
        }
    }

    lsp_respond(request.id, hints);
}
