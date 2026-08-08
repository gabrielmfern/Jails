handle_hover :: (request: LSP_Request_Message_Hover) {
    push_allocator(temp);

    file_path := normalize_path(request.params.textDocument.uri);

    file := get_file(file_path);
    if !file {
        lsp_respond(request.id, null);
        return;
    }

    cursor_location := lsp_location_to_node_location(request.params.position, file_path);
    cursor_node := get_node_by_location(file, cursor_location);

    decl: *Declaration;

    if cursor_node && cursor_node.kind == .DECLARATION {
        decl = cast(*Declaration) cursor_node;
    } else if cursor_node && cursor_node.kind == .IDENTIFIER {
        decls := resolve_identifier_decls(cast(*Identifier) cursor_node);
        if decls.count > 0 decl = decls[0];
    }

    if !decl {
        lsp_respond(request.id, null);
        return;
    }

    text := to_string(decl);
    if ends_with(text, " :: ") text.count -= 4;
    if decl.expression {
        if decl.expression.kind == {
            case .STRUCT; text = tprint("% :: struct", decl.name);
            case .ENUM;   text = tprint("% :: enum", decl.name);
            case .UNION;  text = tprint("% :: union", decl.name);
        }
    }

    hover: LSP_Hover;
    hover.contents.value = tprint("```jai\n%\n```", text);
    lsp_respond(request.id, hover);
}
