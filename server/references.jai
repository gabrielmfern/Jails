// Resolve whatever the cursor is sitting on to its target declaration(s).
// Returns the name and decls, or ok=false if the cursor isn't on an identifier
// or a declaration's name.
cursor_target :: (file: *Program_File, cursor_location: Node.Location) -> name: string, decls: []*Declaration, ok: bool {
    cursor_node := get_node_by_location(file, cursor_location);
    if !cursor_node return "", .[], false;

    if cursor_node.kind == .IDENTIFIER {
        ident := cast(*Identifier) cursor_node;
        return ident.name, resolve_identifier_decls(ident, resolve_procedure_calls=false), true;
    }

    if cursor_node.kind == .DECLARATION {
        decl := cast(*Declaration) cursor_node;
        single := NewArray(1, *Declaration);
        single[0] = decl;
        return decl.name, single, true;
    }

    return "", .[], false;
}

decls_intersect :: (a: []*Declaration, b: []*Declaration) -> bool {
    for x: a for y: b if same_decl(x, y) return true;
    return false;
}

handle_references :: (request: LSP_Request_Message_References) {
    push_allocator(temp);

    file_path := normalize_path(request.params.textDocument.uri);
    file := get_file(file_path);
    if !file {
        lsp_respond(request.id, null);
        return;
    }

    cursor_location := lsp_location_to_node_location(request.params.position, file_path);
    target_name, target_decls, ok := cursor_target(file, cursor_location);
    if !ok || target_decls.count == 0 {
        lsp_respond(request.id, null);
        return;
    }

    locations: [..]LSP_Location;

    if request.params._context.includeDeclaration {
        for decl: target_decls {
            array_add(*locations, node_location_to_lsp_location(decl.location));
        }
    }

    for other_file: server.files {
        for node: other_file.nodes {
            if node.kind != .IDENTIFIER continue;

            other_ident := cast(*Identifier) node;
            if other_ident.name != target_name continue;

            other_decls := resolve_identifier_decls(other_ident, resolve_procedure_calls=false);
            if decls_intersect(target_decls, other_decls) {
                array_add(*locations, node_location_to_lsp_location(other_ident.location));
            }
        }
    }

    lsp_respond(request.id, locations);
}

handle_implementation :: (request: LSP_Request_Message_Implementation) {
    push_allocator(temp);

    file_path := normalize_path(request.params.textDocument.uri);
    file := get_file(file_path);
    if !file {
        lsp_respond(request.id, null);
        return;
    }

    cursor_location := lsp_location_to_node_location(request.params.position, file_path);
    _, decls, ok := cursor_target(file, cursor_location);
    if !ok || decls.count == 0 {
        lsp_respond(request.id, null);
        return;
    }

    locations := NewArray(decls.count, LSP_Location);

    for decls locations[it_index] = node_location_to_lsp_location(it.location);

    lsp_respond(request.id, locations);
}
