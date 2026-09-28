Rename_Edit :: struct {
    file: string;
    line: int;
    char: int;
}

same_decl :: (a: *Declaration, b: *Declaration) -> bool {
    if a == b return true;
    return a.name == b.name && a.location.file == b.location.file && a.location.l0 == b.location.l0 && a.location.c0 == b.location.c0;
}

find_identifier_at :: (file: *Program_File, location: Node.Location) -> *Identifier {
    for node: file.nodes {
        if node.kind != .IDENTIFIER continue;
        if node.location.l0 == location.l0 && node.location.c0 == location.c0 return xx node;
    }

    return null;
}

rename_targets :: (file: *Program_File, cursor_node: *Node) -> old_name: string, targets: []*Declaration {
    if cursor_node.kind == .IDENTIFIER {
        ident := cast(*Identifier) cursor_node;
        return ident.name, resolve_identifier_decls(ident, resolve_procedure_calls=false);
    }

    if cursor_node.kind != .DECLARATION {
        return "", .[];
    }

    decl := cast(*Declaration) cursor_node;

    // Renaming an overloaded procedure has to rename the whole overload set,
    // otherwise the calls end up pointing at a partially renamed set.
    if decl.expression && decl.expression.kind == .PROCEDURE {
        overloads: [..]*Declaration;

        for get_declarations(file, decl, *decl.location, decl.name) {
            if !it.expression || it.expression.kind != .PROCEDURE continue;

            already_added := false;
            for overload: overloads {
                if same_decl(overload, it) {
                    already_added = true;
                    break;
                }
            }

            if !already_added array_add(*overloads, it);
        }

        found_self := false;
        for overloads {
            if same_decl(it, decl) {
                found_self = true;
                break;
            }
        }

        if !found_self array_add(*overloads, decl);

        return decl.name, overloads;
    }

    single := NewArray(1, *Declaration);
    single[0] = decl;
    return decl.name, single;
}

renameable :: (targets: []*Declaration, old_name: string) -> bool {
    for target: targets {
        // Builtin "virtual" declarations (array count, ...) have no real source location.
        if target.location.file.count == 0 || target.location.file == BUILTINS_PATH return false;

        target_file := get_file(target.location.file);
        if !target_file continue;

        // Implicit declarations (`it`, `it_index`) point at a foreign identifier
        // and cannot be renamed at all.
        ident_at := find_identifier_at(target_file, target.location);
        if ident_at && ident_at.name != old_name return false;
    }

    return true;
}

handle_prepare_rename :: (request: LSP_Request_Message_Prepare_Rename) {
    push_allocator(temp);

    file_path := normalize_path(request.params.textDocument.uri);
    file := get_file(file_path);
    if !file {
        lsp_respond(request.id, null);
        return;
    }

    cursor_location := lsp_location_to_node_location(request.params.position, file_path);
    cursor_node := get_node_by_location(file, cursor_location);
    if !cursor_node {
        lsp_respond(request.id, null);
        return;
    }

    old_name, targets := rename_targets(file, cursor_node);
    if targets.count == 0 || old_name.count == 0 || !renameable(targets, old_name) {
        lsp_respond(request.id, null);
        return;
    }

    Prepare_Rename_Result :: struct {
        range: LSP_Range;
        placeholder: string;
    }

    result: Prepare_Rename_Result;
    result.range.start = .{ line=xx cursor_node.location.l0, character=xx cursor_node.location.c0 };
    result.range.end = .{ line=xx cursor_node.location.l0, character=xx (cursor_node.location.c0 + old_name.count) };
    result.placeholder = old_name;

    lsp_respond(request.id, result);
}

handle_rename :: (request: LSP_Request_Message_Rename) {
    push_allocator(temp);

    new_name := request.params.newName;

    file_path := normalize_path(request.params.textDocument.uri);
    file := get_file(file_path);
    if !file || new_name.count == 0 {
        lsp_respond(request.id, null);
        return;
    }

    cursor_location := lsp_location_to_node_location(request.params.position, file_path);
    cursor_node := get_node_by_location(file, cursor_location);
    if !cursor_node {
        lsp_respond(request.id, null);
        return;
    }

    old_name, targets := rename_targets(file, cursor_node);
    if targets.count == 0 || old_name.count == 0 || !renameable(targets, old_name) {
        lsp_respond(request.id, null);
        return;
    }

    edits: [..]Rename_Edit;

    add_edit :: (edits: *[..]Rename_Edit, location: Node.Location) {
        for edits.* {
            if it.file == location.file && it.line == location.l0 && it.char == location.c0 return;
        }

        array_add(edits, .{ file=location.file, line=location.l0, char=location.c0 });
    }

    files_to_scan: [..]*Program_File;
    array_add(*files_to_scan, file);

    for target: targets {
        target_file := get_file(target.location.file);
        if !target_file continue;

        array_add_if_unique(*files_to_scan, target_file);
        for get_files_that_use(target_file) array_add_if_unique(*files_to_scan, it);

        // Declarations whose name is its own identifier node (for values, compound declarations, ...)
        // are renamed by the identifier scan below.
        if find_identifier_at(target_file, target.location) continue;

        add_edit(*edits, target.location);
    }

    for scan_file: files_to_scan {
        for node: scan_file.nodes {
            if node.kind != .IDENTIFIER continue;

            ident := cast(*Identifier) node;
            if ident.name != old_name continue;

            decls := resolve_identifier_decls(ident, resolve_procedure_calls=false);

            matched: bool;
            for decl: decls {
                for target: targets {
                    if same_decl(decl, target) {
                        matched = true;
                        break decl;
                    }
                }
            }

            if matched add_edit(*edits, ident.location);
        }
    }

    if edits.count == 0 {
        lsp_respond(request.id, null);
        return;
    }

    Per_File_Edits :: struct {
        uri: string;
        text_edits: [..]LSP_Text_Edit;
    }

    per_file: [..]Per_File_Edits;

    for edit: edits {
        uri := path_to_lsp_path(edit.file);

        entry: *Per_File_Edits;
        for *per_file {
            if it.uri == uri {
                entry = it;
                break;
            }
        }

        if !entry {
            entry = array_add(*per_file);
            entry.uri = uri;
        }

        text_edit: LSP_Text_Edit;
        text_edit.range.start = .{ line=xx edit.line, character=xx edit.char };
        text_edit.range.end = .{ line=xx edit.line, character=xx (edit.char + old_name.count) };
        text_edit.newText = new_name;

        array_add(*entry.text_edits, text_edit);
    }

    document_changes := NewArray(per_file.count, LSP_Text_Document_Edit);
    for per_file {
        document_changes[it_index].textDocument.uri = it.uri;
        document_changes[it_index].edits = it.text_edits;
    }

    Rename_Result :: struct {
        documentChanges: []LSP_Text_Document_Edit;
    }

    lsp_respond(request.id, Rename_Result.{ documentChanges=document_changes });
}
