MAX_IMPORT_ACTIONS :: 5;

Module_Scan :: struct {
    declarations: [..]*Declaration;
    loads: [..]*Directive_Load;
}

module_scan_visit :: (node: *Node, scan: *Module_Scan) {
    if node.kind == .DECLARATION array_add(*scan.declarations, cast(*Declaration) node);
    if node.kind == .DIRECTIVE_LOAD array_add(*scan.loads, cast(*Directive_Load) node);
}

is_visible_to_importers :: (node: *Node) -> bool {
    parent := node.parent;
    while parent {
        if parent.kind == .DIRECTIVE_SCOPE return (cast(*Directive_Scope) parent).scope_kind == .EXPORT;
        if parent.kind == .IF && !(cast(*If) parent).compile_time return false;
        if parent.kind != .BLOCK && parent.kind != .IF && parent.kind != .CASE return false;
        parent = parent.parent;
    }

    return true;
}

index_module :: (module: string, entry: string) {
    pool: Pool;
    set_allocators(*pool, context.default_allocator);
    defer release(*pool);

    files: [..]string;
    defer array_free(files);
    array_add(*files, entry);

    file_index := 0;
    while file_index < files.count {
        push_allocator(pool_alloc(*pool));

        path := files[file_index];
        file_index += 1;
        content, ok := read_entire_file(path, log_errors=false);
        if !ok continue;

        parser: Parser(*Module_Scan);
        parser.user_data = New(Module_Scan);
        parser.node_visit = module_scan_visit;
        parser.lexer = *create_lexer(content, path);

        while !end(parser.lexer) {
            parse(*parser, null);
        }

        directory := trim_right(path_strip_filename(path), "/");
        for parser.user_data.loads {
            array_add_if_unique(*files, normalize_path(join(directory, it.file, separator="/")));
        }

        for decl: parser.user_data.declarations {
            if !decl.name || !is_visible_to_importers(decl) continue;

            modules := table_find_pointer(*server.module_index, decl.name);
            if !modules {
                push_allocator(context.default_allocator);
                empty: [..]string;
                modules = table_add(*server.module_index, copy_string(decl.name), empty);
            }

            if modules.count && modules.*[modules.count-1] == module continue;

            push_allocator(context.default_allocator);
            array_add(modules, module);
        }
    }
}

build_module_index :: () {
    push_allocator(context.default_allocator);

    folders: [..]string;
    defer array_free(folders);

    for server.local_modules {
        if is_absolute_path(it) array_add(*folders, normalize_path(it,, temp));
        else array_add(*folders, normalize_path(join(server.project_root, it, separator="/",, temp),, temp));
    }
    array_add(*folders, normalize_path(tprint("%/modules", server.args.jai_path),, temp));

    Module_Entry :: struct {
        name: string;
        entry: string;
    }

    entries: [..]Module_Entry;
    defer array_free(entries);

    for folder: folders {
        if !file_exists(folder) continue;

        visit_files(folder, false, *entries, (info: *File_Visit_Info, entries: *[..]Module_Entry) {
            entry: Module_Entry;
            if info.is_directory {
                entry.name = info.short_name;
                entry.entry = tprint("%/module.jai", info.full_name);
                if !file_exists(entry.entry) return;
            } else {
                if !ends_with(info.short_name, ".jai") return;
                entry.name = slice(info.short_name, 0, info.short_name.count - ".jai".count);
                entry.entry = copy_string(info.full_name,, temp);
            }

            for entries.* if it.name == entry.name return;
            entry.name = copy_string(entry.name,, temp);
            array_add(entries, entry);
        }, visit_directories=true);
    }

    start := seconds_since_init();
    for entries index_module(copy_string(it.name), it.entry);
    log("Indexed % modules in % ms", entries.count, (seconds_since_init() - start) * 1000);
}

start_module_index_thread :: () {
    thread_init(*server.module_index_thread, (thread: *Thread) -> s64 {
        context.logger = lsp_log;
        build_module_index();
        return 0;
    });
    thread_start(*server.module_index_thread);
}

compare_import_modules :: (a: string, b: string) -> int {
    if a == b return 0;
    if a == "Basic" return -1;
    if b == "Basic" return 1;
    return compare(a, b);
}

handle_code_action :: (request: LSP_Request_Message_Code_Action) {
    push_allocator(temp);

    file := get_file(normalize_path(request.params.textDocument.uri));
    if !file {
        lsp_respond(request.id, null);
        return;
    }

    thread_is_done(*server.module_index_thread, -1);

    insert_line := 0;
    for node: file.nodes {
        if node.kind != .DIRECTIVE_IMPORT continue;

        top := node;
        while top.parent && (top.parent.kind == .DECLARATION || top.parent.kind == .USING || top.parent.kind == .DIRECTIVE_SCOPE) {
            top = top.parent;
        }

        if top.parent != *file.global_block continue;
        insert_line = max(insert_line, node.location.l1 + 1);
    }

    Workspace_Edit :: struct {
        documentChanges: []LSP_Text_Document_Edit;
    }

    Code_Action :: struct {
        title: string;
        kind := "quickfix";
        diagnostics: []LSP_Diagnostic;
        edit: Workspace_Edit;
    }

    actions: [..]Code_Action;

    UNDECLARED :: "Undeclared identifier '";
    for diagnostic: request.params._context.diagnostics {
        message := trim(diagnostic.message);
        if !begins_with(message, UNDECLARED) continue;

        name := slice(message, UNDECLARED.count, message.count - UNDECLARED.count);
        name_end := find_index_from_left(name, #char "'");
        if name_end < 0 continue;
        name.count = name_end;

        modules := table_find_pointer(*server.module_index, name);
        if !modules continue;

        candidates := array_copy(modules.*);
        quick_sort(candidates, compare_import_modules);

        offered := 0;
        for module: candidates {
            if offered == MAX_IMPORT_ACTIONS break;

            imported := false;
            for file.imports if it.module == module imported = true;
            if imported continue;

            edit: LSP_Text_Edit;
            edit.range.start.line = xx insert_line;
            edit.range.end.line = xx insert_line;
            edit.newText = tprint("#import \"%\";\n", module);

            document_changes := NewArray(1, LSP_Text_Document_Edit);
            document_changes[0].textDocument.uri = request.params.textDocument.uri;
            document_changes[0].edits = NewArray(1, LSP_Text_Edit);
            document_changes[0].edits[0] = edit;

            action := array_add(*actions);
            action.title = tprint("Add #import \"%\";", module);
            action.diagnostics = NewArray(1, LSP_Diagnostic);
            action.diagnostics[0] = diagnostic;
            action.edit.documentChanges = document_changes;
            offered += 1;
        }
    }

    lsp_respond(request.id, actions);
}
