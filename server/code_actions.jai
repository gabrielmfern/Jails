NOT_IMPORTABLE_MODULES :: string.["Preload", "Runtime_Support", "Default_Metaprogram", "Minimal_Metaprogram"];

Module_Declaration :: struct {
    name: string;
    module: int;
}

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

index_module :: (module: int, entry: string) {
    ast_pool: Pool;
    set_allocators(*ast_pool, context.default_allocator);
    defer release(*ast_pool);

    names := pool_alloc(*server.module_index_pool);

    files: [..]string;
    defer array_free(files);
    array_add(*files, entry);

    file_index := 0;
    while file_index < files.count {
        push_allocator(pool_alloc(*ast_pool));

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
            array_add(*server.module_declarations, .{ name=copy_string(decl.name,, names), module=module });
        }
    }
}

build_module_index :: () {
    push_allocator(context.default_allocator);
    start := seconds_since_init();

    set_allocators(*server.module_index_pool, context.default_allocator);
    server.module_declarations.allocator = context.default_allocator;

    folders: [..]string;
    folders.allocator = temp;
    for server.local_modules array_add(*folders, local_module_folder_path(it,, temp));
    array_add(*folders, tprint("%/modules", server.args.jai_path));

    names: [..]string;
    names.allocator = temp;
    for folder: folders {
        if !file_exists(folder) continue;

        visit_files(folder, false, *names, (info: *File_Visit_Info, names: *[..]string) {
            name := info.short_name;
            if !info.is_directory {
                if !ends_with(name, ".jai") return;
                name.count -= ".jai".count;
            }

            if array_find(NOT_IMPORTABLE_MODULES, name) return;
            array_add_if_unique(names, copy_string(name,, temp));
        }, visit_directories=true);
    }

    for names {
        entry := get_module(it,, temp);
        if !entry continue;

        array_add(*server.module_names, copy_string(it,, pool_alloc(*server.module_index_pool)));
        index_module(server.module_names.count-1, entry);
    }

    quick_sort(server.module_declarations, (a: Module_Declaration, b: Module_Declaration) -> int { return compare(a.name, b.name); });

    log("Indexed % declarations from % modules in % ms", server.module_declarations.count, server.module_names.count, (seconds_since_init() - start) * 1000);
}

get_modules_declaring :: (name: string) -> [..]string {
    declarations := server.module_declarations;

    low, high := 0, declarations.count;
    while low < high {
        middle := (low + high) / 2;
        if compare(declarations[middle].name, name) < 0 low = middle + 1;
        else high = middle;
    }

    modules: [..]string;
    while low < declarations.count && declarations[low].name == name {
        array_add_if_unique(*modules, server.module_names[declarations[low].module]);
        low += 1;
    }

    quick_sort(modules, (a: string, b: string) -> int {
        if a == b return 0;
        if a == "Basic" return -1;
        if b == "Basic" return 1;
        return compare(a, b);
    });

    return modules;
}

handle_code_action :: (request: LSP_Request_Message_Code_Action) {
    push_allocator(temp);

    file := get_file(normalize_path(request.params.textDocument.uri));
    if !file {
        lsp_respond(request.id, null);
        return;
    }

    actions: [..]LSP_Code_Action;
    insert_line := -1;

    UNDECLARED :: "Undeclared identifier '";
    for diagnostic: request.params._context.diagnostics {
        message := trim(diagnostic.message);
        if !begins_with(message, UNDECLARED) continue;

        name := slice(message, UNDECLARED.count, message.count - UNDECLARED.count);
        name_end := find_index_from_left(name, #char "'");
        if name_end < 0 continue;
        name.count = name_end;

        if !server.module_names.count build_module_index();

        if insert_line < 0 {
            insert_line = 0;
            for node: file.nodes {
                if node.kind != .DIRECTIVE_IMPORT continue;

                top := node;
                while top.parent && (top.parent.kind == .DECLARATION || top.parent.kind == .USING || top.parent.kind == .DIRECTIVE_SCOPE) {
                    top = top.parent;
                }

                if top.parent != *file.global_block continue;
                insert_line = max(insert_line, node.location.l1 + 1);
            }
        }

        for module: get_modules_declaring(name) {
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
            action.kind = "quickfix";
            action.diagnostics = NewArray(1, LSP_Diagnostic);
            action.diagnostics[0] = diagnostic;
            action.edit.documentChanges = document_changes;
        }
    }

    lsp_respond(request.id, actions);
}
