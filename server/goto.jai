resolve_enum_literal :: (ident: *Identifier) -> *Declaration {
    // Check if this identifier is part of an enum literal (parent is UNARY_OPERATION with DOT)
    if !ident.parent || ident.parent.kind != .UNARY_OPERATION {
        return null;
    }

    expected_type := get_enum_literal_type(cast(*Unary_Operation) ident.parent);
    if !expected_type {
        return null;
    }

    return get_member_in(expected_type, ident);
}

get_enum_literal_type :: (unary_op: *Unary_Operation) -> *Node {
    if unary_op.operation != .DOT {
        return null;
    }

    type := get_expected_type(unary_op);
    if !type || type.kind != .ENUM {
        return null;
    }

    return type;
}

get_expected_type :: (node: *Node) -> *Node {
    if !node.parent {
        return null;
    }

    // Check what context the enum literal is in
    parent_node := node.parent;
    original_parent_node := parent_node;
    expected_type: *Node;

    // For enum_flags with OR/AND operations in .PROCEDURE_CALL arguments, traverse up to find the actual context
    while parent_node && parent_node.kind == .BINARY_OPERATION {
        binary_op := cast(*Binary_Operation) parent_node;
        if binary_op.operation == .PIPE || binary_op.operation == .BITWISE_AND || binary_op.operation == .ASSIGN {
            parent_node = binary_op.parent;
        } else {
            break;
        }
    }

    if parent_node && parent_node.kind != .PROCEDURE_CALL {
        parent_node = original_parent_node;
    }

    if !parent_node {
        return null;
    }

    if parent_node.kind == .PROCEDURE_CALL {
        // For procedure arguments:
        // - SDL_CreateGPUDevice(.SPIRV | .MSL | .DXIL, ...)
        proc_call := cast(*Procedure_Call) parent_node;

        // Find which argument contains our enum literal (might be nested in OR or AND operations if it's an enum_flag)
        arg_index := -1;
        for arg, index: proc_call.arguments {
            // Check if our node is somewhere within this argument
            if is_parent_of(arg, node) {
                arg_index = index;
                break;
            }
        }

        _, procedure := get_procedure_call_decl(proc_call);
        if arg_index >= 0 && procedure {
            params := get_procedure_parameters(procedure);
            index := get_argument_parameter(params, proc_call.arguments[arg_index], arg_index);
            if index >= 0 expected_type = get_node_type(get_parameter_type(params[index]));
        }

    } else if parent_node.kind == .DECLARATION {
        // For struct field defaults: se: Some_Enum = .A
        decl := cast(*Declaration) parent_node;

        if decl.type_inst {
            expected_type = get_node_type(decl.type_inst);
        }

    } else if parent_node.kind == .BINARY_OPERATION {
        binary_op := cast(*Binary_Operation) parent_node;

        // Special case: Check if this comparison is part of a member access chain
        if binary_op.parent && binary_op.parent.kind == .BINARY_OPERATION {
            parent_op := cast(*Binary_Operation) binary_op.parent;

            if parent_op.operation == .DOT {
                comparison_left := binary_op.left;
                if comparison_left && comparison_left.kind == .IDENTIFIER {
                    operation_ident := cast(*Identifier) comparison_left;
                    left_type := get_node_type(parent_op.left);

                    if left_type {
                        decl := get_member_in(left_type, operation_ident);
                        if decl {
                            expected_type = get_node_type(decl);
                        }
                    }
                }
            }
        }

        // If not found yet, handle normal cases
        if !expected_type {
            if binary_op.operation == .ASSIGN {
                // For: se = .B or flags = .SPIRV | .MSL
                // Also handles: ident.parent.kind = .UNARY_OPERATION

                if binary_op.left && binary_op.left.kind == .IDENTIFIER && binary_op.parent && binary_op.parent.kind == .LITERAL {
                    member := resolve_struct_literal_member(cast(*Identifier) binary_op.left);
                    if member {
                        expected_type = get_node_type(member);
                    }
                } else if binary_op.left && binary_op.left.kind == .BINARY_OPERATION {
                    // Left side is a member access chain
                    member_access := cast(*Binary_Operation) binary_op.left;
                    if member_access.operation == .DOT {
                        // Get the type of the entire member access chain
                        expected_type = get_node_type(member_access);
                    }
                } else if binary_op.left && binary_op.left.kind == .IDENTIFIER {
                    // Left side is a simple identifier
                    left_ident := cast(*Identifier) binary_op.left;
                    decls := get_identifier_decls(left_ident);

                    if decls.count > 0 {
                        decl := decls[0];
                        if decl.expression && decl.expression.kind == .BINARY_OPERATION {
                            member_access := cast(*Binary_Operation) decl.expression;
                            if member_access.operation == .DOT && member_access.left {
                                expected_type = get_node_type(member_access.left);
                            }
                        }
                        if !expected_type && decl.type_inst {
                            expected_type = get_node_type(decl.type_inst);
                        }
                    }
                }
            } else if binary_op.operation == .IS_EQUAL    || binary_op.operation == .IS_NOT_EQUAL ||
                      binary_op.operation == .LOGICAL_AND || binary_op.operation == .LOGICAL_OR   ||
                      binary_op.operation == .PIPE        || binary_op.operation == .BITWISE_AND {
                other_side: *Node;
                if binary_op.left == node {
                    other_side = binary_op.right;
                } else {
                    other_side = binary_op.left;
                }

                // Handle member access chains
                if other_side && other_side.kind == .BINARY_OPERATION {
                    member_chain := cast(*Binary_Operation) other_side;
                    if member_chain.operation == .DOT {
                        expected_type = get_node_type(other_side);
                    }
                } else if other_side && other_side.kind == .IDENTIFIER {
                    other_ident := cast(*Identifier) other_side;
                    decls := get_identifier_decls(other_ident);

                    if decls.count > 0 {
                        decl := decls[0];
                        if decl.expression && decl.expression.kind == .BINARY_OPERATION {
                            member_access := cast(*Binary_Operation) decl.expression;
                            if member_access.operation == .DOT && member_access.left {
                                expected_type = get_node_type(member_access.left);
                            }
                        }
                        if !expected_type && decl.type_inst {
                            expected_type = get_node_type(decl.type_inst);
                        }
                    }
                }

                if !expected_type {
                    expected_type = get_node_type(other_side);
                }
            }
        }
    }

    return expected_type;
}

handle_goto_load :: (request: LSP_Request_Message_Definition, file: *Program_File, _load: *Directive_Load) {
    file_path_without_filename := trim_right(path_strip_filename(file.path), "/");
    load_absolute_path := join(file_path_without_filename, _load.file, separator="/");

    lsp_location: LSP_Location_Link;
    lsp_location.originSelectionRange = .{
        start=.{line=xx _load.location.l0, character=xx(_load.location.c1 - _load.file.count-1)},
        end=.{line=xx _load.location.l0, character=xx(_load.location.c1-1)}
    };
    lsp_location.targetUri = path_to_lsp_path(load_absolute_path);

    lsp_respond(request.id, lsp_location);
}

handle_goto_import :: (request: LSP_Request_Message_Definition, file: *Program_File, _import: *Directive_Import) {
    push_allocator(temp);

    file_path_without_filename := trim_right(path_strip_filename(file.path), "/");
    module_absolute_path := get_module_import_path(_import, file_path_without_filename);

    lsp_location: LSP_Location_Link;
    lsp_location.originSelectionRange = .{
        start=.{line=xx _import.location.l0, character=xx(_import.location.c1 - _import.module.count-1)},
        end=.{line=xx _import.location.l0, character=xx(_import.location.c1-1)}
    };
    lsp_location.targetUri = path_to_lsp_path(module_absolute_path);

    lsp_respond(request.id, lsp_location);
}

resolve_dot_member :: (ident: *Identifier) -> handled: bool, decl: *Declaration {
    if !ident || !ident.parent return false, null;

    leaf_node: *Node = ident;
    while leaf_node.parent {
        parent := leaf_node.parent;
        if parent.kind == .ARRAY_SUBSCRIPT && (cast(*Array_Subscript) parent).expression == leaf_node {
            leaf_node = parent;
        } else if parent.kind == .PROCEDURE_CALL && (cast(*Procedure_Call) parent).procedure == leaf_node {
            leaf_node = parent;
        } else if parent.kind == .UNARY_OPERATION && (cast(*Unary_Operation) parent).operation == .POINTER_DEREFERENCE {
            leaf_node = parent;
        } else {
            break;
        }
    }

    if !leaf_node.parent || leaf_node.parent.kind != .BINARY_OPERATION {
        return false, null;
    }

    op := cast(*Binary_Operation) leaf_node.parent;
    path := get_dot_path(op, leaf_node);
    if path.count < 2 {
        return false, null;
    }

    path = array_view(path, 0, path.count-1);

    type := get_path_type(path);
    if !type {
        return true, null;
    }

    return true, get_member_in(type, ident);
}

resolve_named_argument :: (ident: *Identifier) -> *Declaration {
    if !ident.parent || ident.parent.kind != .BINARY_OPERATION {
        return null;
    }

    binary_op := cast(*Binary_Operation) ident.parent;

    // We only want to resolve this if its the left side.
    if binary_op.right == ident {
        return null;
    }

    if !binary_op.parent || binary_op.parent.kind != .PROCEDURE_CALL {
        return null;
    }

    _, procedure := get_procedure_call_decl(cast(*Procedure_Call) binary_op.parent);
    if !procedure {
        return null;
    }

    for get_procedure_parameters(procedure) {
        if it.name == ident.name return it;
    }

    return null;
}

resolve_struct_literal_member :: (ident: *Identifier) -> *Declaration {
    if !ident.parent || ident.parent.kind != .BINARY_OPERATION {
        return null;
    }

    binary_op := cast(*Binary_Operation) ident.parent;

    // We only want to resolve this if its the left side.
    if binary_op.right == ident {
        return null;
    }

    if !binary_op.parent || binary_op.parent.kind != .LITERAL {
        return null;
    }

    literal := cast(*Literal) binary_op.parent;

    type := get_struct_literal_type(literal);
    if !type {
        return null;
    }

    return get_member_in(type, ident);
}

get_struct_literal_type :: (literal: *Literal) -> *Node {
    if literal.value_type != .STRUCT {
        return null;
    }

    type: *Node;
    if literal.struct_literal_info.type {
        type = get_node_type(literal.struct_literal_info.type);
    } else {
        type = get_expected_type(literal);
    }

    if !type || type.kind != .STRUCT {
        return null;
    }

    return type;
}

resolve_assignment_member :: (ident: *Identifier) -> *Declaration {
    if !ident.parent || ident.parent.kind != .UNARY_OPERATION {
        return null;
    }

    unary_op := cast(*Unary_Operation, ident.parent);
    if unary_op.operation != .DOT {
        return null;
    }

    if !unary_op.parent || unary_op.parent.kind != .BINARY_OPERATION  {
        return null;
    }

    binary_op_type := get_node_type(unary_op.parent, true);
    if !binary_op_type {
        return null;
    }

    return get_member_in(binary_op_type, ident);
}

resolve_procedure_call_decl :: (ident: *Identifier) -> *Declaration {
    if !ident.parent || ident.parent.kind != .PROCEDURE_CALL {
        return null;
    }

    proc_call := cast(*Procedure_Call) ident.parent;

    if proc_call.procedure != ident {
        return null;
    }

    return get_procedure_call_decl(proc_call);
}

resolve_enum_member :: (ident: *Identifier) -> *Declaration {
    if !ident.parent || ident.parent.kind != .BLOCK return null;

    block := cast(*Block) ident.parent;
    if !block.parent || block.parent.kind != .ENUM return null;

    return get_member_in(block.parent, ident);
}

resolve_identifier_decls :: (ident: *Identifier, resolve_procedure_calls := true) -> []*Declaration {
    single :: (decl: *Declaration) -> []*Declaration {
        decls := NewArray(1, *Declaration);
        decls[0] = decl;
        return decls;
    }

    decl := resolve_enum_member(ident);
    if decl return single(decl);

    decl = resolve_named_argument(ident);
    if decl return single(decl);

    decl = resolve_struct_literal_member(ident);
    if decl return single(decl);

    decl = resolve_assignment_member(ident);
    if decl return single(decl);

    decl = resolve_enum_literal(ident);
    if decl return single(decl);

    handled, member := resolve_dot_member(ident);
    if handled {
        if member return single(member);
        return .[];
    }

    if resolve_procedure_calls {
        decl = resolve_procedure_call_decl(ident);
        if decl return single(decl);
    }

    return get_identifier_decls(ident);
}

handle_goto :: (request: LSP_Request_Message_Definition) {
    push_allocator(temp);

    file_path := normalize_path(request.params.textDocument.uri);

    file := get_file(file_path);
    if !file {
        log_error("File does not exists or has not been parser yet! (%)", file_path);
        lsp_respond(request.id, null);
        return;
    }

    cursor_location := lsp_location_to_node_location(request.params.position, file_path);

    cursor_node := get_node_by_location(file, cursor_location);
    if !cursor_node {
        lsp_respond(request.id, null);
        return;
    }

    if cursor_node.kind == .DIRECTIVE_LOAD {
        handle_goto_load(request, file, xx cursor_node);
        return;
    }

    if cursor_node.kind == .DIRECTIVE_IMPORT {
        handle_goto_import(request, file, xx cursor_node);
        return;
    }

    if cursor_node.kind != .IDENTIFIER {
        lsp_respond(request.id, null);
        return;
    }

    decls := resolve_identifier_decls(cast(*Identifier) cursor_node);
    if decls.count == 0 {
        lsp_respond(request.id, null);
        return;
    }

    locations := NewArray(decls.count, LSP_Location);
    for decls locations[it_index] = node_location_to_lsp_location(it.location);

    lsp_respond(request.id, locations);
}
