log_node :: (node: *Node, prefix: string = "") {
    log("%'%'", prefix, to_debug_string(node));
}

to_string :: (node: *Node) -> string {
     if !node {
        return "null";
    }

    builder: String_Builder;
    ast_print(*builder, node);
    return builder_to_string(*builder);
}

type_name :: (type: *Node, pointer_depth: int, bindings: []Poly_Binding = .[]) -> string, resolved: bool {
    builder: String_Builder;
    for 1..pointer_depth append(*builder, "*");

    if !type return "", false;

    resolved := false;
    array_type := cast(*Array_Type) type;
    literal := cast(*Literal) type;
    if type.kind != .LITERAL && type.parent && type.parent.kind == .DECLARATION && (cast(*Declaration) type.parent).expression == type {
        append(*builder, (cast(*Declaration) type.parent).name);
        resolved = type.kind == .STRUCT || type.kind == .UNION || type.kind == .ENUM;
    } else if type.kind == .IDENTIFIER {
        append(*builder, (cast(*Identifier) type).name);
        resolved = array_find(BUILTIN_TYPES, (cast(*Identifier) type).name);
    } else if type.kind == .LITERAL {
        resolved = true;
        if literal.value_type == {
            case .INT;    append(*builder, "int");
            case .FLOAT;  append(*builder, "float");
            case .STRING; append(*builder, "string");
            case .BOOL;   append(*builder, "bool");
            case;
                ast_print(*builder, type);
                resolved = false;
        }
    } else if type.kind == .ARRAY_TYPE {
        element, _, element_bindings, element_depth := get_node_type(array_type.element_type, root_bindings = bindings);
        element_name, element_resolved := type_name(element, element_depth, element_bindings);

        dimension := array_type.dimension;
        dimension_resolved := true;
        if dimension && dimension.kind != .LITERAL {
            constant := get_node_type(dimension);
            if !constant || constant.kind != .LITERAL {
                bound := get_node_type(dimension, root_bindings = bindings);
                if bound && bound.kind == .LITERAL dimension = bound;
                else dimension_resolved = false;
            }
        }

        if array_type.resizable {
            append(*builder, "[..]");
        } else {
            append(*builder, "[");
            if dimension ast_print(*builder, dimension);
            append(*builder, "]");
        }

        if element_resolved {
            append(*builder, element_name);
        } else {
            ast_print(*builder, array_type.element_type);
        }

        resolved = element_resolved && dimension_resolved;
    } else {
        ast_print(*builder, type);
    }

    return builder_to_string(*builder,, temp), resolved;
}

get_line_starts :: (content: string) -> []int {
    line_starts: [..]int;
    array_add(*line_starts, 0);
    for content if it == #char "\n" array_add(*line_starts, it_index + 1);
    return line_starts;
}

utf16_column ::(line: string, byte_column: int) -> int {
    column := byte_column;
    for 0..min(byte_column, line.count)-1 {
        if line[it] & 0xC0 == 0x80 column -= 1;
        if line[it] >= 0xF0 column += 1;
    }

    return column;
}

to_debug_string :: (node: *Node) -> string {
    if !node {
        return "null";
    }

    if node.kind == {
        case .IDENTIFIER;
            return tprint("IDENTIFIER (%)", cast(*Identifier, node).name);
        case;
            return tprint("%", node.kind);
    }
}

array_reverse :: (array: []$T) {
    n := array.count;
    i := 0;
    while i < n / 2 {
        j := n - 1 - i;
        tmp := array[i];
        array[i] = array[j];
        array[j] = tmp;
        i += 1;
    }
}

log_path :: (path: []*Node) {
    for path log("[%]: %", it_index, to_string(it));
}

operator == :: (a: Node.Location, b: Node.Location) -> bool {
    return a.l0 == b.l0 && a.l1 == b.l1 && a.c0 == b.c0 && a.c1 == b.c1;
}
