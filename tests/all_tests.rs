mod infra;

// Your tests go here!
success_tests! {
    // Number and Boolean Literals
    {
        name: num,
        file: "num.snek",
        expected: "644",
    },
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },

    // Input Expression
    {
        name: input_default,
        file: "input0.snek",
        expected: "false",
    },
    {
        name: input_bool,
        file: "input0.snek",
        input: "true",
        expected: "true",
    },
    {
        name: input_num,
        file: "input0.snek",
        input: "123",
        expected: "123",
    },

    // Simple Number Expressions
    {
        name: add1,
        file: "add1.snek",
        expected: "73",
    },
    {
        name: add1_sub1,
        file: "add1_sub1.snek",
        expected: "4",
    },
    {
        name: add_num,
        file: "add.snek",
        input: "10",
        expected: "15",
    },

    // Nested Arithmetic Expressions
    {
        name: nested_arith0,
        file: "nested_arith0.snek",
        expected: "35",
    },
    {
        name: nested_arith1,
        file: "nested_arith1.snek",
        expected: "25",
    },
    {
        name: nested_arith2,
        file: "nested_arith2.snek",
        expected: "0",
    },
    {
        name: nested_arith3,
        file: "nested_arith3.snek",
        input: "8",
        expected: "1117",
    },
    {
        name: nested_arith4,
        file: "nested_arith4.snek",
        expected: "-1",
    },

    // Dynamic Type Checks with isnum/isbool
    {
        name: type_check_succ0,
        file: "isnum.snek",
        expected: "false",
    },
    {
        name: type_check_succ1,
        file: "isnum.snek",
        input: "547",
        expected: "true",
    },
    {
        name: type_check_succ2,
        file: "isnum.snek",
        input: "true",
        expected: "false",
    },
    {
        name: type_check_succ3,
        file: "isbool.snek",
        expected: "true",
    },
    {
        name: type_check_succ4,
        file: "isbool.snek",
        input: "689",
        expected: "false",
    },
    {
        name: type_check_succ5,
        file: "type_check_succ5.snek",
        expected: "true",
    },

    // Comparison Expressions
    {
        name: compare_expr_succ0,
        file: "compare_expr_succ0.snek",
        expected: "true",
    },

    {
        name: compare_expr_succ2,
        file: "compare_expr_succ2.snek",
        expected: "true",
    },

    // Let expressions
    {
        name: let_and_set,
        file: "let_and_set.snek",
        expected: "6",
    },
    {
        name: binding0,
        file: "binding0.snek",
        expected: "5",
    },
    {
        name: binding1,
        file: "binding1.snek",
        expected: "-5",
    },

    {
        name: binding_expr,
        file: "binding_expr.snek",
        expected: "1225",
    },
    {
        name: binding_nested,
        file: "binding_nested.snek",
        expected: "1",
    },

    {
        name: binding_chain,
        file: "binding_chain.snek",
        expected: "3",
    },
    {
        name: binding_nested_chain,
        file: "binding_nested_chain.snek",
        expected: "12",
    },

    // Let expressions with shadowing
    {
        name: shadowed_binding_succ0,
        file: "shadowed_binding_succ0.snek",
        expected: "100",
    },
    {
        name: shadowed_binding_succ1,
        file: "shadowed_binding_succ1.snek",
        expected: "7",
    },
    {
        name: shadowed_binding_succ2,
        file: "shadowed_binding_succ2.snek",
        expected: "150",
    },
    {
        name: shadowed_binding_succ3,
        file: "shadowed_binding_succ3.snek",
        expected: "5",
    },
    {
        name: shadowed_binding_succ4,
        file: "shadowed_binding_succ4.snek",
        expected: "18",
    },
    {
        name: shadowed_binding_succ5,
        file: "shadowed_binding_succ5.snek",
        expected: "5",
    },
    {
        name: shadowed_binding_succ6,
        file: "shadowed_binding_succ6.snek",
        expected: "3",
    },
    {
        name: shadowed_binding_succ7,
        file: "shadowed_binding_succ7.snek",
        expected: "200",
    },

    // Misc complex expressions with arithmetic and let bindings
    {
        name: complex_expr,
        file: "complex_expr.snek",
        expected: "6",
    },
    {
        name: quick_brown_fox,
        file: "quick_brown_fox.snek",
        expected: "-3776",
    },

    // If expressions
    {
        name: isbool_isnum_1,
        file: "isbool_isnum.snek",
        input: "10",
        expected: "true"
    },
    {
        name: isbool_isnum_2,
        file: "isbool_isnum.snek",
        input: "true",
        expected: "false"
    },
    {
        name: input_compare_1,
        file: "input_compare.snek",
        input: "2",
        expected: "false",
    },
    {
        name: input_compare_2,
        file: "input_compare.snek",
        input: "10",
        expected: "true",
    },
    {
        name: condition,
        file: "condition.snek",
        expected: "true"
    },
    {
        name: condition_2,
        file: "condition_2.snek",
        expected: "false"
    },
    {
        name: if_expr_succ0,
        file: "if_expr_succ0.snek",
        expected: "10",
    },
    {
        name: if_expr_succ1,
        file: "if_expr_input.snek",
        input: "635",
        expected: "20",
    },
    {
        name: if_expr_succ2,
        file: "if_expr_succ2.snek",
        expected: "8",
    },
    {
        name: if_expr_succ3,
        file: "if_expr_succ3.snek",
        expected: "7",
    },

    // Set expr
    {
        name: set,
        file: "set.snek",
        expected: "true"
    },
    {
        name: set_expr_succ0,
        file: "set_expr1.snek",
        expected: "true",
    },
    {
        name: set_expr_succ1,
        file: "set_expr2.snek",
        expected: "25",
    },
    {
        name: set_expr_succ2,
        file: "set_expr3.snek",
        input: "25",
        expected: "true",
    },
    {
        name: set_expr_succ3,
        file: "set_expr3.snek",
        input: "20",
        expected: "false",
    },

    // loop expression
    {
        name: loop_1,
        file: "loop_1.snek",
        expected: "-6",
    },
    {
        name: input_loop_1,
        file: "input_loop.snek",
        input: "-1",
        expected: "1",
    },
    {
        name: input_loop_2,
        file: "input_loop.snek",
        input: "4",
        expected: "24",
    },
    {
        name: loop_expr_succ0,
        file: "loop_expr0.snek",
        input: "3",
        expected: "6",
    },
    {
        name: loop_expr_succ1,
        file: "loop_expr0.snek",
        input: "7",
        expected: "5040",
    },
    {
        name: loop_expr_succ2,
        file: "loop_expr1.snek",
        expected: "-6",
    },

    // Call expression
    {
        name: fact,
        file: "fact.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: even_odd_1,
        file: "even_odd.snek",
        input: "10",
        expected: "10\ntrue\ntrue",
    },
    {
        name: even_odd_2,
        file: "even_odd.snek",
        input: "9",
        expected: "9\nfalse\nfalse",
    },
    {
        name: func_no_arg,
        file: "func_no_arg.snek",
        expected: "9\n9",
    },
    {
        name: func_mult_arg,
        file: "func_mult_arg.snek",
        input: "9",
        expected: "9\n9",
    },
    {
        name: func_many_print,
        file: "fun_many_print.snek",
        input: "9",
        expected: "9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9\n9",
    },
    {
        name: func_many_call,
        file: "func_many_call.snek",
        expected: "8\n9\n9\n9",
    },

    // test depth
    {
        name: let_depth,
        file: "let_depth.snek",
        expected: "5",
    },
    {
        name: add_depth,
        file: "add_depth.snek",
        expected: "8",
    },

    // tuple expression
    {
        name: tuple,
        file: "tuple.snek",
        expected: "(1, 3, true, false)",
    },
    {
        name: tuple_nested,
        file: "tuple_nested.snek",
        expected: "(2, 3, (5, 8), (1, (3, 5)))",
    },
    {
        name: tuple_index,
        file: "tuple_index.snek",
        input: "1",
        expected: "5",
    },
    {
        name: tuple_index_nested,
        file: "tuple_index_nested.snek",
        expected: "8",
    },
    {
        name: tuple_recursive,
        file: "tuple_recursive.snek",
        expected: "(2, 8, 5)",
    },
    // address, num, boolean equal
    {
        name: tuple_equal,
        file: "tuple_equal.snek",
        expected: "true\nfalse",
    },
    // print tuple
    {
        name: simple_example,
        file: "simple_example.snek",
        expected: "(2, 4, nil, false)\n((2, 4, nil, false), 2, 3)\n(2, 4, ((...), 2, 3), false)\n((2, 4, (...), false), 2, 3)\nnil\ntrue\nfalse",
    },
    {
        name: points_0,
        file: "points.snek",
        input: "1",
        expected: "(7, 4)\n(-7, 13)\n(-9, 16)",
    },
    {
        name: points_1,
        file: "points.snek",
        input: "-8",
        expected: "(7, -5)\n(-7, 13)\n(-9, 16)",
    },
}

runtime_error_tests! {
    // integer overflow
    {
        name: number_overflow_fail0,
        file: "number_overflow_fail0.snek",
        expected: "overflow",
    },
    {
        name: number_overflow_fail1,
        file: "number_overflow_fail1.snek",
        expected: "overflow",
    },
    {
        name: number_overflow_fail2,
        file: "add.snek",
        input: "4611686018427387899",
        expected: "overflow",
    },
    {
        name: number_overflow_fail3,
        file: "nested_arith3.snek",
        input: "4611686018427387890",
        expected: "overflow",
    },

    // type mismatch
    {
        name: invalid_argument_fail0,
        file: "invalid_argument_fail0.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail1,
        file: "invalid_argument_fail1.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail2,
        file: "invalid_argument_fail2.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail3,
        file: "invalid_argument_fail3.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail4,
        file: "invalid_argument_fail4.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail5,
        file: "invalid_argument_fail5.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail6,
        file: "invalid_argument_fail6.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail7,
        file: "nested_arith3.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail8,
        file: "if_expr_input.snek",
        input: "665",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail9,
        file: "set_expr3.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail10,
        file: "loop_expr0.snek",
        input: "5",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail11,
        file: "invalid_argument_fail11.snek",
        expected: "invalid argument",
    },
    {
        name: multiple_args_type_error,
        file: "mul_arg_type_error.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument,
        file: "invalid_argument.snek",
        expected: "invalid argument",
    },
    {
        name: input_compare_3,
        file: "input_compare.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: overflow_add1,
        file: "overflow_add1.snek",
        expected: "overflow",
    },
    {
        name: condition_1,
        file: "condition_1.snek",
        expected: "invalid argument",
    },
    // index out of bound
    {
        name: tuple_index_1,
        file: "tuple_index.snek",
        input: "2",
        expected: "out of bound",
    },
    {
        name: tuple_index_2,
        file: "tuple_index.snek",
        input: "-2",
        expected: "out of bound",
    },
    {
        name: error_bounds,
        file: "error-bounds.snek",
        expected: "out of bound",
    },
    // type mismatch
    {
        name: tuple_equal_fail,
        file: "tuple_equal_fail.snek",
        expected: "invalid argument",
    },
    {
        name: error_tag,
        file: "error-tag.snek",
        expected: "invalid argument",
    },
    {
        name: points_false,
        file: "points.snek",
        input: "true",
        expected: "invalid argument",
    },
}

static_error_tests! {
    // Invalid S-expressions
    {
        name: parse_sexp_fail1,
        file: "parse_sexp_fail1.snek",
        expected: "Invalid",
    },
    {
        name: parse_sexp_fail2,
        file: "parse_sexp_fail2.snek",
        expected: "Invalid",
    },

    // Invalid tokens/operators
    {
        name: parse_token_fail1,
        file: "parse_token_fail1.snek",
        expected: "Invalid",
    },
    {
        name: parse_token_fail2,
        file: "parse_token_fail2.snek",
        expected: "Invalid",
    },
    {
        name: parse_token_fail3,
        file: "parse_token_fail3.snek",
        expected: "Invalid",
    },
    {
        name: parse_token_fail4,
        file: "parse_token_fail4.snek",
        expected: "Invalid",
    },


    // Invalid/Out of bounds Number Literal
    {
        name: number_bounds_fail0,
        file: "number_bounds_fail0.snek",
        expected: "Invalid",
    },
    {
        name: number_bounds_fail1,
        file: "number_bounds_fail1.snek",
        expected: "Invalid",
    },

    // Invalid operator arguments
    {
        name: parse_op_fail1,
        file: "parse_op_fail1.snek",
        expected: "Invalid",
    },
    {
        name: parse_op_fail2,
        file: "parse_op_fail2.snek",
        expected: "Invalid",
    },
    {
        name: parse_op_fail3,
        file: "parse_op_fail3.snek",
        expected: "Invalid",
    },
    {
        name: parse_op_fai4,
        file: "parse_op_fail4.snek",
        expected: "Invalid",
    },
    {
        name: parse_op_fail5,
        file: "parse_op_fail5.snek",
        expected: "Invalid",
    },
    {
        name: parse_op_fail6,
        file: "parse_op_fail6.snek",
        expected: "Invalid",
    },
    {
        name: parse_op_fail7,
        file: "parse_op_fail7.snek",
        expected: "Invalid",
    },
    {
        name: parse_op_fail8,
        file: "parse_op_fail8.snek",
        expected: "Invalid",
    },

    // Invalid let expressions
    {
        name: parse_let_nobindings_fail,
        file: "parse_let_nobindings_fail.snek",
        expected: "Invalid",
    },
    {
        name: parse_let_improperargs_fail1,
        file: "parse_let_improperargs_fail1.snek",
        expected: "Invalid",
    },
    {
        name: parse_let_improperargs_fail2,
        file: "parse_let_improperargs_fail2.snek",
        expected: "Invalid",
    },
    {
        name: parse_let_improperargs_fail3,
        file: "parse_let_improperargs_fail3.snek",
        expected: "Invalid",
    },
    {
        name: parse_let_improperargs_fail4,
        file: "parse_let_improperargs_fail4.snek",
        expected: "Invalid",
    },
    {
        name: parse_let_improperargs_fail5,
        file: "parse_let_improperargs_fail5.snek",
        expected: "keyword",
    },

    {
        name: duplicate_binding_fail0,
        file: "duplicate_binding_fail0.snek",
        expected: "Duplicate binding",
    },
    {
        name: duplicate_binding_fail1,
        file: "duplicate_binding_fail1.snek",
        expected: "Duplicate binding",
    },
    {
        name: duplicate_binding_fail2,
        file: "duplicate_binding_fail2.snek",
        expected: "Duplicate binding",
    },

    // Invalid if expressions
    {
        name: parse_if_fail0,
        file: "parse_if_fail0.snek",
        expected: "Invalid",
    },
    {
        name: parse_if_fail1,
        file: "parse_if_fail1.snek",
        expected: "Invalid",
    },

    // Unbound identifier
    {
        name: unbound_identifier_fail0,
        file: "unbound_identifier_fail0.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: unbound_identifier_fail1,
        file: "unbound_identifier_fail1.snek",
        expected: "Unbound variable identifier y",
    },
    {
        name: unbound_identifier_fail2,
        file: "unbound_identifier_fail2.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: unbound_identifier_fail3,
        file: "unbound_identifier_fail3.snek",
        expected: "Unbound variable identifier z",
    },
    {
        name: unbound_identifier_fail4,
        file: "unbound_identifier_fail4.snek",
        expected: "Unbound variable identifier t",
    },
    {
        name: unbound_identifier_fail5,
        file: "unbound_identifier_fail5.snek",
        expected: "Unbound variable identifier x",
    },

    // Invalid block
    {
        name: parse_block_fail0,
        file: "parse_block_fail0.snek",
        expected: "Invalid",
    },

    // Invalid break
    {
        name: invalid_break_fail0,
        file: "invalid_break_fail0.snek",
        expected: "break",
    },

    // Invalid loop
    {
        name: invalid_loop_fail0,
        file: "invalid_loop_fail0.snek",
        expected: "Invalid",
    },
    {
        name: duplicate_params,
        file: "duplicate_params.snek",
        expected: "",
    },
    {
        name: duplicate_func_def,
        file: "duplicate_func_def.snek",
        expected: "",
    },
    {
        name: func_with_input,
        file: "func_with_input.snek",
        expected: "",
    },
    {
        name: func_wrong_args,
        file: "func_wrong_args.snek",
        expected: "",
    },
    {
        name: func_name_as_variable,
        file: "func_name_as_variable.snek",
        expected: "Unbound variable",
    },
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: block_fail,
        file: "block_fail.snek",
        expected: "Invalid",
    },
    {
        name: let_only,
        file: "let_only.snek",
        expected: "keyword",
    },
    {
        name: invalid_func,
        file: "invalid_func.snek",
        expected: "",
    },
    // tuple syntax
    {
        name: invalid_tuple,
        file: "error3.snek",
        expected: "Invalid",
    },
}
