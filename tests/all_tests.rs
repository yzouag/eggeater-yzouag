mod infra;

// Your tests go here!
success_tests! {
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
        name: let_depth,
        file: "let_depth.snek",
        expected: "5",
    },
    {
        name: add_depth,
        file: "add_depth.snek",
        expected: "8",
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
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
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
        name: let_and_set,
        file: "let_and_set.snek",
        expected: "6",
    },
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
        name: set,
        file: "set.snek",
        expected: "true"
    },
}

runtime_error_tests! {
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
}

static_error_tests! {
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
}
