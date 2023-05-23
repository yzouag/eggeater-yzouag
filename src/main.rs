use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use std::collections::HashSet;

struct Program {
    defs: Vec<Definition>,
    main: Expr,
    function_table: HashMap<String, i32>,
}

#[derive(Debug)]
enum Definition {
    Fun(String, Vec<String>, Expr),
}

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i64),
    Label(String),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    RDI,
    RBX,
    RCX, // store rdi
    AL,
    R15,
}

#[derive(Debug)]
enum Instr {
    Label(String),
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ITest(Val, Val),
    IXor(Val, Val),
    ICmp(Val, Val),
    IAnd(Val, Val),
    ISar(Val, Val),
    INeg(Val),
    IJe(Val),
    ISete(Val),
    ISetg(Val),
    ISetge(Val),
    ISetl(Val),
    ISetle(Val),
    IJne(Val),
    IJl(Val),
    IJge(Val),
    IJo(Val),
    IJz(Val),
    IJnz(Val),
    IJmp(Val),
    IPush(Val),
    IPop(Val),
    ICall(Val),
}

#[derive(Debug)]
enum Op1 { Add1, Sub1, IsNum, IsBool, Print,}

#[derive(Debug)]
enum Op2 { Plus, Minus, Times, Equal, Greater, GreaterEqual, Less, LessEqual, }

#[derive(Debug)]
enum Expr {
    Number(i64),
    Boolean(bool),
    Nil,
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Call(String, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Tuple(Vec<Expr>),
    SetIndex(Box<Expr>, Box<Expr>, Box<Expr>),
}

fn is_def(s: &Sexp) -> bool {
    match s {
        Sexp::List(v) => {
            match &v[..] {
                [Sexp::Atom(S(keyword)), Sexp::List(_), _] if keyword == "fun" => true,
                _ => false
            }
        }
        _ => false,
    }
}

fn parse_program(s: &Sexp) -> Program {
    match s {
        Sexp::List(vec) => {
            let mut defs: Vec<Definition> = Vec::new();
            let mut func_table: HashMap<String, i32> = HashMap::new();
            for (i, e) in vec.iter().enumerate() {
                if is_def(e) {
                    defs.push(parse_definition(e, &mut func_table));
                } else {
                    if i != vec.len()-1 {
                        panic!("Invalid Program");
                    }
                    return Program {
                        defs: defs,
                        main: parse_expr(e),
                        function_table: func_table,
                    };
                }
            }
            panic!("Invalid Program");
        },
        _ => panic!("program should be a list")
    }
}

fn parse_definition(s: &Sexp, f_table: &mut HashMap<String, i32>) -> Definition {
    match s {
        Sexp::List(def_vec) => match &def_vec[..] {
            [Sexp::Atom(S(keyword)), Sexp::List(name_vec), body] if keyword == "fun" => {
                match &name_vec[..] {
                    [Sexp::Atom(S(func_name)), rest @ ..] => {
                        if check_keyword(func_name) {
                            panic!("Bad fundef");
                        }
                        if f_table.contains_key(func_name) {
                            panic!("Duplicate parameter name");
                        }
                        f_table.insert(func_name.to_string(), rest.len() as i32);
                        Definition::Fun(func_name.to_string(), parse_fun_args(rest), parse_expr(body))
                    },
                    _ => panic!("Invalid Bad fundef"),
                }
            },
            _ => panic!("Invalid Bad fundef"),
        },
        _ => panic!("Invalid Bad fundef"),
    }
}

fn parse_fun_args(names: &[Sexp]) -> Vec<String> {
    let mut args = Vec::new();
    for name in names {
        match name {
            Sexp::Atom(S(arg_name)) => {
                if check_keyword(arg_name) {
                    panic!("Bad fundef");
                }
                if args.contains(arg_name) {
                    panic!("Bad fundef");
                }
                args.push(arg_name.to_string());
            }
            _ => panic!("Bad fundef"),
        }
    }
    args
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            match n.checked_mul(2) {
                Some(_) => Expr::Number(*n),
                None => panic!("Invalid"),
            }
        },
        Sexp::Atom(S(s)) => {
            match s.as_str() {
                "true" => Expr::Boolean(true),
                "false" => Expr::Boolean(false),
                "nil" => Expr::Nil,
                "input" => Expr::Id("input".to_string()),
                _ => {
                    if check_keyword(s) {
                        panic!("keyword");
                    }
                    Expr::Id(s.to_string())
                }
            }
        }
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(keyword)), Sexp::List(bindings), e] if keyword == "let" => {
                    if bindings.len() == 0 {
                        panic!("Invalid let binding");
                    }
                    Expr::Let(bindings.iter().map(|binding| parse_bind(binding)).collect(), Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(keyword)), Sexp::Atom(S(name)), e] if keyword == "set!" => {
                    Expr::Set(name.to_string(), Box::new(parse_expr(e)))
                },
                [Sexp::Atom(S(keyword)), condition, thn, els] if keyword == "if" => {
                    Expr::If(Box::new(parse_expr(condition)), Box::new(parse_expr(thn)), Box::new(parse_expr(els)))
                },
                [Sexp::Atom(S(keyword)), rest @ ..] if keyword == "block" => {
                    if rest.len() == 0 {
                        panic!("Invalid block");
                    }
                    Expr::Block(rest.iter().map(|e| parse_expr(e)).collect())
                },
                [Sexp::Atom(S(keyword)), rest @ ..] if keyword == "tuple" => {
                    if rest.len() == 0 {
                        panic!("Invalid tuple");
                    }
                    Expr::Tuple(rest.iter().map(|e| parse_expr(e)).collect())
                },
                [Sexp::Atom(S(keyword)), tuple, index] if keyword == "index" => {
                    Expr::Index(Box::new(parse_expr(tuple)), Box::new(parse_expr(index)))
                },
                [Sexp::Atom(S(keyword)), tuple, index, value] if keyword == "setindex!" => {
                    Expr::SetIndex(Box::new(parse_expr(tuple)), Box::new(parse_expr(index)), Box::new(parse_expr(value)))
                },
                [Sexp::Atom(S(keyword)), e] if keyword == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "print" => Expr::UnOp(Op1::Print, Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), rest @ ..] => {
                    Expr::Call(keyword.to_string(), rest.iter().map(|e| parse_expr(e)).collect())
                }
                _ => panic!("Invalid"),
            }
        },
        _ => panic!("Invalid"),
    }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::Atom(_) => panic!("Invalid"),
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(id)), e] => {
                    if check_keyword(id) {
                        panic!("keyword")
                    }
                    (id.to_string(), parse_expr(e))
                }
                _ => panic!("Invalid")
            }
        }
    }
}

fn check_keyword(s: &String) -> bool { // can we modify "input"?
    if vec!["add1", "sub1", "let", "isnum", "isbool", "true", "input", "false", "block",
    "loop", "break", "if", "set!", "setindex!", "index", "tuple", "nil"].iter().any(|w| w == s) {
        return true;
    }
    false
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn check_type(ins: &mut Vec<Instr>) {
    ins.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
    ins.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
    ins.push(Instr::IJnz(Val::Label("throw_error".to_string())));
}

fn check_overflow(ins: &mut Vec<Instr>) {
    ins.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(7)));
    ins.push(Instr::IJo(Val::Label("throw_error".to_string())));
}

fn compile_expr(ins: &mut Vec<Instr>, e: &Expr, si:i64, env: &HashMap<String, i64>, brake: &String, l: &mut i32, is_main: bool, func_table: &HashMap<String, i32>) {
    match e {
        Expr::Number(n) => ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n<<1))),
        Expr::Boolean(b) => {
            if *b {
                ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(7)));
            } else {
                ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            }
        },
        Expr::Nil => ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))),
        Expr::Id(s) => {
            if s == "input" && is_main {
                ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
            } else {
                match env.get(s) {
                    Some(i) => ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *i))),
                    None => panic!("Unbound variable identifier {s}"),
                }
            }
        },
        Expr::Let(bindings, expr) => {
            // first check if there are duplicate bindings
            let mut names: HashSet<&String> = HashSet::new();
            for binding in bindings {
                if !names.insert(&binding.0) {
                    panic!("Duplicate binding");
                }
            }
            // bind one by one
            let mut new_env = HashMap::clone(env);
            let mut new_si = si;
            for (id, subexpr) in bindings {
                compile_expr(ins, subexpr, new_si, &new_env, brake, l, is_main, func_table);
                new_env.insert(id.to_string(), new_si*8);
                ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, new_si*8), Val::Reg(Reg::RAX)));
                new_si += 1;
            }
            compile_expr(ins, expr, new_si, &new_env, brake, l, is_main, func_table);
        },
        Expr::UnOp(op1, subexpr) => {
            compile_expr(ins, subexpr, si, env, brake, l, is_main, func_table);
            match op1 {
                Op1::Add1 | Op1:: Sub1 => {
                    // first check type
                    check_type(ins);
                    if matches!(op1, Op1::Add1) {
                        ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
                    } else {
                        ins.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                    }
                    // finally check if overflow
                    check_overflow(ins);
                },
                Op1::IsBool => {
                    let if_label = new_label(l, "isbool_if");
                    let end_label = new_label(l, "isbool_end");
                    // check last two digits 1
                    ins.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(3)));
                    ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(3)));
                    ins.push(Instr::IJe(Val::Label(if_label.to_string())));
                    // not boolean (!=3)
                    ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))); // false
                    ins.push(Instr::IJmp(Val::Label(end_label.to_string())));
                    // is boolean
                    ins.push(Instr::Label(if_label.to_string()));
                    ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(7))); // true
                    // end
                    ins.push(Instr::Label(end_label));
                },
                Op1::IsNum => {
                    ins.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
                    ins.push(Instr::IXor(Val::Reg(Reg::RAX), Val::Imm(1)));
                    ins.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(4)));
                    ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(3)));
                },
                Op1::Print => {
                    ins.push(Instr::IPush(Val::Reg(Reg::RDI)));
                    ins.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX)));
                    ins.push(Instr::ICall(Val::Label("snek_print".to_owned())));
                    ins.push(Instr::IPop(Val::Reg(Reg::RDI)));
                }
            }
        },
        Expr::BinOp(op2, subexpr1, subexpr2) => {
            if matches!(op2, Op2::Equal) {
                let compare_label = new_label(l, "equal_compare");
                compile_expr(ins, subexpr1, si, env, brake, l, is_main, func_table);
                ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
                compile_expr(ins, subexpr2, si+1, env, brake, l, is_main, func_table);
                // check last bit is same
                ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                ins.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si*8)));
                ins.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
                ins.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                ins.push(Instr::IJnz(Val::Label("throw_error".to_string())));
                // if last bit 0, go to compare
                ins.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                ins.push(Instr::IJz(Val::Label(compare_label.to_string())));
                // if last bit 1, check second last bit
                ins.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(3)));
                ins.push(Instr::IJnz(Val::Label("throw_error".to_string())));
                // compare
                ins.push(Instr::Label(compare_label));
                ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
                ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));       
                ins.push(Instr::ISete(Val::Reg(Reg::AL)));
                ins.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(4)));
                ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(3)));       
            } else {
                compile_expr(ins, subexpr1, si, env, brake, l, is_main, func_table);
                check_type(ins);
                ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
                compile_expr(ins, subexpr2, si+1, env, brake, l, is_main, func_table);
                check_type(ins);
                match op2 {
                    Op2::Plus => {
                        ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
                        check_overflow(ins);
                    },
                    Op2::Minus => {
                        ins.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
                        check_overflow(ins);
                        ins.push(Instr::INeg(Val::Reg(Reg::RAX)));
                    },
                    Op2::Times => {
                        ins.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si*8)));
                        check_overflow(ins);
                        ins.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
                    },
                    _ => {
                        ins.push(Instr::ICmp(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
                        ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0)));
                        if matches!(op2, Op2::Greater) {
                            ins.push(Instr::ISetg(Val::Reg(Reg::AL)));
                        } else if matches!(op2, Op2::GreaterEqual) {
                            ins.push(Instr::ISetge(Val::Reg(Reg::AL)));
                        } else if matches!(op2, Op2::Less) {
                            ins.push(Instr::ISetl(Val::Reg(Reg::AL)));
                        } else if matches!(op2, Op2::LessEqual) {
                            ins.push(Instr::ISetle(Val::Reg(Reg::AL)));
                        }
                        ins.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(4)));
                        ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(3)));
                    }
                }
            }
        },
        Expr::Break(e) => {
            if brake == "" {
                panic!("break");
            }
            compile_expr(ins, e, si, env, brake, l, is_main, func_table);
            ins.push(Instr::IJmp(Val::Label(brake.to_string())));
        },
        Expr::Loop(e) => {
            let start_loop = new_label(l, "loop");
            let end_loop = new_label(l, "endloop");
            ins.push(Instr::Label(String::from(&start_loop)));
            compile_expr(ins, e, si, env, &end_loop, l, is_main, func_table);
            ins.push(Instr::IJmp(Val::Label(start_loop)));
            ins.push(Instr::Label(end_loop));
        },
        Expr::Block(list_expr) => {
            for expr in list_expr {
                compile_expr(ins, expr, si, env, brake, l, is_main, func_table);
            }
        },
        Expr::If(iff, thnn, elze) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            compile_expr(ins, iff, si, env, brake, l, is_main, func_table);
            ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(3)));
            ins.push(Instr::IJe(Val::Label(String::from(&else_label))));
            compile_expr(ins, thnn, si, env, brake, l, is_main, func_table);
            ins.push(Instr::IJmp(Val::Label(String::from(&end_label))));
            ins.push(Instr::Label(else_label));
            compile_expr(ins, elze, si, env, brake, l, is_main, func_table);
            ins.push(Instr::Label(end_label));
        },
        Expr::Set(name, e) => {
            match env.get(name) {
                Some(i) => {
                    compile_expr(ins, e, si, env, brake, l, is_main, func_table);
                    ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, *i), Val::Reg(Reg::RAX)))
                },
                None => panic!("Unbound variable identifier {name}"),
            }
        },
        Expr::Call(name, args) => {
            // validate function call
            match func_table.get(name) {
                None => panic!("Invalid function call"),
                Some(i) => {
                    if *i != args.len() as i32 {
                        panic!("Invalid function call")
                    }
                }
            }
            let num_args = args.len() as i64;
            let mut current = 0;
            // evaluate each arg and put on stack
            for arg in args {
                compile_expr(ins, arg, si+current, env, brake, l, is_main, func_table);
                if current == num_args-1 {
                    break;
                }
                let current_word = (si+current)*8;
                ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, current_word), Val::Reg(Reg::RAX)));
                current += 1;
            }
            // move stack up
            let offset = (num_args + 1) * 8;
            ins.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset)));
            // calling convention, move all temp to func arg positions
            for i in 0..(num_args-1) {
                let current_word_after_sub = (si+i)*8 + offset;
                ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, current_word_after_sub)));
                ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, i*8), Val::Reg(Reg::RBX)));
            }
            if num_args > 0 {
                ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, (num_args-1)*8), Val::Reg(Reg::RAX)));
            }
            ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, num_args*8), Val::Reg(Reg::RDI)));
            // call function
            ins.push(Instr::ICall(Val::Label(name.to_string())));
            // after return, recover rdi
            ins.push(Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffset(Reg::RSP, num_args*8)));
            // recover stack pointer
            ins.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset)));
        },
        Expr::Tuple(elements) => {
            let num_elements = elements.len() as i64;
            let mut current = 0;
            // evaluate each element and put on stack
            for ele in elements {
                compile_expr(ins, ele, si+current, env, brake, l, is_main, func_table);
                if current == num_elements-1 {
                    break;
                }
                let current_word = (si+current)*8;
                ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, current_word), Val::Reg(Reg::RAX)));
                current += 1;
            }
            // move elements to heap
            ins.push(Instr::IMov(Val::RegOffset(Reg::R15, num_elements*8), Val::Reg(Reg::RAX)));
            for i in 1..num_elements {
                ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, (si+i-1)*8)));
                ins.push(Instr::IMov(Val::RegOffset(Reg::R15, i*8), Val::Reg(Reg::RAX)));
            }
            // put tuple length as first element of tuple
            ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(num_elements)));
            ins.push(Instr::IMov(Val::RegOffset(Reg::R15, 0), Val::Reg(Reg::RBX)));
            // move tuple address to rax and +1 for address type
            ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::R15)));
            ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
            // update r15
            ins.push(Instr::IAdd(Val::Reg(Reg::R15), Val::Imm((num_elements+1)*8)));
        },
        Expr::Index(tuple, index) => {
            // compile tuple
            compile_expr(ins, tuple, si, env, brake, l, is_main, func_table);
            // check tuple type
            ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            ins.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(7)));
            ins.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)));
            ins.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            ins.push(Instr::IJne(Val::Label("throw_error".to_string())));
            ins.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)));
            ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
            // compile index and check type, store in RAX
            compile_expr(ins, index, si+1, env, brake, l, is_main, func_table);
            check_type(ins);
            ins.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
            // put tuple address on RBX
            ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si*8)));
            // check index out of range < 0, >= length
            ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RBX, 0)));
            ins.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(8)));
            ins.push(Instr::IJge(Val::Label("throw_error".to_string())));
            ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(0)));
            ins.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(8)));
            ins.push(Instr::IJl(Val::Label("throw_error".to_string())));
            // put element into rax
            ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
            ins.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(8)));
            ins.push(Instr::IAdd(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RBX, 0)));
        },
        Expr::SetIndex(tuple, index, value) => {
            // compile tuple and check type, store it on stack
            compile_expr(ins, tuple, si, env, brake, l, is_main, func_table);
            ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            ins.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(7)));
            ins.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)));
            ins.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            ins.push(Instr::IJne(Val::Label("throw_error".to_string())));
            ins.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)));
            ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, si*8), Val::Reg(Reg::RAX)));
            // compile index and check type
            compile_expr(ins, index, si+1, env, brake, l, is_main, func_table);
            check_type(ins);
            ins.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
            // check index out of range
            ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si*8)));
            ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RBX, 0)));
            ins.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(8)));
            ins.push(Instr::IJge(Val::Label("throw_error".to_string())));
            ins.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(0)));
            ins.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(8)));
            ins.push(Instr::IJl(Val::Label("throw_error".to_string())));
            ins.push(Instr::IMov(Val::RegOffset(Reg::RSP, (si+1)*8), Val::Reg(Reg::RAX)));
            // compile value
            compile_expr(ins, value, si+2, env, brake, l, is_main, func_table);
            // RAX = value, RBX = tuple address, RCX = index
            ins.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si*8)));
            ins.push(Instr::IMov(Val::Reg(Reg::RCX), Val::RegOffset(Reg::RSP, (si+1)*8)));
            ins.push(Instr::IAdd(Val::Reg(Reg::RCX), Val::Imm(1)));
            ins.push(Instr::IMul(Val::Reg(Reg::RCX), Val::Imm(8)));
            ins.push(Instr::IAdd(Val::Reg(Reg::RCX), Val::Reg(Reg::RBX)));
            ins.push(Instr::IMov(Val::RegOffset(Reg::RCX, 0), Val::Reg(Reg::RAX)));
            ins.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            ins.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
        }
    }
}

fn aligned_depth(e: &Expr, args: i64) -> i64 {
    let d = depth(e);
    if (d+args) % 2 == 1 {
        return d+1;
    }
    return d;
}

fn depth(e: &Expr) -> i64 {
    match e {
        Expr::Number(_) | Expr::Boolean(_) | Expr::Id(_) | Expr::Nil => 0,
        Expr::Loop(expr) | Expr:: Break(expr) | Expr::Set(_, expr) |
            Expr::UnOp(_, expr) => depth(expr),
        Expr::BinOp(_, e1, e2) => depth(e1).max(depth(e2) + 1),
        Expr::If(iff, thnn, elsa) => depth(iff).max(depth(thnn)).max(depth(elsa)),
        Expr::Block(exprs) => exprs.iter().map(|expr| depth(expr)).max().unwrap_or(0),
        Expr::Let(bindings, e) => {
            let mut current = 0;
            let mut max_depth = 0;
            for (_, expr) in bindings {
                max_depth = max_depth.max(depth(expr)+current);
                current += 1;
            }
            max_depth.max(depth(e) + bindings.len() as i64)
        },
        Expr::Call(_, args) => {
            let mut current = 0;
            let mut max_depth = 0;
            for arg in args {
                max_depth = max_depth.max(depth(arg)+current);
                current += 1;
            }
            max_depth
        },
        Expr::Tuple(elements) => {
            let mut current = 0;
            let mut max_depth = 0;
            for e in elements {
                max_depth = max_depth.max(depth(e)+current);
                current += 1;
            }
            max_depth
        },
        Expr::Index(tuple, index) => depth(tuple).max(depth(index)+1), // NOTE: should not +1?
        Expr::SetIndex(tuple, index, value) => 
            depth(tuple).max(depth(index)+1).max(depth(value)+2), // should we plus 1??
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(v1, v2) => {
            format!("\nmov {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IAdd(v1, v2) => {
            format!("\nadd {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ISub(v1, v2) => {
            format!("\nsub {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IMul(v1, v2) => {
            format!("\nimul {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ITest(v1, v2) => {
            format!("\ntest {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IXor(v1, v2) => {
            format!("\nxor {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ICmp(v1, v2) => {
            format!("\ncmp {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::IAnd(v1, v2) => {
            format!("\nand {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::ISar(v1, v2) => {
            format!("\nsar {}, {}", val_to_str(v1), val_to_str(v2))
        },
        Instr::INeg(v) => {
            format!("\nneg {}", val_to_str(v))
        },
        Instr::IJe(v) => {
            format!("\nje {}", val_to_str(v))
        },
        Instr::IJne(v) => {
            format!("\njne {}", val_to_str(v))
        },
        Instr::IJl(v) => {
            format!("\njl {}", val_to_str(v))
        },
        Instr::IJge(v) => {
            format!("\njge {}", val_to_str(v))
        },
        Instr::IJo(v) => {
            format!("\njo {}", val_to_str(v))
        },
        Instr::IJz(v) => {
            format!("\njz {}", val_to_str(v))
        },
        Instr::IJnz(v) => {
            format!("\njnz {}", val_to_str(v))
        },
        Instr::ISete(v) => {
            format!("\nsete {}", val_to_str(v))
        },
        Instr::ISetg(v) => {
            format!("\nsetg {}", val_to_str(v))
        },
        Instr::ISetge(v) => {
            format!("\nsetge {}", val_to_str(v))
        },
        Instr::ISetl(v) => {
            format!("\nsetl {}", val_to_str(v))
        },
        Instr::ISetle(v) => {
            format!("\nsetle {}", val_to_str(v))
        },
        Instr::IJmp(v) => {
            format!("\njmp {}", val_to_str(v))
        },
        Instr::Label(s) => {
            format!("\n{s}:")
        },
        Instr::IPush(v) => {
            format!("\npush {}", val_to_str(v))
        },
        Instr::IPop(v) => {
            format!("\npop {}", val_to_str(v))
        },
        Instr::ICall(v) => {
            format!("\ncall {}", val_to_str(v))
        },
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(Reg::RAX) => "rax".to_string(),
        Val::Reg(Reg::RSP) => "rsp".to_string(),
        Val::Reg(Reg::RDI) => "rdi".to_string(),
        Val::Reg(Reg::RBX) => "rbx".to_string(),
        Val::Reg(Reg::RCX) => "rcx".to_string(),
        Val::Reg(Reg::AL) => "al".to_string(),
        Val::Reg(Reg::R15) => "r15".to_string(),
        Val::Imm(n) => n.to_string(),
        Val::RegOffset(Reg::RSP, offset) => format!("[rsp+{}]", offset),
        Val::RegOffset(Reg::RAX, offset) => format!("[rax+{}]", offset),
        Val::RegOffset(Reg::RBX, offset) => format!("[rbx+{}]", offset),
        Val::RegOffset(Reg::RCX, offset) => format!("[rcx+{}]", offset),
        Val::RegOffset(Reg::R15, offset) => format!("[r15+{}]", offset),
        Val::RegOffset(Reg::RDI, offset) => format!("[rdi+{}]", offset),
        Val::RegOffset(Reg::AL, offset) => format!("[al+{}]", offset),
        Val::Label(s) => s.to_string(),
    }
}

fn compile_definition(d: &Definition, label: &mut i32, func_table: &HashMap<String, i32>) -> String {
    match d {
        Definition::Fun(name, args, body) => {
            let depth = aligned_depth(body, args.len() as i64);
            let offset = depth * 8;
            let mut body_env = HashMap::new();
            for (i, arg) in args.iter().enumerate() {
                body_env.insert(arg.to_string(), (depth+(i as i64)+1)*8);
            }

            let mut body_instrs: Vec<Instr> = Vec::new();
            compile_expr(&mut body_instrs, body, 0, &body_env, &String::from(""), label, false, func_table);
            let mut body_string = String::new();
            for instruction in body_instrs {
                body_string.push_str(&instr_to_str(&instruction));
            }

            format!(
                "
{name}:
sub rsp, {offset}
{body_string}
add rsp, {offset}
ret
"
            )
        }
    }
}

fn compile_main(e: &Expr, label: &mut i32, func_table: &HashMap<String, i32>) -> String {
    // convert expression to Instructions
    let mut main_instrs: Vec<Instr> = Vec::new();
    let depth = aligned_depth(e, 0);
    let offset = depth * 8;
    // move rsp to the max possible top of stack
    main_instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset)));
    compile_expr(&mut main_instrs, e, 0, &HashMap::new(), &String::from(""), label, true, func_table);
    main_instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset))); // move rsp back
    
    // convert instructions to string
    let mut main_string = String::new();
    for instruction in main_instrs {
        main_string.push_str(&instr_to_str(&instruction));
    }
    main_string
}

fn compile(p: &Program) -> (String, String) {
    let mut label = 0;

    // compile definitions
    let mut defs = String::new();
    for def in &p.defs[..] {
        defs.push_str(&compile_definition(def, &mut label, &p.function_table));
    }

    // compile main
    let main = compile_main(&p.main, &mut label, &p.function_table);

    (defs, main)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];
    
    // You will make result hold the result of actually compiling
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let prog = "(".to_owned() + &in_contents + ")";

    let prog = parse_program(&parse(&prog).unwrap_or_else(|_| panic!("Invalid")));
    let (defs, main_func) = compile(&prog);

    let asm_program = format!(
"section .text
global our_code_starts_here
extern snek_error
extern snek_print
throw_error:
  mov rdi, rcx
  push rsp
  call snek_error
  ret
{}
our_code_starts_here:
  mov r15, rsi
  {}
  ret
",
        defs,
        main_func
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}