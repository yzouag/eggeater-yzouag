use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64, memory : *mut i64) -> u64;
}

#[no_mangle]
#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    if errcode == 7 {
        eprintln!("overflow");
        std::process::exit(1);
    } else if errcode == 3 {
        eprintln!("invalid argument");
        std::process::exit(1);
    } else if errcode == 8 {
        eprintln!("index out of bound");
        std::process::exit(1);
    } else {
        eprintln!("an error ocurred {errcode}");
        std::process::exit(1);
    }
}

fn snek_str(val: i64, seen: &mut Vec<i64>) -> String {
    if val == 7 {
        "true".to_string()
    } else if val == 3 {
        "false".to_string()
    } else if val % 2 == 0 {
        format!("{}", val >> 1)
    } else if val == 1 {
        "nil".to_string()
    } else if val & 1 == 1 {
        if seen.contains(&val)  { return "(...)".to_string() }
        seen.push(val);
        let addr = (val - 1) as *const i64;
        let length = unsafe { *addr };
        let mut list_string = String::from("(");
        for i in 1..length {
            let element_addr = unsafe { *addr.offset(i as isize) };
            list_string.push_str(&snek_str(element_addr, seen));
            list_string.push_str(", ");
        }
        let element_addr = unsafe { *addr.offset(length as isize) };
        list_string.push_str(&snek_str(element_addr, seen));
        list_string.push(')');
        seen.pop();
        list_string
    } else {
        format!("Unknown value: {}", val)
    }
}

#[no_mangle]
#[export_name = "\x01snek_print"]
pub extern "C" fn snek_print(val: u64) -> u64 {
    let mut seen = Vec::<i64>::new();
    println!("{}", snek_str(val as i64, &mut seen));
    return val;
}

fn parse_input(input: &str) -> u64 {
    if input == "true" {
        return 7
    } else if input == "false" {
        return 3
    } else if input == "nil" {
        return 1
    } else {
        match input.parse::<i64>() {
            Ok(n) => {
                match n.checked_mul(2) {
                    Some(num) => num as u64,
                    None => panic!("overflow"),
                }
            },
            Err(_) => {
                eprintln!("input is not valid");
                std::process::exit(1);
            },
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let mut memory = Vec::<i64>::with_capacity(1000000);
    let buffer :*mut i64 = memory.as_mut_ptr();

    let i: u64 = unsafe { our_code_starts_here(input, buffer) };
    snek_print(i);
}
