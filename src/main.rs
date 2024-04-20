use std::env;
use std::fs;

mod ast;
mod lexer;
mod parser;
mod typechecker;
mod interpreter;
mod compiler;

use lexer::Lexer;
use parser::Parser;
use typechecker::TypeChecker;
use interpreter::Interpreter;
use compiler::Compiler;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("Could not read file");

    let mut lexer = Lexer::new(&contents);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    let mut type_checker = TypeChecker::new();
    match type_checker.check_program(&program) {
        Ok(_) => println!("Type checking passed"),
        Err(e) => {
            eprintln!("Type checking failed: {:?}", e);
            std::process::exit(1);
        }
    }

    let mut interpreter = Interpreter::new();
    match interpreter.interpret(&program) {
        Ok(value) => println!("Program executed successfully. Result: {:?}", value),
        Err(e) => {
            eprintln!("Execution failed: {}", e);
            std::process::exit(1);
        }
    }

    let mut compiler = Compiler::new();
    match compiler.compile(&program) {
        Ok(code) => {
            println!("Compilation successful. Generated code:");
            println!("{}", code);
        }
        Err(e) => {
            eprintln!("Compilation failed: {:?}", e);
            std::process::exit(1);
        }
    }
}