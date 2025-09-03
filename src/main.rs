use std::{borrow::BorrowMut, env};

use parser::parser::Parser;
use runtime::{interpreter::RuntimeScope, static_analysis::{StaticAnalysis, StaticAnalysisScope}};
// use runtime::interpreter::{Runtime, RuntimeScope};

mod expressions;
mod parser;
mod runtime;

fn main() {
    let mut args: Vec<String> = env::args().collect();
    args.remove(0);

    for arg in args {
        let mut parser = match Parser::new(String::from(&arg)) {
            Ok(parser) => parser,
            Err(err) => {
                eprintln!("error: error opening file {}: {}", arg, err);
                continue;
            }
        };

        let mut module = match parser.parse_file() {
            Ok(module) => module,
            Err(err) => {
                eprintln!("{}", err.message);
                std::process::exit(1);
            }
        };

        println!("{}", module.to_json());
        
        eprintln!("executing...");

        module.static_analysis(StaticAnalysisScope::new().borrow_mut());
    }
}
