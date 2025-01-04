use parser::parser::Parser;

mod expressions;
mod parser;
mod runtime;

fn main() {
    let mut parser = match Parser::new(String::from("./test.v")) {
        Ok(parser) => parser,
        Err(err) => panic!("error: error parseing file: {}", err),
    };

    let module = match parser.parse_file() {
        Ok(module) => module,
        Err(err) => {
            eprintln!("{}", err.message);
            std::process::exit(1);
        }
    };

    println!("{}", module.to_json());
}
