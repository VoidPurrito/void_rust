use parser::parser::Parser;

mod expressions;
mod parser;

fn main() {
    let mut parser = match Parser::new(String::from("./test.mn")) {
        Ok(parser) => parser,
        Err(err) => panic!("error: error parseing file: {}", err),
    };
        
    match parser.parse_file() {
        Ok(module) => println!("{}", module.to_json()),
        Err(err) => {
            eprintln!("{}", err.message);
            std::process::exit(1);
        }
    };
}
