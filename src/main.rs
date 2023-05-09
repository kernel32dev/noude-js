
use std::process::ExitCode;

mod syntax;
mod token;
mod utils;

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        println!("Usage: noude file_name");
        return ExitCode::FAILURE;
    }

    let file_name = &args[1];

    let contents = match std::fs::read_to_string(file_name) {
        Ok(s) => s,
        Err(e) => {
            println!("Error reading file {}: {}", file_name, e);
            return ExitCode::FAILURE;
        }
    };

    let mut iter = crate::token::TokenIter::new(contents.as_str());
    let program = crate::syntax::Program::parse(&mut iter);

    println!("{:#?}", program);
    println!("Erros: {:#?}", iter.errors());

    if iter.errors().is_empty() {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
