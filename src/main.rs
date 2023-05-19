
use std::{process::ExitCode, rc::Rc, time::Instant};

mod compiler;
mod operator;
mod runtime;
mod syntax;
mod token;
mod utils;

/// todo token/syntax: label, with, regexp, generators, class, import, export, for await, {await prop() {}}, template literals, hashbang, bigint
/// todo compile/runtime: arguments, standard library, assignment, patterns, for, break, continue, switch, case, exceptions, captures, bigint

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
    let script = crate::syntax::Script::parse(&mut iter);

    println!("{:#?}", script);
    println!("Erros: {:#?}", iter.errors());

    if !iter.errors().is_empty() {
        println!("Houveram erros de syntaxe");
        return ExitCode::FAILURE;
    }

    let program = match crate::compiler::compile(&script) {
        Ok(program) => Rc::new(program),
        Err(errors) =>  {
            println!("Erros: {:#?}", errors);
            println!("Houveram erros na compilação");
            return ExitCode::FAILURE;
        }
    };

    println!("{:#?}", program);

    let start = Instant::now();
    let output = crate::runtime::execute(program.into());
    let duration = start.elapsed();

    println!("O programa executou em: {:?}", duration);
    println!("O programa retornou: {:#?}", output);

    ExitCode::SUCCESS
}
