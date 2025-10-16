#![allow(dead_code)]
mod interner;
mod lexer;
mod span;
mod errors;
mod ast;
mod parser;
mod reporter;
mod driver;

pub use driver::driver;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn sptest() {
        use std::{env, fs::read_to_string};

        match env::current_dir() {
            Ok(path) => {
                println!("[DEBUG]The current dir is: {path:?}");
            }
            Err(e) => {
                println!("[DEGUG]Error when getting the current dir: {e}");
            }
        }
        let source = read_to_string("../tests/all_tests_in.cact").expect("No such file.");
        let source = source.as_str();
        driver(source);

    }
}