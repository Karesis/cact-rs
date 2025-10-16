use std::{env, fs::read_to_string};

use cactc_core::driver;

fn main() {
    match env::current_dir() {
        Ok(path) => {
            println!("[DEBUG]The current dir is: {path:?}");
        }
        Err(e) => {
            println!("[DEGUG]Error when getting the current dir: {e}");
        }
    }
    let args: Vec<String> = env::args().collect();
    let path = &args[1];
    let source = read_to_string(path).expect("No such file.");
    let source = source.as_str();
    driver(source);
}