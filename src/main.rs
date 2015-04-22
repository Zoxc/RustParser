#[cfg(test)]
extern crate quickcheck;

mod ast;
mod misc;
mod interner;
mod lexer;
mod parser;
use std::io::Read;

fn main() {
    let path = std::path::Path::new("test.txt");
    let mut hw_file = std::fs::File::open(&path).unwrap();
    let mut data = "".to_string();
    hw_file.read_to_string(&mut data).ok();
    parser::parse(&data[..]);
}
