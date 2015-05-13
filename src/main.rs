#![feature(rustc_private, collections, std_misc)]

extern crate arena;

#[cfg(test)]
extern crate quickcheck;

extern crate rustc_llvm as llvm;

use std::rc::Rc;
use std::io::Read;
use misc::{Context, Source};

mod ast;
mod print;
mod misc;
mod interner;
mod lexer;
mod parser;
mod resolution;
mod node_map;
mod ty;
mod detect_return;
mod infer;
mod recursion;
mod codegen;

fn main() {
    let path = std::path::Path::new("test.txt");
    let mut hw_file = std::fs::File::open(&path).unwrap();
    let mut data = "".to_string();
    hw_file.read_to_string(&mut data).ok();

	let src = Source::new(Rc::new(Context::new()), "input".to_string(), &data[..]);

    println!("parsing...");

    let mut ast = parser::parse(&src);

    if src.has_msgs() {
        print!("{}", src.format_msgs());
        return;
    }

    resolution::run(&src, &mut ast);

    if src.has_msgs() {
        print!("{}", src.format_msgs());
        return;
    }

    let map = node_map::create(&ast);

    println!("inferring...");
    
    infer::run(&src, &ast, &map);

	print!("{}", src.format_msgs());
}
