#[cfg(test)]
extern crate quickcheck;

use std::rc::Rc;
use std::io::Read;
use misc::{Context, Source};

mod ast;
mod print;
mod misc;
mod interner;
mod lexer;
mod parser;
mod declare;
mod resolution;

fn main() {
    let path = std::path::Path::new("test.txt");
    let mut hw_file = std::fs::File::open(&path).unwrap();
    let mut data = "".to_string();
    hw_file.read_to_string(&mut data).ok();

	let src = Source::new(Rc::new(Context::new()), "input".to_string(), &data[..]);

    let mut ast = parser::parse(&src);

    declare::run(&mut ast);
    resolution::run(&src, &mut ast);

	print!("{}", src.format_msgs());
}
