#![feature(rustc_private, slice_concat_ext, slice_extras, slice_splits)]

#![feature(plugin)]
#![plugin(clippy)]

#![allow(single_match)]

extern crate arena;

#[cfg(test)]
extern crate quickcheck;

extern crate libc;
extern crate rustc_llvm as llvm;

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

fn make_src(ctx: &mut Context, f: &str)  {
    let path = std::path::Path::new(f);
    let mut hw_file = std::fs::File::open(&path).unwrap();
    let mut data = "".to_owned();
    hw_file.read_to_string(&mut data).ok();

    let src = Source::create(ctx, f.to_owned(), &data[..]);

    println!("parsing {}...", f);

    ctx.srcs[src].ast = Some(parser::parse(ctx, &ctx.srcs[src]));
}

fn main() {
    let mut ctx = Context::new();

    make_src(&mut ctx, "core.st");
    make_src(&mut ctx, "test.txt");

    if ctx.failed() {
        return;
    }

    let parents = resolution::run(&mut ctx);

    if ctx.failed() {
        return;
    }

    let map = node_map::create(&ctx);

    println!("inferring...");
    
    infer::run(&ctx, &map, parents);
}
