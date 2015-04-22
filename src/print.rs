use lexer::{Span, Spanned};
use misc::{Source, Name};
use ast::*;

fn ident(src: &Source, i: Ident) -> String {
	src.ctx.interners.name.get(i.0.val).to_string()
}

fn block(src: &Source, b: &Block<Expr_>) -> String {
	let mut s = "".to_string();

	b.as_ref().map(|block| {
		let r: &str = &block.val.iter().map(|e| expr(src, &e)).collect::<Vec<String>>().connect("\n");

		s = "\n".to_string();
		s.push_str(&r.lines().map(|l| format!("    {}", l)).collect::<Vec<String>>().connect("\n"));
	});

	s
}

fn expr(src: &Source, e: &Expr_) -> String {
	match e.val {
		Expr::If(ref cond, ref then, ref otherwise) => {
			let mut r = format!("if ({}){}", expr(src, cond), block(src, then));
			if let Some(ref v) = *otherwise {
				r.push_str(&expr(src, &*v));
			};
			r
		},
		Expr::Return(ref ret) => format!("return ({})", expr(src, ret)),
		Expr::Block(ref b) => block(src, b),
		Expr::Error => format!("<error>"),
	}
}

pub fn item_block(src: &Source, b: &Block<Item_>) -> String {
	let mut s = "".to_string();

	b.as_ref().map(|block| {
		let r: &str = &block.val.iter().map(|e| item(src, &e)).collect::<Vec<String>>().connect("\n");

		s = "\n".to_string();
		s.push_str(&r.lines().map(|l| format!("    {}", l)).collect::<Vec<String>>().connect("\n"));
	});

	s
}

pub fn item(src: &Source, e: &Item_) -> String {
	match e.val {
		Item::Data(i, ref b) => format!("data {}{}", ident(src, i), item_block(src, b)),
		Item::Fn(i, ref p, ref b) => format!("fn {}({}){}", ident(src, i), p.iter().map(|param| ident(src, *param)).collect::<Vec<String>>().connect(", "), block(src, b)),
	}
}
