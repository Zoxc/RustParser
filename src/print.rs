use misc::Source;
use ast::*;

fn ident(src: &Source, i: Ident) -> String {
	src.get_name(i.0.val)
}

fn do_block<T>(src: &Source, block: &Block_<N<T>>, f: fn(&Source, &N<T>) -> String) -> String {
	if block.val.vals.is_empty() {
		return "".to_string();
	}

	let r: &str = &block.val.vals.iter().map(|e| f(src, &e)).collect::<Vec<String>>().connect("\n");

	let mut s = "\n".to_string();
	s.push_str(&r.lines().map(|l| format!("    {}", l)).collect::<Vec<String>>().connect("\n"));

	s
}

fn block(src: &Source, block: &Block_<Expr_>) -> String {
	do_block(src, block, expr)
}

pub fn item_block(src: &Source, block: &Block_<Item_>) -> String {
	do_block(src, block, item)
}

fn expr(src: &Source, e: &Expr_) -> String {
	match e.val {
		Expr::Ref(i, _) => ident(src, i),
		Expr::If(ref cond, ref then, ref otherwise) => {
			let mut r = format!("if ({}){}", expr(src, cond), block(src, then));
			if let Some(ref v) = *otherwise {
				r.push_str(&expr(src, &*v));
			};
			r
		},
		Expr::Assign(op, ref lhs, ref rhs) => format!("({} {} {})", expr(src, lhs), src.get_op(op), expr(src, rhs)),
		Expr::BinOp(ref lhs, op, ref rhs) => format!("({} {} {})", expr(src, lhs), src.get_op(op), expr(src, rhs)),
		Expr::UnaryOp(op, ref e) => format!("({}{})", src.get_op(op), expr(src, e)),
		Expr::Return(ref ret) => format!("return ({})", expr(src, ret)),
		Expr::Loop(ref b) => format!("loop{}", block(src, b)),
		Expr::Break => format!("break"),
		Expr::Block(ref b) => block(src, b),
		Expr::Error => format!("<error>"),
	}
}

fn ty(src: &Source, t: &Ty_) -> String {
	match t.val {
		Ty::Error => format!("<error>"),
		Ty::Ptr(ref t) => format!("{}*", ty(src, t)),
		Ty::Infer => "_".to_string(),
		Ty::Ref(i, _) => ident(src, i),
	}
}

fn fn_param(src: &Source, p: &FnParam_) -> String {
	match p.val.1.val {
		Ty::Infer => ident(src, p.val.0),
		_ => format!("{} {}", ty(src, &p.val.1), ident(src, p.val.0))
	}
}

pub fn item(src: &Source, e: &Item_) -> String {
	match e.val {
		Item::Data(i, ref b) => format!("data {}{}", ident(src, i), item_block(src, b)),
		Item::Fn(i, ref p, ref b) => format!("fn {}({}){}", ident(src, i), p.iter().map(|param| fn_param(src, param)).collect::<Vec<String>>().connect(", "), block(src, b)),
	}
}
