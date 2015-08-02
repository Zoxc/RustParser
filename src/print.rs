use misc::Context;
use ast::*;

fn ident(ctx: &Context, i: Ident) -> String {
	ctx.get_name(i.0.val)
}

fn do_block<T>(ctx: &Context, block: &Block_<N<T>>, f: fn(&Context, &N<T>) -> String) -> String {
	if block.val.vals.is_empty() {
		return "".to_owned();
	}

	let r: &str = &block.val.vals.iter().map(|e| f(ctx, &e)).collect::<Vec<String>>().join("\n");

	let mut s = "\n".to_owned();
	s.push_str(&r.lines().map(|l| format!("    {}", l)).collect::<Vec<String>>().join("\n"));

	s
}

fn block(ctx: &Context, block: &Block_<Expr_>) -> String {
	do_block(ctx, block, expr)
}

fn generics(ctx: &Context, generics: &Generics) -> String {
	if generics.params.is_empty() {
		"".to_owned()
	} else {
		let p = generics.params.iter().map(|p| format!("{}({})", ident(ctx, p.val.name), p.info.id.0)).collect::<Vec<String>>().join(", ");
		format!("[{}]", p)
	}
}

fn substs(ctx: &Context, s: &Option<Vec<Ty_>>) -> String {
	match *s {
		Some(ref v) => {
			let p = v.iter().map(|t| ty(ctx, t)).collect::<Vec<String>>().join(", ");
			format!("[{}]", p)
		}
		None => "".to_owned()
	}
}

pub fn item_block(ctx: &Context, block: &Block_<Item_>) -> String {
	do_block(ctx, block, item)
}

fn expr(ctx: &Context, e: &Expr_) -> String {
	match e.val {
		Expr::Num(n) => ctx.get_num(n),
		Expr::Call(ref obj, ref args) => {
			let a = args.iter().map(|e| expr(ctx, e)).collect::<Vec<String>>().join(", ");
			format!("{}({})", expr(ctx, obj), a)
		} 
		Expr::Ref(i, _, ref s) => format!("{}{}", ident(ctx, i), substs(ctx, s)),
		Expr::If(ref cond, ref then, ref otherwise) => {
			let mut r = format!("if ({}){}", expr(ctx, cond), block(ctx, then));
			if let Some(ref v) = *otherwise {
				r.push_str(" else ");
				r.push_str(&expr(ctx, &*v));
			};
			r
		},
		Expr::Assign(op, ref lhs, ref rhs) => format!("({} {} {})", expr(ctx, lhs), ctx.get_op(op), expr(ctx, rhs)),
		Expr::BinOp(ref lhs, op, ref rhs) => format!("({} {} {})", expr(ctx, lhs), ctx.get_op(op), expr(ctx, rhs)),
		Expr::UnaryOp(op, ref e) => format!("({}{})", ctx.get_op(op), expr(ctx, e)),
		Expr::Return(ref ret) => match *ret {
			Some(ref e) => format!("return ({})", expr(ctx, &e)),
			None => format!("return")
		},
		Expr::Loop(ref b) => format!("loop{}", block(ctx, b)),
		Expr::Break => format!("break"),
		Expr::Block(ref b) => block(ctx, b),
		Expr::Error => format!("<error>"),
	}
}

fn ty(ctx: &Context, t: &Ty_) -> String {
	match t.val {
		Ty::Error => format!("<error>"),
		Ty::Ptr(ref t) => format!("{}*", ty(ctx, t)),
		Ty::Infer => "_".to_owned(),
		Ty::Ref(i, _, ref s) => format!("{}{}", ident(ctx, i), substs(ctx, s)),
	}
}

fn fn_param(ctx: &Context, p: &FnParam_) -> String {
	match p.val.1.val {
		Ty::Infer => ident(ctx, p.val.0),
		_ => format!("{} {}", ty(ctx, &p.val.1), ident(ctx, p.val.0))
	}
}

pub fn item(ctx: &Context, e: &Item_) -> String {
	match e.val {
		Item::Case(i, ref b) => format!("when {}{}", ident(ctx, i), item_block(ctx, b)),
		Item::Data(i, ref g, ref b) => format!("data {}{}{}", ident(ctx, i), generics(ctx, g), item_block(ctx, b)),
		Item::Fn(ref d) => format!("fn {}{}({}){}", ident(ctx, d.name), generics(ctx, &d.generics), d.params.iter().map(|param| fn_param(ctx, param)).collect::<Vec<String>>().join(", "), block(ctx, &d.block)),
	}
}
