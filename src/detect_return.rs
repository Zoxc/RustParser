use ast::*;

struct DetectReturn {
	found: bool,
}

impl<'c> Folder<'c> for DetectReturn {
	fn fold_expr(&mut self, val: &'c Expr_) {
		match val.val {
			Expr::Return(_) => self.found = true,
			_ => ()
		};

		if self.found == true {
			return;
		}

		fold::fold_expr(self, val);
	}
}

pub fn run<'c>(block: &'c Block_<Expr_>) -> bool {
	let mut pass = DetectReturn { found: false };
	pass.fold_expr_block(block);
	pass.found
}