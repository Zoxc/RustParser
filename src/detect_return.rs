use ast::*;

struct DetectReturn {
	found: bool,
}

impl<'c> Visitor<'c> for DetectReturn {
	fn visit_expr(&mut self, val: &'c Expr_) {
		if self.found == true {
			return;
		}
		if let Expr::Return(..) = val.val {
			self.found = true;
			return;
		}
		visit::visit_expr(self, val);
	}
}

pub fn run<'c>(block: &'c Block_<Expr_>) -> bool {
	let mut pass = DetectReturn { found: false };
	pass.visit_expr_block(block);
	pass.found
}