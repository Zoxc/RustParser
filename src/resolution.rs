use misc::{Source, Msg};
use ast::*;

struct ResolutionPass<'c> {
	declare: bool,
	src: &'c Source,
	parent: Option<&'c ResolutionPass<'c>>,
	symbols: &'c mut SymbolTable,
}

impl<'c> ResolutionPass<'c> {
	fn lookup(&self, ident: Ident) -> Option<Id> {
		self.symbols.get(ident).or_else(|| {
			match self.parent {
				Some(parent) => parent.lookup(ident),
				None => None
			}
		})
	}

	fn name(&mut self, id: Id, ident: Ident) {
		if self.declare {
			self.symbols.name(id, ident);
		}
	}

	fn generics(&mut self, _generics: &mut Generics) {

	}

	fn wrap(&'c self, symbols: &'c mut SymbolTable) -> ResolutionPass<'c> {
		ResolutionPass { declare: self.declare, src: self.src, parent: Some(&self), symbols: symbols }
	}

	fn item(&mut self, val: &mut Item_) {
		match val.val {
			Item::Data(i, ref mut g, ref mut b) => {
				self.name(val.info.id, i);

				let mut pass = self.wrap(&mut b.val.symbols);
				pass.items(&mut b.val.vals);
				pass.generics(g);
			}
			Item::Fn(ref mut def) => {
				self.name(val.info.id, def.name);
				let mut pass = self.wrap(&mut def.block.val.symbols);
				for param in def.params.iter_mut() {
					pass.name(param.info.id, param.val.0);
					pass.ty(&mut param.val.1);
				}
				pass.exprs(&mut def.block.val.vals);
			}
		};
	}

	fn ty(&mut self, val: &mut Ty_) {
		if self.declare {
			return;
		}

		match val.val {
			Ty::Infer => (),
			Ty::Error => (),
			Ty::Ptr(ref mut t) => self.ty(t), 
			Ty::Ref(ident, ref mut id) => self.fold_ref(ident, id),
		};
	}

	fn fold_ref(&mut self, ident: Ident, id: &mut Id) {
		if self.declare {
			return;
		}

		match self.lookup(ident) {
			Some(node) => *id = node,
			None => self.src.msg(ident.0.span, Msg::Resolution(ident.0.val)),
		}
	}

	fn expr(&mut self, val: &mut Expr_) {
		match val.val {
			Expr::Break => (),
			Expr::Error => (),
			Expr::Ref(ident, ref mut id) => self.fold_ref(ident, id),
			Expr::If(ref mut cond, ref mut then, ref mut otherwise) => {
				self.expr(cond);
				self.expr_block(then);
				if let Some(ref mut v) = *otherwise {
					self.expr(v);
				};
			},
			Expr::Assign(_, ref mut lhs, ref mut rhs) => {
				self.expr(lhs);
				self.expr(rhs);
			} 
			Expr::UnaryOp(_, ref mut e) => {
				self.expr(e);
			} 
			Expr::BinOp(ref mut lhs, _, ref mut rhs) => {
				self.expr(lhs);
				self.expr(rhs);
			} 
			Expr::Return(ref mut ret) => self.expr(ret),
			Expr::Block(ref mut b) => self.expr_block(b),
			Expr::Loop(ref mut b) => self.expr_block(b),
		};
	}

	fn expr_block(&mut self, block: &mut Block_<Expr_>) {
		let mut pass = self.wrap(&mut block.val.symbols);
		for v in block.val.vals.iter_mut() {
			pass.expr(v);
		}
	}

	fn exprs(&mut self, vals: &mut Vec<Expr_>) {
		for v in vals.iter_mut() {
			self.expr(v);
		}
	}

	fn items(&mut self, vals: &mut Vec<Item_>) {
		for v in vals.iter_mut() {
			self.item(v);
		}
	}
}

pub fn run(src: &Source, block: &mut Block_<Item_>) {
	{
		let mut declare_pass = ResolutionPass { declare: true, src: src, parent: None, symbols: &mut block.val.symbols };
		declare_pass.items(&mut block.val.vals);
	}
	{
		let mut lookup_pass = ResolutionPass { declare: false, src: src, parent: None, symbols: &mut block.val.symbols };
		lookup_pass.items(&mut block.val.vals);
	}
}