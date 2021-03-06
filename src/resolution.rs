use misc::{Context, Source, Msg};
use ast::*;
use std::collections::HashMap;
use std::cell::RefCell;

struct ResolutionPass<'c> {
	declare: bool,
	src: &'c Source,
	id: Option<Id>,
	parent: Option<&'c ResolutionPass<'c>>,
	parents: &'c RefCell<HashMap<Id, Id>>,
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
			self.id.map(|parent| self.parents.borrow_mut().insert(id, parent));
			self.symbols.name(id, ident);
		}
	}

	fn generics(&mut self, generics: &mut Generics) {
		for p in generics.params.iter_mut() {
			self.name(p.info.id, p.val.name);
		}
	}

	fn wrap(&'c self, symbols: &'c mut SymbolTable, id: Id) -> ResolutionPass<'c> {
		ResolutionPass { declare: self.declare, src: self.src, id: Some(id), parent: Some(&self), parents: self.parents, symbols: symbols }
	}

	fn item(&mut self, val: &mut Item_) {
		match val.val {
			Item::Case(i, ref mut b) => {
				self.name(val.info.id, i);

				let mut pass = self.wrap(&mut b.val.symbols, val.info.id);
				pass.items(&mut b.val.vals);
			}
			Item::Data(i, ref mut g, ref mut b) => {
				self.name(val.info.id, i);

				let mut pass = self.wrap(&mut b.val.symbols, val.info.id);
				pass.items(&mut b.val.vals);
				pass.generics(g);
			}
			Item::Fn(ref mut def) => {
				self.name(val.info.id, def.name);
				let mut pass = self.wrap(&mut def.block.val.symbols, val.info.id);
				pass.generics(&mut def.generics);
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
			Ty::Ref(ident, ref mut id, ref mut s) => self.fold_ref(ident, id, s),
		};
	}

	fn fold_ref(&mut self, ident: Ident, id: &mut Id, s: &mut Option<Vec<Ty_>>) {
		if self.declare {
			return;
		}

		match self.lookup(ident) {
			Some(node) => *id = node,
			None => self.src.msg(ident.0.span, Msg::Resolution(ident.0.val)),
		}

		s.as_mut().map(|v| v.iter_mut().map(|t| self.ty(t)));
	}

	fn expr(&mut self, val: &mut Expr_) {
		match val.val {
			Expr::Break | Expr::Error | Expr::Num(_) => (),
			Expr::Call(ref mut obj, ref mut args) => {
				self.expr(obj);
				self.exprs(args);
			}
			Expr::Ref(ident, ref mut id, ref mut s) => self.fold_ref(ident, id, s),
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
			Expr::Return(ref mut ret) => {ret.as_mut().map(|e| self.expr(e));},
			Expr::Block(ref mut b) => self.expr_block(b),
			Expr::Loop(ref mut b) => self.expr_block(b),
		};
	}

	fn expr_block(&mut self, block: &mut Block_<Expr_>) {
		let mut pass = self.wrap(&mut block.val.symbols, self.id.unwrap());
		pass.exprs(&mut block.val.vals);
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

pub fn run(ctx: &mut Context) -> HashMap<Id, Id> {
	let parents = RefCell::new(HashMap::new());
	let mut globals = SymbolTable::new();

	for src in ctx.srcs.iter_mut() {
		let mut block = src.ast.take().unwrap();
		{
			let mut declare_pass = ResolutionPass { declare: true, src: src, id: None, parent: None, parents: &parents, symbols: &mut globals };
			declare_pass.items(&mut block.val.vals);
		}
		src.ast = Some(block);
	}

	for src in ctx.srcs.iter_mut() {
		let mut block = src.ast.take().unwrap();
		{
			let mut lookup_pass = ResolutionPass { declare: false, src: src, id: None, parent: None, parents: &parents, symbols: &mut globals };
			lookup_pass.items(&mut block.val.vals);
		}
		src.ast = Some(block);
	}

	ctx.srcs[0].ast.as_mut().unwrap().val.symbols = globals;

	parents.into_inner()
}