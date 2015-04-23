use lexer::{Span, Spanned};
use misc::{Name, Source};
use std::collections::HashMap;

#[derive(Clone)]
pub struct SymbolTable {
	map: HashMap<Name, Id>,
}

impl SymbolTable {
	pub fn new() -> SymbolTable {
		SymbolTable {
			map: HashMap::new(),
		}
	}

	pub fn name(&mut self, id: Id, ident: Ident) {
		self.map.insert(ident.0.val, id);
	}

	pub fn get(&self, ident: Ident) -> Option<Id> {
		self.map.get(&ident.0.val).map(|v| *v)
	}
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct Id(pub u32);

pub const NONE: Id = Id(0);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Info {
	pub id: Id,
	pub span: Span,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct N<T> {
	pub val: T,
	pub info: Info,
}

impl<T> N<T> {
	pub fn new(src: &Source, span: Span, val: T) -> N<T> {
		N {
			val: val,
			info: Info {
				id: src.ctx.new_id(),
				span: span,
			},
		}
	}
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Ident(pub Spanned<Name>);

#[derive(Clone)]
pub struct Block<T> {
	pub symbols: SymbolTable,
	pub vals: Vec<T>,
}

impl<T> Block<T> {
	pub fn new() -> Block<T> {
		Block {
			symbols: SymbolTable::new(),
			vals: Vec::new()
		}
	}
}

pub type Block_<T> = N<Block<T>>;

pub type Item_ = N<Item>;

#[derive(Clone)]
pub enum Item {
	Data(Ident, Block_<N<Item>>),
	Fn(Ident, Vec<Ident>, Block_<N<Expr>>),
}

pub type Expr_ = N<Expr>;

#[derive(Clone)]
pub enum Expr {
	Error,
	If(Box<N<Expr>>, Block_<N<Expr>>, Option<Box<N<Expr>>>),
	Return(Box<N<Expr>>),
	Block(Block_<N<Expr>>),
	Ref(Ident, Id),
}

pub enum Lookup<'c> {
	Item(&'c Item_),
	Expr(&'c Expr_),
}

pub mod fold {
	use super::*;

	pub fn fold_expr_block<T: Folder>(this: &mut T, block: &mut Block_<Expr_>) {
		fold_exprs(this, &mut block.val.vals);
	}

	pub fn fold_exprs<T: Folder>(this: &mut T, vals: &mut Vec<Expr_>) {
		for v in vals.iter_mut() {
			this.fold_expr(v);
		}
	}

	pub fn fold_item_block<T: Folder>(this: &mut T, block: &mut Block_<Item_>) {
		fold_items(this, &mut block.val.vals);
	}

	pub fn fold_items<T: Folder>(this: &mut T, vals: &mut Vec<Item_>) {
		for v in vals.iter_mut() {
			this.fold_item(v);
		}
	}
}

pub trait Folder: Sized {
	fn fold_expr_block(&mut self, block: &mut Block_<Expr_>) {
		fold::fold_expr_block(self, block);
	}

	fn fold_item_block(&mut self, block: &mut Block_<Item_>) {
		fold::fold_item_block(self, block);
	}

	fn fold_data(&mut self, _info: Info, _name: Ident, block: &mut Block_<Item_>) {
		fold::fold_item_block(self, block);
	}

	fn fold_fn(&mut self, _info: Info, _name: Ident, _params: &mut Vec<Ident>, block: &mut Block_<Expr_>) {
		fold::fold_expr_block(self, block);
	}

	fn fold_item(&mut self, val: &mut Item_) {
		match val.val {
			Item::Data(i, ref mut b) => self.fold_data(val.info, i, b),
			Item::Fn(i, ref mut p, ref mut b) => self.fold_fn(val.info, i, p, b)
		};
	}

	fn fold_ref(&mut self, _ident: Ident, _id: &mut Id) {
	}

	fn fold_expr(&mut self, val: &mut Expr_) {
		match val.val {
			Expr::Error => (),
			Expr::Ref(ident, ref mut id) => self.fold_ref(ident, id),
			Expr::If(ref mut cond, ref mut then, ref mut otherwise) => {
				self.fold_expr(cond);
				self.fold_expr_block(then);
				if let Some(ref mut v) = *otherwise {
					self.fold_expr(v);
				};
			},
			Expr::Return(ref mut ret) => self.fold_expr(ret),
			Expr::Block(ref mut b) => self.fold_expr_block(b),
		};
	}
}