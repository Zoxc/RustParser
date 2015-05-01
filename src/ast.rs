use lexer::{Span, Spanned};
use misc::{Name, Op, Source};
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

pub type FnParam_ = N<FnParam>;

#[derive(Clone)]
pub struct FnParam(pub Ident, pub Ty_);

pub type Item_ = N<Item>;

#[derive(Clone)]
pub enum Item {
	Data(Ident, Block_<N<Item>>),
	Fn(Ident, Vec<FnParam_>, Block_<Expr_>),
}

pub type Ty_ = N<Ty>;

#[derive(Clone)]
pub enum Ty {
	Error,
	Infer,
	Ptr(Box<Ty_>),
	Ref(Ident, Id),
}

pub type Expr_ = N<Expr>;

#[derive(Clone)]
pub enum Expr {
	Error,
	Assign(Op, Box<N<Expr>>, Box<N<Expr>>),
	BinOp(Box<N<Expr>>, Op, Box<N<Expr>>),
	UnaryOp(Op, Box<N<Expr>>),
	If(Box<N<Expr>>, Block_<N<Expr>>, Option<Box<N<Expr>>>),
	Return(Box<N<Expr>>),
	Block(Block_<N<Expr>>),
	Loop(Block_<Expr_>),
	Break,
	Ref(Ident, Id),
}

pub enum Lookup<'c> {
	Item(&'c Item_),
	Expr(&'c Expr_),
}

pub mod fold_mut {
	use super::*;

	pub fn fold_expr_block<T: FolderMut>(this: &mut T, block: &mut Block_<Expr_>) {
		fold_exprs(this, &mut block.val.vals);
	}

	pub fn fold_exprs<T: FolderMut>(this: &mut T, vals: &mut Vec<Expr_>) {
		for v in vals.iter_mut() {
			this.fold_expr(v);
		}
	}

	pub fn fold_item_block<T: FolderMut>(this: &mut T, block: &mut Block_<Item_>) {
		fold_items(this, &mut block.val.vals);
	}

	pub fn fold_items<T: FolderMut>(this: &mut T, vals: &mut Vec<Item_>) {
		for v in vals.iter_mut() {
			this.fold_item(v);
		}
	}

	pub fn fold_fn<T: FolderMut>(this: &mut T, _info: Info, _name: Ident, params: &mut Vec<FnParam_>, block: &mut Block_<Expr_>) {
		fold_fn_params(this, params);
		this.fold_expr_block(block);
	}

	pub fn fold_fn_param<T: FolderMut>(this: &mut T, param: &mut FnParam_) {
		this.fold_ty(&mut param.val.1);
	}

	pub fn fold_fn_params<T: FolderMut>(this: &mut T, vals: &mut Vec<FnParam_>) {
		for v in vals.iter_mut() {
			this.fold_fn_param(v);
		}
	}
}

pub trait FolderMut: Sized {
	fn fold_expr_block(&mut self, block: &mut Block_<Expr_>) {
		fold_mut::fold_expr_block(self, block);
	}

	fn fold_item_block(&mut self, block: &mut Block_<Item_>) {
		fold_mut::fold_item_block(self, block);
	}

	fn fold_data(&mut self, _info: Info, _name: Ident, block: &mut Block_<Item_>) {
		fold_mut::fold_item_block(self, block);
	}

	fn fold_fn(&mut self, info: Info, name: Ident, params: &mut Vec<FnParam_>, block: &mut Block_<Expr_>) {
		fold_mut::fold_fn(self, info, name, params, block);
	}

	fn fold_fn_param(&mut self, param: &mut FnParam_) {
		fold_mut::fold_fn_param(self, param);
	}

	fn fold_item(&mut self, val: &mut Item_) {
		match val.val {
			Item::Data(i, ref mut b) => self.fold_data(val.info, i, b),
			Item::Fn(i, ref mut p, ref mut b) => self.fold_fn(val.info, i, p, b)
		};
	}

	fn fold_ty(&mut self, val: &mut Ty_) {
		match val.val {
			Ty::Infer => (),
			Ty::Error => (),
			Ty::Ptr(ref mut t) => self.fold_ty(t), 
			Ty::Ref(ident, ref mut id) => self.fold_ref(ident, id),
		};
	}

	fn fold_ref(&mut self, _ident: Ident, _id: &mut Id) {
	}

	fn fold_expr(&mut self, val: &mut Expr_) {
		match val.val {
			Expr::Break => (),
			Expr::Error => (),
			Expr::Ref(ident, ref mut id) => self.fold_ref(ident, id),
			Expr::If(ref mut cond, ref mut then, ref mut otherwise) => {
				self.fold_expr(cond);
				self.fold_expr_block(then);
				if let Some(ref mut v) = *otherwise {
					self.fold_expr(v);
				};
			},
			Expr::Assign(_, ref mut lhs, ref mut rhs) => {
				self.fold_expr(lhs);
				self.fold_expr(rhs);
			} 
			Expr::UnaryOp(_, ref mut e) => {
				self.fold_expr(e);
			} 
			Expr::BinOp(ref mut lhs, _, ref mut rhs) => {
				self.fold_expr(lhs);
				self.fold_expr(rhs);
			} 
			Expr::Return(ref mut ret) => self.fold_expr(ret),
			Expr::Block(ref mut b) => self.fold_expr_block(b),
			Expr::Loop(ref mut b) => self.fold_expr_block(b),
		};
	}
}


pub mod fold {
	use super::*;

	pub fn fold_expr_block<'c, T: Folder<'c>>(this: &mut T, block: &'c Block_<Expr_>) {
		fold_exprs(this, &block.val.vals);
	}

	pub fn fold_exprs<'c, T: Folder<'c>>(this: &mut T, vals: &'c Vec<Expr_>) {
		for v in vals.iter() {
			this.fold_expr(v);
		}
	}

	pub fn fold_item_block<'c, T: Folder<'c>>(this: &mut T, block: &'c Block_<Item_>) {
		fold_items(this, &block.val.vals);
	}

	pub fn fold_item<'c, T: Folder<'c>>(this: &mut T, val: &'c Item_) {
		match val.val {
			Item::Data(i, ref b) => this.fold_data(val.info, i, b),
			Item::Fn(i, ref p, ref b) => this.fold_fn(val.info, i, p, b)
		};
	}

	pub fn fold_expr<'c, T: Folder<'c>>(this: &mut T, val: &'c Expr_) {
		match val.val {
			Expr::Break => (),
			Expr::Error => (),
			Expr::Ref(ident, id) => this.fold_ref(ident, id),
			Expr::If(ref cond, ref then, ref otherwise) => {
				this.fold_expr(cond);
				this.fold_expr_block(then);
				if let Some(ref v) = *otherwise {
					this.fold_expr(v);
				};
			},
			Expr::Assign(_, ref lhs, ref rhs) => {
				this.fold_expr(lhs);
				this.fold_expr(rhs);
			} 
			Expr::UnaryOp(_, ref e) => {
				this.fold_expr(e);
			} 
			Expr::BinOp(ref lhs, _, ref rhs) => {
				this.fold_expr(lhs);
				this.fold_expr(rhs);
			} 
			Expr::Return(ref ret) => this.fold_expr(ret),
			Expr::Block(ref b) => this.fold_expr_block(b),
			Expr::Loop(ref b) => this.fold_expr_block(b),
		};
	}
	pub fn fold_items<'c, T: Folder<'c>>(this: &mut T, vals: &'c Vec<Item_>) {
		for v in vals.iter() {
			this.fold_item(v);
		}
	}

	pub fn fold_fn<'c, T: Folder<'c>>(this: &mut T, _info: Info, _name: Ident, params: &'c Vec<FnParam_>, block: &'c Block_<Expr_>) {
		fold_fn_params(this, params);
		this.fold_expr_block(block);
	}

	pub fn fold_fn_param<'c, T: Folder<'c>>(this: &mut T, param: &'c FnParam_) {
		this.fold_ty(&param.val.1);
	}

	pub fn fold_fn_params<'c, T: Folder<'c>>(this: &mut T, vals: &'c Vec<FnParam_>) {
		for v in vals.iter() {
			this.fold_fn_param(v);
		}
	}
}

pub trait Folder<'c>: Sized {
	fn fold_expr_block(&mut self, block: &'c Block_<Expr_>) {
		fold::fold_expr_block(self, block);
	}

	fn fold_item_block(&mut self, block: &'c Block_<Item_>) {
		fold::fold_item_block(self, block);
	}

	fn fold_data(&mut self, _info: Info, _name: Ident, block: &'c Block_<Item_>) {
		fold::fold_item_block(self, block);
	}

	fn fold_fn(&mut self, info: Info, name: Ident, params: &'c Vec<FnParam_>, block: &'c Block_<Expr_>) {
		fold::fold_fn(self, info, name, params, block);
	}

	fn fold_fn_param(&mut self, param: &'c FnParam_) {
		fold::fold_fn_param(self, param);
	}

	fn fold_item(&mut self, val: &'c Item_) {
		fold::fold_item(self, val);
	}

	fn fold_ty(&mut self, val: &'c Ty_) {
		match val.val {
			Ty::Infer => (),
			Ty::Error => (),
			Ty::Ptr(ref t) => self.fold_ty(t), 
			Ty::Ref(ident, id) => self.fold_ref(ident, id),
		};
	}

	fn fold_ref(&mut self, _ident: Ident, _id: Id) {
	}

	fn fold_expr(&mut self, val: &'c Expr_) {
		fold::fold_expr(self, val);
	}
}