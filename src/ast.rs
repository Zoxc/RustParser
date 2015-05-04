use lexer::{Span, Spanned};
use misc::{Name, Num, Op, Source};
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

pub type TypeParam_ = N<TypeParam>;

#[derive(Clone)]
pub struct TypeParam {
	pub name: Ident,
}

#[derive(Clone)]
pub struct Generics {
	pub params: Vec<TypeParam_>,
}

#[derive(Clone)]
pub struct FnDef {
	pub name: Ident,
	pub generics: Generics,
	pub params: Vec<FnParam_>,
	pub returns: Ty_,
	pub block: Block_<Expr_>,
}

pub type Item_ = N<Item>;

#[derive(Clone)]
pub enum Item {
	Data(Ident, Generics, Block_<N<Item>>),
	Fn(FnDef),
}

pub type Ty_ = N<Ty>;

#[derive(Clone, Eq, PartialEq)]
pub enum Ty {
	Error,
	Infer,
	Ptr(Box<Ty_>),
	Ref(Ident, Id, Option<Vec<Ty_>>),
}

pub type Expr_ = N<Expr>;

#[derive(Clone)]
pub enum Expr {
	Error,
	Num(Num),
	Assign(Op, Box<N<Expr>>, Box<N<Expr>>),
	BinOp(Box<N<Expr>>, Op, Box<N<Expr>>),
	UnaryOp(Op, Box<N<Expr>>),
	If(Box<N<Expr>>, Block_<N<Expr>>, Option<Box<N<Expr>>>),
	Return(Option<Box<N<Expr>>>),
	Block(Block_<N<Expr>>),
	Loop(Block_<Expr_>),
	Break,
	Ref(Ident, Id, Option<Vec<Ty_>>),
}

pub enum Lookup<'c> {
	Item(&'c Item_),
	Expr(&'c Expr_),
	FnParam(&'c FnParam_),
	TypeParam(&'c TypeParam_),
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
			Item::Data(i, ref g, ref b) => this.fold_data(val.info, i, g, b),
			Item::Fn(ref d) => this.fold_fn(val.info, d)
		};
	}

	pub fn fold_ty_substs<'c, T: Folder<'c>>(this: &mut T, val: &'c Option<Vec<Ty_>>) {
		val.as_ref().map(|t| t.iter().map(|t| this.fold_ty(t)));
	}

	pub fn fold_expr<'c, T: Folder<'c>>(this: &mut T, val: &'c Expr_) {
		match val.val {
			Expr::Break | Expr::Error | Expr::Num(_) => (),
			Expr::Ref(ident, id, ref substs) => {
				this.fold_ref(ident, id);
				fold_ty_substs(this, substs);
			}
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
			Expr::Return(ref ret) => {ret.as_ref().map(|m| this.fold_expr(&m));},
			Expr::Block(ref b) => this.fold_expr_block(b),
			Expr::Loop(ref b) => this.fold_expr_block(b),
		};
	}
	pub fn fold_items<'c, T: Folder<'c>>(this: &mut T, vals: &'c Vec<Item_>) {
		for v in vals.iter() {
			this.fold_item(v);
		}
	}

	pub fn fold_type_param<'c, T: Folder<'c>>(this: &mut T, _param: &'c TypeParam_) {
	}

	pub fn fold_fn<'c, T: Folder<'c>>(this: &mut T, _info: Info, def: &'c FnDef) {
		fold_fn_params(this, &def.params);
		this.fold_expr_block(&def.block);
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

	fn fold_type_param(&mut self, _param: &'c TypeParam_) {
	}

	fn fold_generics(&mut self, generics: &'c Generics) {
		for p in generics.params.iter() {
			self.fold_type_param(p);
		}
	}

	fn fold_data(&mut self, _info: Info, _name: Ident, generics: &'c Generics, block: &'c Block_<Item_>) {
		self.fold_generics(generics);
		fold::fold_item_block(self, block);
	}

	fn fold_fn(&mut self, info: Info, def: &'c FnDef){
		fold::fold_fn(self, info, def);
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
			Ty::Ref(ident, id, ref substs) => {
				self.fold_ref(ident, id);
				fold::fold_ty_substs(self, substs);
			},
		};
	}

	fn fold_ref(&mut self, _ident: Ident, _id: Id) {
	}

	fn fold_expr(&mut self, val: &'c Expr_) {
		fold::fold_expr(self, val);
	}
}