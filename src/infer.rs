use std::collections::HashMap;
use ast;
use misc;
use misc::Source;
use std::rc::Rc;
use lexer::Span;
use std::cell::RefCell;
use ast::{Id, Info, Ident, FnParam_, Generics, Block_, Expr_, Item, Expr, Item_, Folder, Lookup};
use ty::{Ty, Ty_, Scheme, Var, Substs};
use node_map::NodeMap;
use recursion;
use arena::TypedArena;

pub enum Msg {
	Error(String),
}

impl Msg {
	pub fn msg(&self, _src: &Source) -> String {
		match *self {
			Msg::Error(ref str) => str.clone(),
		}
	}
}

struct InferContext<'c> {
	src: &'c Source,
	arena: &'c TypedArena<Ty_<'c>>,
	node_map: &'c NodeMap<'c>,
	type_map: RefCell<HashMap<Id, Scheme<'c>>>,
	recursion_map: HashMap<Id, Rc<Vec<Id>>>,

	ty_err: Ty<'c>,
	ty_unit: Ty<'c>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum Valueness {
	Right,
	Left,
	LeftTuple,
}

#[derive(Copy, Clone)]
struct Args {
	pub loop_node: Option<Id>,
	pub unused: bool,
	pub valueness: Valueness,
	pub node: Id,
	pub in_typeof: bool,
}

impl Args {
	fn next(&self) -> Args {
		Args {
			node: self.node,
			loop_node: self.loop_node,
			unused: false,
			in_typeof: self.in_typeof,
			valueness: Valueness::Right,
		}
	}

	fn new(node: Id) -> Args {
		Args {
			node: node,
			loop_node: None,
			unused: true,
			in_typeof: false,
			valueness: Valueness::Right,
		}
	}

	fn map<F: FnOnce(&mut Self)>(&self, f: F) -> Args {
		let mut next = self.next();
		f(&mut next);
		next
	}
}

struct InferGroup<'ctx, 'c: 'ctx> {
	infer_vars: u32,
	ctx: &'ctx InferContext<'c>,
}

fn alloc_ty<'c>(arena: &'c TypedArena<Ty_<'c>>, ty: Ty_<'c>) -> Ty<'c> {
	arena.alloc(ty)
}

impl<'ctx, 'c> InferGroup<'ctx, 'c> {
	fn error(&self, sp: Span, msg: String) {
		self.ctx.src.msg(sp, misc::Msg::Infer(Msg::Error(msg)));
	}

	fn alloc_ty(&self, ty: Ty_<'c>) -> Ty<'c> {
		alloc_ty(self.ctx.arena, ty)
	}

	fn new_var(&mut self) -> Ty<'c> {
		let r = self.infer_vars;
		self.infer_vars += 1;
		self.alloc_ty(Ty_::Infer(Var(r)))
	}

	fn infer_generics(&mut self, generics: &'c Generics, ty: Ty<'c>) -> Scheme<'c> {
		Scheme {
			ty: ty,
			params: Vec::new(),
		}
	}

	fn infer(&mut self, args: Args, e: &'c Expr_) -> Ty<'c> {
		if args.valueness != Valueness::Right {
			match e {
				_ => {
					self.error(e.info.span, format!("Invalid l-value"));
					return self.ctx.ty_err;
				}
			}
		}


		self.ctx.ty_err
	}

	fn infer_block(&mut self, args: Args, b: &'c Block_<Expr_>) -> Ty<'c> {
		if !b.val.vals.is_empty() {
			let unused_next = args.next();
			for e in b.val.vals[..].init().iter() {
				self.infer(unused_next, e);
			};
		}

		match b.val.vals.last() {
			Some(r) => self.infer(args.map(|a| a.unused = args.unused ), r),
			None => self.ctx.ty_unit
		}
	}

	fn infer_item(&mut self, item: &'c Item_) -> Scheme<'c> {
		match item.val {
			Item::Data(i, ref g, ref b) => {
				let r = self.alloc_ty(Ty_::Ref(item.info.id, Substs(Vec::new())));
				self.infer_generics(g, r)
			}
			Item::Fn(ref d) => {
				let args = Args::new(item.info.id);
				let r = self.infer_block(args, &d.block);
				self.infer_generics(&d.generics, r)
			}
		}
	}

	fn infer_group(&mut self, ids: Vec<Id>) {
		for id in ids.iter() {
			let scheme = match *self.ctx.node_map.get(id).unwrap() {
				Lookup::Item(item) => self.infer_item(item),
				Lookup::Expr(_) => panic!(),
			};
			self.ctx.type_map.borrow_mut().insert(*id, scheme);
		}
	}
}

impl<'c> InferContext<'c> {
	fn alloc_ty(&self, ty: Ty_<'c>) -> Ty<'c> {
		self.arena.alloc(ty)
	}

	fn infer_id(&self, id: Id) -> Scheme<'c> {
		match self.type_map.borrow().get(&id) {
			Some(r) => return r.clone(),
			None => ()
		}
		let def = &[id];
		let ids = self.recursion_map.get(&id).map(|r| &r[..]).unwrap_or(def).to_vec();
		let mut group = InferGroup {
			infer_vars: 0,
			ctx: self,
		};
		group.infer_group(ids);

		self.infer_id(id)
	}

} 

struct InferPass<'ctx, 'c: 'ctx> {
	ctx: &'ctx InferContext<'c>,
}

impl<'ctx, 'c> Folder<'c> for InferPass<'ctx, 'c> {
	// Ignore expressions
	fn fold_expr(&mut self, val: &'c Expr_) {
	}

	fn fold_item(&mut self, val: &'c Item_) {
		self.ctx.infer_id(val.info.id);
		ast::fold::fold_item(self, val);
	}
}

pub fn run<'c>(src: &'c Source, block: &'c Block_<Item_>, node_map: &'c NodeMap<'c>) {
	let arena = TypedArena::new();
	let mut ctx = InferContext {
		src: src,
		arena: &arena,
		node_map: node_map,
		type_map: RefCell::new(HashMap::new()),
		recursion_map: recursion::run(block),

		ty_err: alloc_ty(&arena, Ty_::Error),
		ty_unit: alloc_ty(&arena, Ty_::Tuple(Vec::new())),
	};

	let mut pass = InferPass { ctx: &ctx };
	pass.fold_item_block::<'c, 'c>(block);
}