use std::collections::HashMap;
use ast;
use misc;
use misc::Source;
use std::rc::Rc;
use lexer::Span;
use std::cell::RefCell;
use ast::{Id, Info, Ident, FnParam_, Generics, Block_, Expr_, Item, Expr, Item_, Folder, Lookup};
use ty::{Ty, Ty_, Scheme, Substs};
use node_map::NodeMap;
use recursion;
use arena::TypedArena;
use detect_return;
use std::slice::SliceConcatExt;

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
	ty_int: Ty<'c>,
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
	infer_vars: RefCell<Vec<Ty<'c>>>,
	ids: Vec<Id>,
	tys: HashMap<Id, Ty<'c>>,
	ctx: &'ctx InferContext<'c>,
}

fn alloc_ty<'c>(arena: &'c TypedArena<Ty_<'c>>, ty: Ty_<'c>) -> Ty<'c> {
	arena.alloc(ty)
}

impl<'ctx, 'c> InferGroup<'ctx, 'c> {
	fn error(&self, sp: Span, msg: String) {
		self.ctx.src.msg(sp, misc::Msg::Infer(Msg::Error(msg)));
	}

	fn print(&self, sp: Span, s: &str) {
		println!("{}\n{}", s, self.ctx.src.format_span(sp));
	}

	fn alloc_ty(&self, ty: Ty_<'c>) -> Ty<'c> {
		alloc_ty(self.ctx.arena, ty)
	}

	fn new_var(&mut self) -> Ty<'c> {
		let r = self.infer_vars.borrow().len() as u32;
		let t = self.alloc_ty(Ty_::Infer(r));
		self.infer_vars.borrow_mut().push(t);
		t
	}

	fn format_ty_(&self, mut ty: Ty<'c>, p: bool) -> String {
		macro_rules! connect {
		    ($vec:expr) => {{
		    	let vec: Vec<String> = $vec.iter().map(|t| self.format_ty(t)).collect();
		    	vec.connect(", ")
		    }}
		}

		ty = self.prune(ty);
		match *ty {
			Ty_::Error => format!("<error>"),
			Ty_::Param(_) => format!("<Param>"),
			Ty_::Int => format!("int"),
			Ty_::Infer(i) => format!("ρ{}", i),
			Ty_::Tuple(ref vec) => format!("({})", connect!(vec)),
			Ty_::Fn(ref args, ret) => if p {
				format!("(({}) -> {})", connect!(args), self.format_ty_(ret, true))
			} else {
				format!("({}) -> {}", connect!(args), self.format_ty_(ret, true))
			},
			Ty_::Ref(_, ref substs) => format!("ref[{}]", connect!(substs.0)),
			Ty_::Ptr(p) => format!("{}*", self.format_ty(p)),
		}
	}

	fn format_ty(&self, mut ty: Ty<'c>) -> String {
		self.format_ty_(ty, false)
	}

	fn infer_generics(&mut self, _generics: &'c Generics, ty: Ty<'c>) -> Scheme<'c> {
		Scheme {
			ty: ty,
			params: Vec::new(),
		}
	}

	fn lookup_var(&self, idx: u32) -> Ty<'c> {
		let ty = self.infer_vars.borrow()[idx as usize];

		match *ty {
			Ty_::Infer(target) if target != idx => {
				let r = self.lookup_var(target);
				self.infer_vars.borrow_mut()[idx as usize] = r;
				r
			}
			_ => ty,
		}
	}

	fn prune(&self, t: Ty<'c>, ) -> Ty<'c> {
		match *t {
			Ty_::Infer(var) => self.lookup_var(var),
			_ => t,
		}
	}

	fn unify(&mut self, sp: Span, mut l: Ty<'c>, mut r: Ty<'c>) {
		l = self.prune(l);
		r = self.prune(r);

		macro_rules! arg_match {
		    ($la:expr, $ra:expr) => {{
		    	let l_args = &$la[..];
		    	let r_args = &$ra[..];
				if l_args.len() != r_args.len() {
					false
				} else {
					l_args.iter().zip(r_args.iter()).all(|(l, r)| { self.unify(sp, l, r); false });
					true
				}
		    }}
		}

		let success = match (l, r) {
			(&Ty_::Error, _) => return,
			(_, &Ty_::Error) => return,

			(&Ty_::Infer(v), _) => {
				if l == r { // TODO: Can this happen?
					return;
				}

				if l.occurs_in(r) {
					self.error(sp, format!("Type {} occurs in type {} during unification", self.format_ty(l), self.format_ty(r)));
					return;
				}

				self.infer_vars.borrow_mut()[v as usize] = r;
				return
			}
			(_, &Ty_::Infer(v)) => return self.unify(sp, r, l),

			(&Ty_::Param(a), &Ty_::Param(b)) => a == b,
			(&Ty_::Int, &Ty_::Int) => true,
			(&Ty_::Tuple(ref la), &Ty_::Tuple(ref ra)) => arg_match!(la, ra),
			(&Ty_::Ref(a, ref la), &Ty_::Ref(b, ref ra)) => a == b && arg_match!(la.0, ra.0),
			(&Ty_::Fn(ref la, lr), &Ty_::Fn(ref ra, rr)) => {
				self.unify(sp, lr, rr);
				arg_match!(la, ra)
			}
			(&Ty_::Ptr(lp), &Ty_::Ptr(rp)) => {
				self.unify(sp, lp, rp);
				true
			}
			_ => false,
		};

		if !success {
			self.error(sp, format!("Failed to unify types {} and {}", self.format_ty(l), self.format_ty(r)));
		}
	}

	fn infer_ty(&mut self, ty: &'c ast::Ty_) -> Ty<'c> {
		match ty.val {
			ast::Ty::Infer => self.new_var(),
			ast::Ty::Ref(_,_,_) => self.ctx.ty_int, // TODO: FIX
			ast::Ty::Ptr(ref p) => {
				let t = self.infer_ty(p);
				self.alloc_ty(Ty_::Ptr(t))
			},
			ast::Ty::Error => panic!(),
		}
	}

	fn infer(&mut self, args: Args, e: &'c Expr_, result: Ty<'c>) {
		macro_rules! error {
		    ($($e:expr),*) => {self.error(e.info.span, format!($($e),*))}
		}

		macro_rules! result {
		    ($t:expr) => {self.unify(e.info.span, result, $t)}
		}

				self.print(e.info.span, "item");
		if args.valueness != Valueness::Right {
			match e.val {
				_ => {
					error!("Invalid l-value");
					result!(self.ctx.ty_err);
					return;
				}
			}
		}

		match e.val {
			Expr::Ref(id, ref ret, ref substs) => {
				result!(self.ctx.ty_int);
			}
			Expr::Return(ref ret) => {
				let r = match **self.tys.get(&args.node).unwrap() {
					Ty_::Fn(_, r) => r,
					_ => {
						error!("Return outside a function");
						self.ctx.ty_err
					}
				};
				match *ret {
					Some(ref ret) => self.infer(args.next(), ret, r),
					None => self.unify(e.info.span, r, self.ctx.ty_unit),
				}
			}
			Expr::Break => {
				if args.loop_node.is_none() {
					error!("Break without a loop");
					result!(self.ctx.ty_err);
				} else {
					result!(self.ctx.ty_unit);
				}
			}
			_ => {
				error!("Unknown AST node");
				result!(self.ctx.ty_err);
			}
		}
	}

	fn infer_block(&mut self, args: Args, b: &'c Block_<Expr_>, result: Ty<'c>) {
		if !b.val.vals.is_empty() {
			let unused_next = args.next();
			for e in b.val.vals[..].init().iter() {
				let var = self.new_var();
				self.infer(unused_next, e, var);
			};
		}

		match b.val.vals.last() {
			Some(r) => self.infer(args.map(|a| a.unused = args.unused ), r, result),
			None => self.unify(b.info.span, result, self.ctx.ty_unit),
		}
	}

	fn infer_item(&mut self, item: &'c Item_) -> Scheme<'c> {
		match item.val {
			Item::Data(i, ref g, _) => {
				let r = self.alloc_ty(Ty_::Ref(item.info.id, Substs(Vec::new())));
				self.infer_generics(g, r)
			}
			Item::Fn(ref d) => {
				let args = Args::new(item.info.id);
				let ty_args = d.params.iter().map(|p| self.infer_ty(&p.val.1)).collect();
				let returns = if d.returns.val == ast::Ty::Infer {
					if detect_return::run(&d.block) {
						self.new_var()
					} else {
						self.ctx.ty_unit
					}
				} else {
					self.infer_ty(&d.returns)
				};
				let ty = self.alloc_ty(Ty_::Fn(ty_args, returns));
				self.tys.insert(item.info.id, ty);
				self.print(item.info.span, &format!("fn {} of type {}", self.ctx.src.get_name(d.name.0.val), self.format_ty(ty)));
				let t = self.new_var();
				self.infer_block(args, &d.block, t);
				let t = self.new_var();
				self.infer_generics(&d.generics, t)
			}
		}
	}

	fn infer_group(&mut self) {
		for id in self.ids.clone().iter() {
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
			infer_vars: RefCell::new(Vec::new()),
			ids: ids,
			tys: HashMap::new(),
			ctx: self,
		};
		group.infer_group();

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
	let ctx = InferContext {
		src: src,
		arena: &arena,
		node_map: node_map,
		type_map: RefCell::new(HashMap::new()),
		recursion_map: recursion::run(block),

		ty_err: alloc_ty(&arena, Ty_::Error),
		ty_int: alloc_ty(&arena, Ty_::Int),
		ty_unit: alloc_ty(&arena, Ty_::Tuple(Vec::new())),
	};

	let mut pass = InferPass { ctx: &ctx };
	pass.fold_item_block::<'c, 'c>(block);
}