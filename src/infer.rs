use std::collections::HashMap;
use ast;
use misc;
use misc::Source;
use std::rc::Rc;
use lexer::Span;
use std::cell::RefCell;
use ast::{Id, Info, Ident, FnParam_, Generics, Block_, Expr_, Item, Expr, Item_, Folder, Lookup};
use ty::{Ty, Ty_, Scheme, Level};
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
	tys: HashMap<Id, Scheme<'c>>,
	ctx: &'ctx InferContext<'c>,
}

fn alloc_ty<'c>(arena: &'c TypedArena<Ty_<'c>>, ty: Ty_<'c>) -> Ty<'c> {
	arena.alloc(ty)
}

impl<'ctx, 'c> InferGroup<'ctx, 'c> {
	fn name(&self, id: Id) -> String {
		let ident = match *self.ctx.node_map.get(&id).unwrap() {
			Lookup::Item(item) => match item.val {
				Item::Fn(ref d) => d.name,
				Item::Data(name, _, _) => name,
			},
			Lookup::FnParam(p) => p.val.0,
			_ => panic!(),
		};
		self.ctx.src.get_name(ident.0.val)
	}

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
/*
	fn ty_eq(&self, mut a: Ty<'c>, mut b: Ty<'c>) -> bool {
		a = self.prune(a);
		b = self.prune(b);

		macro_rules! arg_match {
		    ($la:expr, $ra:expr) => {{
		    	let l_args = &$la[..];
		    	let r_args = &$ra[..];
				if l_args.len() != r_args.len() {
					false
				} else {
					l_args.iter().zip(r_args.iter()).all(|(l, r)| { self.ty_eq(l, r) })
				}
		    }}
		}

		match (a, b) {
			(&Ty_::Error, _) => true,
			(_, &Ty_::Error) => true,

			(&Ty_::Infer(a), &Ty_::Infer(b)) => a == b,

			(&Ty_::Int, &Ty_::Int) => true,
			(&Ty_::Tuple(ref la), &Ty_::Tuple(ref ra)) => arg_match!(la, ra),
			(&Ty_::Kind(a), &Ty_::Kind(b)) =>  a == b,
			(&Ty_::Ref(a, ref la), &Ty_::Ref(b, ref ra)) => a == b && arg_match!(la, ra),
			(&Ty_::Proj(a, ref la, ls), &Ty_::Proj(b, ref ra, rs)) => a == b && ls == rs && arg_match!(la, ra),
			(&Ty_::Fn(ref la, lr), &Ty_::Fn(ref ra, rr)) => self.ty_eq(lr, rr) && arg_match!(la, ra),
			(&Ty_::Ptr(lp), &Ty_::Ptr(rp)) => self.ty_eq(lp, rp),
			_ => false,
		}
	}
*/
	fn inst_ty<P: Fn(Id) -> Option<Ty<'c>>>(&self, mut ty: Ty<'c>, param: &P) -> Ty<'c> {
		macro_rules! map {
		    ($t:expr) => {self.inst_ty($t, param)}
		}

		macro_rules! map_vec {
		    ($vec:expr) => {$vec.iter().map(|t| map!(t)).collect()}
		}

		ty = self.prune(ty);

		match *ty {
			Ty_::Error | Ty_::Int | Ty_::Infer(_)  => ty,
			Ty_::Tuple(ref vec) => self.alloc_ty(Ty_::Tuple(map_vec!(vec))),
			Ty_::Fn(ref args, ret) => self.alloc_ty(Ty_::Fn(map_vec!(args), map!(ret))),
			Ty_::Kind(id) => {
				match param(id) {
					Some(r) => r,
					None => ty,
				}
			}
			Ty_::Ref(id, ref substs) => {
				match param(id) {
					Some(r) => {
						match *r {
							Ty_::Kind(id) => {
								debug_assert!(!substs.is_empty());
								self.alloc_ty(Ty_::Ref(id, map_vec!(substs)))
							}
							_ => {
								debug_assert!(substs.is_empty());
								r
							}
						} 
					}
					None => self.alloc_ty(Ty_::Ref(id, map_vec!(substs))),
				}
			}
			Ty_::Proj(id, ref substs, sub) => self.alloc_ty(Ty_::Proj(id, map_vec!(substs), sub)),
			Ty_::Ptr(p) => self.alloc_ty(Ty_::Ptr(map!(p))),
		}
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
			Ty_::Int => format!("int"),
			Ty_::Infer(i) => format!("λ{}", i),
			Ty_::Tuple(ref vec) => format!("({})", connect!(vec)),
			Ty_::Fn(ref args, ret) => if p {
				format!("(({}) -> {})", connect!(args), self.format_ty_(ret, true))
			} else {
				format!("({}) -> {}", connect!(args), self.format_ty_(ret, true))
			},
			Ty_::Kind(_) => format!("kind"),
			Ty_::Ref(_, ref substs) => format!("ref[{}]", connect!(substs)),
			Ty_::Proj(_, ref substs, _) => format!("proj[{}]", connect!(substs)),
			Ty_::Ptr(p) => format!("{}*", self.format_ty(p)),
		}
	}

	fn format_ty(&self, mut ty: Ty<'c>) -> String {
		self.format_ty_(ty, false)
	}

	fn infer_generics(&mut self, _generics: &'c Generics, value: bool, ty: Ty<'c>) -> Scheme<'c> {
		Scheme {
			ty: ty,
			value: value,
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

			(&Ty_::Infer(a), &Ty_::Infer(b)) if a == b => return, // TODO: Can this happen?
			(&Ty_::Infer(v), _) => {
				if l.occurs_in(r) {
					self.error(sp, format!("Type {} occurs in type {} during unification", self.format_ty(l), self.format_ty(r)));
					return;
				}

				self.infer_vars.borrow_mut()[v as usize] = r;
				return
			}
			(_, &Ty_::Infer(v)) => return self.unify(sp, r, l),

			(&Ty_::Int, &Ty_::Int) => true,
			(&Ty_::Tuple(ref la), &Ty_::Tuple(ref ra)) => arg_match!(la, ra),
			(&Ty_::Kind(a), &Ty_::Kind(b)) => a == b,
			(&Ty_::Ref(a, ref la), &Ty_::Ref(b, ref ra)) => a == b && arg_match!(la, ra),
			(&Ty_::Proj(a, ref la, ls), &Ty_::Proj(b, ref ra, rs)) => a == b && ls == rs && arg_match!(la, ra),
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
			ast::Ty::Ref(_, id, ref substs) => {
				match self.infer_id(ty.info.span, id, substs) {
					Level::Type(v) => v,
					_ => {
						self.error(ty.info.span, format!("Expected a type"));
						self.ctx.ty_err
					}
				}
			}
			ast::Ty::Ptr(ref p) => {
				let t = self.infer_ty(p);
				self.alloc_ty(Ty_::Ptr(t))
			},
			ast::Ty::Error => panic!(),
		}
	}

	fn infer(&mut self, args: Args, e: &'c Expr_, result: Ty<'c>) {
		macro_rules! error {
		    ($($e:expr),*) => {{
		    	let m = format!($($e),*);
		    	self.error(e.info.span, m)
		    }}
		}

		macro_rules! result {
		    ($t:expr) => {{
		    	let t = $t;
		    	self.unify(e.info.span, result, t)
		    }}
		}

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
			Expr::Loop(ref b) => {
				let mut args = args.next();
				args.loop_node = Some(e.info.id);
				let v = self.new_var();
				self.infer_block(args, b, v);
				result!(self.ctx.ty_unit);
			}
			Expr::Assign(op, ref lhs, ref rhs) => {
				let mut l_args = args.next();
				l_args.valueness = Valueness::LeftTuple;
				self.infer(l_args, lhs, result);
				self.infer(args.next(), rhs, result);
			}
			Expr::BinOp(ref lhs, op, ref rhs) => {
				self.infer(args.next(), lhs, result);
				self.infer(args.next(), rhs, result);
			}
			Expr::Ref(name, id, ref substs) => {
				result!(self.infer_value(e.info.span, id, substs));
			}
			Expr::Return(ref ret) => {
				let r = match *self.tys.get(&args.node).unwrap().ty {
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

		self.print(e.info.span, &format!("item of type {}", self.format_ty(result)));
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

	fn infer_value(&mut self, sp: Span, id: Id, substs: &'c Option<Vec<ast::Ty_>>) -> Ty<'c> {
		match self.infer_id(sp, id, substs) {
			Level::Value(v) => v,
			_ => {
				self.error(sp, format!("Expected a value"));
				self.ctx.ty_err
			}
		}
	}

	fn infer_id(&mut self, sp: Span, id: Id, substs: &'c Option<Vec<ast::Ty_>>) -> Level<'c> {
		let scheme = match self.tys.get(&id) {
			Some(t) => t.clone(),
			None => {
				self.ctx.infer_id(id)
			}
		};

		let err = if scheme.value {
			Level::Value(self.ctx.ty_err)
		} else {
			Level::Type(self.ctx.ty_err)
		};

		let mut param_map = HashMap::new();

		match *substs {
			Some(ref substs) => {
				if substs.len() != scheme.params.len() {
					self.error(sp, format!("{} has {} type parameters, but {} was given", self.name(id), scheme.params.len(), substs.len()));
					return err;
				}
				for (sub, param) in substs.iter().zip(scheme.params.iter()) {
					param_map.insert(param.id, self.infer_ty(sub));
				}
			}
			None => {
				for param in scheme.params {
					param_map.insert(param.id, self.new_var());
				}
			}
		}

		let param = |id| param_map.get(&id).map(|t| *t);

		let ty = self.inst_ty(scheme.ty, &param);

		if scheme.value {
			Level::Value(ty)
		} else {
			Level::Type(ty)
		}
	}

	fn infer_item_shallow(&mut self, item: &'c Item_) {
		let scheme = match item.val {
			Item::Data(i, ref g, _) => {
				let r = self.alloc_ty(Ty_::Ref(item.info.id, Vec::new()));
				self.infer_generics(g, false, r)
			}
			Item::Fn(ref d) => {
				let ty_args = d.params.iter().map(|p| {
					let t = self.infer_ty(&p.val.1);
					self.tys.insert(p.info.id, Scheme::plain(t));
					t
				}).collect();
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
				self.infer_generics(&d.generics, true, ty)
			}
		};
		self.tys.insert(item.info.id, scheme);
	}

	fn infer_item(&mut self, item: &'c Item_) {
		let ty = self.tys.get(&item.info.id).unwrap().ty;

		match item.val {
			Item::Fn(ref d) => {
				let args = Args::new(item.info.id);
				let t = self.new_var();
				self.infer_block(args, &d.block, t);
				self.print(item.info.span, &format!("fn {} :: {}", self.ctx.src.get_name(d.name.0.val), self.format_ty(ty)));
			}
			_ => ()
		}
	}

	fn infer_group(&mut self) {
		for id in self.ids.clone().iter() {
			match *self.ctx.node_map.get(id).unwrap() {
				Lookup::Item(item) => self.infer_item_shallow(item),
				_ => panic!(),
			};
		}
		for id in self.ids.clone().iter() {
			match *self.ctx.node_map.get(id).unwrap() {
				Lookup::Item(item) => self.infer_item(item),
				_ => panic!(),
			};
		}
		for (k, v) in self.tys.drain() {
			self.ctx.type_map.borrow_mut().insert(k, v);
		}
	}
}

impl<'c> InferContext<'c> {
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
	fn fold_expr(&mut self, _: &'c Expr_) {
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