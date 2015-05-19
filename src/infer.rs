use std::collections::HashMap;
use ast;
use misc;
use misc::Source;
use misc::interned::*;
use std::rc::Rc;
use lexer::Span;
use std::cell::RefCell;
use ast::{Id, Info, Ident, FnParam_, Generics, Block_, Expr_, Item, Expr, Item_, Visitor, Lookup};
use ty::{Ty, Ty_, Scheme, Level, TyParam};
use node_map::NodeMap;
use recursion;
use arena::TypedArena;
use detect_return;
use std::slice::SliceConcatExt;
use codegen;

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

pub struct RefMap<'c> {
	pub params: HashMap<Id, Ty<'c>>,
}

pub struct InferContext<'c> {
	pub src: &'c Source,
	arena: &'c TypedArena<Ty_<'c>>,
	pub node_map: &'c NodeMap<'c>,
	pub type_map: RefCell<HashMap<Id, (Scheme<'c>, Rc<GroupInfo<'c>>)>>,
	recursion_map: HashMap<Id, Rc<Vec<Id>>>,
	pub parents: HashMap<Id, Id>,

	ty_err: Ty<'c>,
	ty_int: Ty<'c>,
	ty_bool: Ty<'c>,
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
	pub valueness: Valueness,
	pub node: Id,
	pub in_typeof: bool,
}

impl Args {
	fn next(&self) -> Args {
		Args {
			node: self.node,
			loop_node: self.loop_node,
			in_typeof: self.in_typeof,
			valueness: Valueness::Right,
		}
	}

	fn new(node: Id) -> Args {
		Args {
			node: node,
			loop_node: None,
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

pub struct GroupInfo<'c> {
	pub vars: Vars<'c>,
	pub refs: HashMap<Id, RefMap<'c>>,
	pub tys: HashMap<Id, Ty<'c>>, // `If` nodes not present are unused
}

pub struct Vars<'c> {
	pub vars: RefCell<Vec<Ty<'c>>>,
}

impl<'c> Vars<'c> {
	fn format_ty_(&self, ctx: &InferContext<'c>, mut ty: Ty<'c>, p: bool) -> String {
		macro_rules! connect {
		    ($vec:expr) => {{
		    	let vec: Vec<String> = $vec.iter().map(|t| self.format_ty(ctx, t)).collect();
		    	vec.connect(", ")
		    }}
		}

		ty = self.prune(ty);
		match *ty {
			Ty_::Error => format!("<error>"),
			Ty_::Int => format!("int"),
			Ty_::Bool => format!("bool"),
			Ty_::Infer(i) => format!("λ{}", i),
			Ty_::Tuple(ref vec) => format!("({})", connect!(vec)),
			Ty_::Fn(ref args, ret) => if p {
				format!("(({}) -> {})", connect!(args), self.format_ty_(ctx, ret, true))
			} else {
				format!("({}) -> {}", connect!(args), self.format_ty_(ctx, ret, true))
			},
			Ty_::Kind(_) => format!("kind"),
			Ty_::Ref(i, ref substs) => format!("{}[{}]", ctx.path(i), connect!(substs)),
			Ty_::Proj(_, ref substs, _) => format!("proj[{}]", connect!(substs)),
			Ty_::Ptr(p) => format!("{}*", self.format_ty(ctx, p)),
		}
	}

	pub fn format_ty(&self, ctx: &InferContext<'c>, mut ty: Ty<'c>) -> String {
		self.format_ty_(ctx, ty, false)
	}

	fn lookup_var(&self, idx: usize) -> Ty<'c> {
		let ty = self.vars.borrow()[idx];

		match *ty {
			Ty_::Infer(target) if target != idx => {
				let r = self.lookup_var(target);
				self.vars.borrow_mut()[idx] = r;
				r
			}
			_ => ty,
		}
	}

	pub fn prune(&self, t: Ty<'c>, ) -> Ty<'c> {
		match *t {
			Ty_::Infer(var) => self.lookup_var(var),
			_ => t,
		}
	}

	pub fn alloc_ty(&self, ctx: &InferContext<'c>, ty: Ty_<'c>) -> Ty<'c> {
		alloc_ty(ctx.arena, ty)
	}

	pub fn inst_ty(&self, ctx: &InferContext<'c>, mut ty: Ty<'c>, params: &HashMap<Id, Ty<'c>>, allow_infer: bool) -> Ty<'c> {
		macro_rules! map {
		    ($t:expr) => {self.inst_ty(ctx, $t, params, allow_infer)}
		}

		macro_rules! map_vec {
		    ($vec:expr) => {$vec.iter().map(|t| map!(t)).collect()}
		}

		ty = self.prune(ty);

		match *ty {
			Ty_::Infer(_) => if allow_infer { ty } else { panic!() },
			Ty_::Error | Ty_::Int | Ty_::Bool => ty,
			Ty_::Tuple(ref vec) => self.alloc_ty(ctx, Ty_::Tuple(map_vec!(vec))),
			Ty_::Fn(ref args, ret) => self.alloc_ty(ctx, Ty_::Fn(map_vec!(args), map!(ret))),
			Ty_::Kind(id) => {
				match params.get(&id) {
					Some(r) => *r,
					None => ty,
				}
			}
			Ty_::Ref(id, ref substs) => {
				match params.get(&id) {
					Some(r) => {
						match **r {
							Ty_::Kind(id) => {
								debug_assert!(!substs.is_empty());
								self.alloc_ty(ctx, Ty_::Ref(id, map_vec!(substs)))
							}
							_ => {
								debug_assert!(substs.is_empty());
								*r
							}
						} 
					}
					None => self.alloc_ty(ctx, Ty_::Ref(id, map_vec!(substs))),
				}
			}
			Ty_::Proj(id, ref substs, sub) => self.alloc_ty(ctx, Ty_::Proj(id, map_vec!(substs), sub)),
			Ty_::Ptr(p) => self.alloc_ty(ctx, Ty_::Ptr(map!(p))),
		}
	}

}

struct InferGroup<'ctx, 'c: 'ctx> {
	info: GroupInfo<'c>,
	var_spans: RefCell<Vec<Span>>,
	tys: HashMap<Id, Scheme<'c>>,
	ctx: &'ctx InferContext<'c>,
}

fn alloc_ty<'c>(arena: &'c TypedArena<Ty_<'c>>, ty: Ty_<'c>) -> Ty<'c> {
	arena.alloc(ty)
}

impl<'ctx, 'c> InferGroup<'ctx, 'c> {
	fn name(&self, id: Id) -> String {
		self.ctx.info(id).0
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

	fn new_var(&mut self, sp: Span) -> Ty<'c> {
		let r = self.info.vars.vars.borrow().len();
		let t = self.alloc_ty(Ty_::Infer(r));
		self.info.vars.vars.borrow_mut().push(t);
		self.var_spans.borrow_mut().push(sp);
		t
	}

	fn inst_ty(&self, mut ty: Ty<'c>, params: &HashMap<Id, Ty<'c>>) -> Ty<'c> {
		self.info.vars.inst_ty(self.ctx, ty, params, true)
	}

	fn format_ty(&self, mut ty: Ty<'c>) -> String {
		self.info.vars.format_ty(self.ctx, ty)
	}

	fn infer_generics(&mut self, generics: &'c Generics, value: bool, ty: Ty<'c>) -> Scheme<'c> {
		Scheme {
			ty: ty,
			value: value,
			params: generics.params.iter().map(|p| {
				TyParam {
					id: p.info.id,
					scheme: Scheme {
						ty: self.alloc_ty(Ty_::Ref(p.info.id, Vec::new())),
						value: false,
						params: Vec::new()
					}
				}
			}).collect(),
		}
	}

	fn prune(&self, t: Ty<'c>, ) -> Ty<'c> {
		self.info.vars.prune(t)
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

				self.info.vars.vars.borrow_mut()[v as usize] = r;
				return
			}
			(_, &Ty_::Infer(v)) => return self.unify(sp, r, l),

			(&Ty_::Int, &Ty_::Int) => true,
			(&Ty_::Bool, &Ty_::Bool) => true,
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
			ast::Ty::Infer => self.new_var(ty.info.span),
			ast::Ty::Ref(_, id, ref substs) => {
				match self.infer_id(ty.info, id, substs) {
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

	fn infer(&mut self, args: Args, e: &'c Expr_, mut result: Option<Ty<'c>>) {
		macro_rules! error {
		    ($($e:expr),*) => {{
		    	let m = format!($($e),*);
		    	self.error(e.info.span, m)
		    }}
		}

		macro_rules! get_result {
		    () => {{
		    	if result.is_none() {
		    		result = Some(self.new_var(e.info.span));
		    	}
		    	result
		    }}
		}

		macro_rules! result {
		    ($t:expr) => {{
		    	let t = $t;
		    	match result {
		    		Some(r) => self.unify(e.info.span, r, t),
		    		None => result = Some(t),
		    	};
		    	result
		    }}
		}

		if args.valueness != Valueness::Right {
			match e.val {
				Expr::Ref(..) => (),
				_ => {
					error!("Invalid l-value");
					result!(self.ctx.ty_err);
					return;
				}
			}
		}

		match e.val {
			Expr::Call(ref obj, ref f_args) => {
				let fun = self.new_var(e.info.span);
				self.infer(args.next(), obj, Some(fun));

				match *self.prune(fun) {
					Ty_::Fn(ref obj_args, obj_r) => {
						if obj_args.len() != f_args.len() {
							error!("Call expected {} arguments, but {} was given", obj_args.len(), f_args.len());
						} else {
							for (o, a) in obj_args.iter().zip(f_args.iter()) {
								self.infer(args.next(), a, Some(*o));

							}
						}
						result!(obj_r);
					}
					_ => {
						let f_ty_args = f_args.iter().map(|a| {
							let v = self.new_var(a.info.span);
							self.infer(args.next(), a, Some(v));
							v
						}).collect();
						let r = self.new_var(e.info.span);
						let fun_ty = self.alloc_ty(Ty_::Fn(f_ty_args, r));
						self.unify(e.info.span, fun_ty, fun);
						result!(r);
					}
				}
			}
			Expr::Loop(ref b) => {
				let mut args = args.next();
				args.loop_node = Some(e.info.id);
				self.infer_block(args, b, None);
				result!(self.ctx.ty_unit);
			}
			Expr::Assign(op, ref lhs, ref rhs) => {
				let mut l_args = args.next();
				l_args.valueness = Valueness::LeftTuple;
				let r = get_result!();
				self.infer(l_args, lhs, r);
				self.infer(args.next(), rhs, r);
			}
			Expr::BinOp(ref lhs, op, ref rhs) => {
				let r = if op == OP_EQ {
					result!(self.ctx.ty_bool);
					Some(self.new_var(e.info.span))
				} else {
					get_result!()
				};
				self.infer(args.next(), lhs, r);
				self.infer(args.next(), rhs, r);
			}
			Expr::If(ref cond, ref then, ref or) => {
				result.map(|t| self.info.tys.insert(e.info.id, t));
				self.infer(args.next(), cond, Some(self.ctx.ty_bool));
				self.infer_block(args, &then, result);
				or.as_ref().map(|e| self.infer(args.next(), e, result));
			}
			Expr::Ref(name, id, ref substs) => {
				result!(self.infer_value(e.info, id, substs));
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
					Some(ref ret) => self.infer(args.next(), ret, Some(r)),
					None => self.unify(e.info.span, r, self.ctx.ty_unit),
				}
				result!(self.ctx.ty_unit);
			}
			Expr::Num(_) => {
				result!(self.ctx.ty_int);
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

		//self.print(e.info.span, &format!("expr({}) of type {}", e.info.id.0, self.format_ty(result.unwrap())));
	}

	fn infer_block(&mut self, args: Args, b: &'c Block_<Expr_>, result: Option<Ty<'c>>) {
		if !b.val.vals.is_empty() {
			let unused_next = args.next();
			for e in b.val.vals[..].init().iter() {
				self.infer(unused_next, e, None);
			};
		}

		match b.val.vals.last() {
			Some(r) => self.infer(args.next(), r, result),
			None => {result.map(|r| self.unify(b.info.span, r, self.ctx.ty_unit));},
		};
	}

	fn infer_value(&mut self, info: Info, id: Id, substs: &'c Option<Vec<ast::Ty_>>) -> Ty<'c> {
		match self.infer_id(info, id, substs) {
			Level::Value(v) => v,
			_ => {
				self.error(info.span, format!("Expected a value"));
				self.ctx.ty_err
			}
		}
	}

	fn infer_id(&mut self, info: Info, id: Id, substs: &'c Option<Vec<ast::Ty_>>) -> Level<'c> {
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
					self.error(info.span, format!("{} has {} type parameters, but {} was given", self.ctx.path(id), scheme.params.len(), substs.len()));
					return err;
				}
				for (sub, param) in substs.iter().zip(scheme.params.iter()) {
					param_map.insert(param.id, self.infer_ty(sub));
				}
			}
			None => {
				for param in scheme.params {
					param_map.insert(param.id, self.new_var(info.span));
				}
			}
		}

		// DEBUG
		let p = param_map.iter().map(|(k,v)| format!("{}: {}, ", self.ctx.path(*k), self.format_ty(v))).fold(String::new(), |mut a, b| {
	        a.push_str(&b);
	        a
	    });

		//self.print(sp, &format!("ref({}) map {}", id.0, p));

		let ty = self.inst_ty(scheme.ty, &param_map);

	    let map = RefMap {
	    	params: param_map,
	    };

	    self.info.refs.insert(info.id, map);

		if scheme.value {
			Level::Value(ty)
		} else {
			Level::Type(ty)
		}
	}

	fn infer_item_shallow(&mut self, item: &'c Item_) -> Scheme<'c> {
		match item.val {
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
						self.new_var(item.info.span)
					} else {
						self.ctx.ty_unit
					}
				} else {
					self.infer_ty(&d.returns)
				};
				let ty = self.alloc_ty(Ty_::Fn(ty_args, returns));
				self.infer_generics(&d.generics, true, ty)
			}
		}
	}

	fn infer_item(&mut self, item: &'c Item_) {
		let ty = self.tys.get(&item.info.id).unwrap().ty;

		match item.val {
			Item::Fn(ref d) => {
				let args = Args::new(item.info.id);
				self.infer_block(args, &d.block, None);
			}
			_ => ()
		}
	}

	fn infer_group(mut self, ids: Vec<Id>) -> (GroupInfo<'c>, HashMap<Id, Scheme<'c>>) {
		for id in ids.iter() {
			let scheme = match *self.ctx.node_map.get(id).unwrap() {
				Lookup::Item(item) => self.infer_item_shallow(item),
				Lookup::TypeParam(p) => Scheme {
					ty: self.alloc_ty(Ty_::Ref(p.info.id, Vec::new())),
					value: false,
					params: Vec::new(),
				},
				_ => panic!(),
			};
			self.tys.insert(*id, scheme);
		}
		for id in ids.iter() {
			match *self.ctx.node_map.get(id).unwrap() {
				Lookup::Item(item) => self.infer_item(item),
				Lookup::TypeParam(p) => (),
				_ => panic!(),
			};
		}
		for id in ids.iter() {
			let ty = self.tys.get(id).unwrap().ty;
			let (name, span) = self.ctx.info(*id);
			self.print(span, &format!("item({}) {} :: {}", id.0, self.ctx.path(*id), self.format_ty(ty)));
		}

		for i in 0..self.info.vars.vars.borrow().len() {
			let ty = self.info.vars.vars.borrow()[i];
			match *ty {
				Ty_::Infer(a) if a == i => {
					self.error(self.var_spans.borrow()[i], format!("Unconstrained type variable {}", self.format_ty(ty)));
				}
				_ => (),
			}
		}

		for (_, scheme) in self.tys.iter_mut() {
			scheme.ty = self.info.vars.inst_ty(self.ctx, scheme.ty, &HashMap::new(), true);
		}
/*
		for (_, map) in self.info.refs.iter_mut() {
			for (_, ty) in map.params.iter_mut() {
				*ty = self.info.vars.inst_ty(self.ctx, *ty, &HashMap::new(), true);
			}
		}
*/
		(self.info, self.tys)
	}
}

impl<'c> InferContext<'c> {
	fn infer_id(&self, id: Id) -> Scheme<'c> {
		match self.type_map.borrow().get(&id) {
			Some(r) => return r.0.clone(),
			None => ()
		}
		let def = &[id];
		let ids = self.recursion_map.get(&id).map(|r| &r[..]).unwrap_or(def).to_vec();
		let mut group = InferGroup {
			info: GroupInfo {
				vars: Vars {
					vars: RefCell::new(Vec::new())
				},
				tys: HashMap::new(),
				refs: HashMap::new(),
			},
			var_spans: RefCell::new(Vec::new()),
			tys: HashMap::new(),
			ctx: self,
		};
		let (info, tys) = group.infer_group(ids);
		let info = Rc::new(info);

		for (k, v) in tys {
			self.type_map.borrow_mut().insert(k, (v, info.clone()));
		}

		self.infer_id(id)
	}

	pub fn info(&self, id: Id) -> (String, Span) {
		let (ident, span) = match *self.node_map.get(&id).unwrap() {
			Lookup::Item(item) => (match item.val {
				Item::Fn(ref d) => d.name,
				Item::Data(name, _, _) => name,
			}, item.info.span),
			Lookup::FnParam(p) => (p.val.0, p.info.span),
			Lookup::TypeParam(p) => (p.val.name, p.info.span),
			_ => panic!(),
		};
		(self.src.get_name(ident.0.val), span)
	}

	pub fn path(&self, id: Id) -> String {
		match self.parents.get(&id) {
			Some(p) => format!("{}.{}", self.path(*p), self.info(id).0),
			_ => self.info(id).0,
		}
	}
} 

struct InferPass<'ctx, 'c: 'ctx> {
	ctx: &'ctx InferContext<'c>,
}

impl<'ctx, 'c> Visitor<'c> for InferPass<'ctx, 'c> {
	// Ignore expressions
	fn visit_expr(&mut self, _: &'c Expr_) {
	}

	fn visit_item(&mut self, val: &'c Item_) {
		self.ctx.infer_id(val.info.id);
		ast::visit::visit_item(self, val);
	}
}

pub fn run<'c>(src: &'c Source, block: &'c Block_<Item_>, node_map: &'c NodeMap<'c>, parents: HashMap<Id, Id>) {
	let arena = TypedArena::new();
	let ctx = InferContext {
		src: src,
		arena: &arena,
		node_map: node_map,
		type_map: RefCell::new(HashMap::new()),
		recursion_map: recursion::run(block, node_map),
		parents: parents,

		ty_err: alloc_ty(&arena, Ty_::Error),
		ty_int: alloc_ty(&arena, Ty_::Int),
		ty_bool: alloc_ty(&arena, Ty_::Bool),
		ty_unit: alloc_ty(&arena, Ty_::Tuple(Vec::new())),
	};

	let mut pass = InferPass { ctx: &ctx };
	pass.visit_item_block(block);

    if src.has_msgs() {
        print!("{}", src.format_msgs());
        return;
    }

    println!("generating code...");
    
    codegen::run(block, &ctx);
}