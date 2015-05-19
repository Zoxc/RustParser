use std::collections::{HashMap, HashSet};
use ast;
use ast::*;
use std::rc::Rc;
use std::ptr;
use ty;
use misc::interned::*;
use node_map::NodeMap;
use std::ffi::CString;
use libc::{c_uint, c_ulonglong};
use infer::{InferContext, GroupInfo, RefMap};
use llvm;
use llvm::{TypeRef, ContextRef, ValueRef, ModuleRef, BasicBlockRef, BuilderRef};
use codegen::{GenContext, c_str};

pub struct GenExpr<'g, 'i: 'g, 'c: 'i> {
	pub ctx: &'g GenContext<'i, 'c>,
	pub def: Option<&'c FnDef>,
	pub builder: BuilderRef,
	pub bb: BasicBlockRef,
	pub map: &'g RefMap<'c>,
	pub func: ValueRef,
	pub info: Rc<GroupInfo<'c>>,
	pub vars: HashMap<Id, ValueRef>,
	pub post_loop: Option<BasicBlockRef>,
}

impl<'g, 'i, 'c> GenExpr<'g, 'i, 'c> {
	pub fn new_bb(&mut self, s: &str) -> BasicBlockRef {
		unsafe {
			llvm::LLVMAppendBasicBlockInContext(self.ctx.ll_ctx, self.func, c_str(s).as_ptr())
		}
	}

	pub fn use_bb(&mut self, b: BasicBlockRef) {
		unsafe {
			llvm::LLVMMoveBasicBlockAfter(b, self.bb);
			self.bb = b;
			llvm::LLVMPositionBuilderAtEnd(self.builder, b);
		}
	}

	pub fn block(&mut self, b: &'c Block_<Expr_>) -> Option<ValueRef>  {
		if !b.val.vals.is_empty() {
			for e in b.val.vals[..].init().iter() {
				self.expr(e);
			};
		}

		match b.val.vals.last() {
			Some(r) => self.expr(r),
			None => None,
		}
	}

	pub fn fixed_ty(&self, ty: ty::Ty<'c>) -> ty::Ty<'c> {
		self.ctx.fixed_ty(ty, (self.map, &self.info.vars))
	}

	pub fn ll_ty(&self, ty: ty::Ty<'c>) -> Option<TypeRef> {
		self.ctx.ll_ty(self.fixed_ty(ty))
	}

	pub fn get_ref(&self, id: Id, map: &RefMap<'c>) -> ValueRef {
		let new_map = RefMap {
			params: map.params.iter().map(|(k,v)| (*k, self.fixed_ty(v))).collect()
		};
		self.ctx.gen(id, &new_map);
		let s = self.ctx.mangle(id, &new_map);
		unsafe { llvm::LLVMGetNamedFunction(self.ctx.ll_mod, c_str(&s).as_ptr()) }
	}

	pub fn expr(&mut self, e: &'c Expr_) -> Option<ValueRef> {
		unsafe {
			match e.val {
				Expr::Call(ref obj, ref f_args) => {
					self.expr(obj);
					f_args.iter().map(|a| self.expr(a));
					None
				}
				Expr::Loop(ref b) => {
					let bb = self.new_bb("loop");
					llvm::LLVMBuildBr(self.builder, bb);
					self.use_bb(bb);
					let old_post = self.post_loop;
					let post_loop = self.new_bb("post-loop");
					self.post_loop = Some(post_loop);
					self.block(b);
					llvm::LLVMBuildBr(self.builder, bb);
					self.use_bb(post_loop);
					self.post_loop = old_post;
					None
				}
				Expr::Block(ref b) => {
					self.block(b)
				}
				Expr::Assign(_op, ref lhs, ref rhs) => {
					self.expr(lhs);
					self.expr(rhs);
					None
				}
				Expr::If(ref cond, ref then, ref or) => {
					let c = self.expr(cond).unwrap();
					let then_bb = self.new_bb("then");
					let else_bb = self.new_bb("else");
					llvm::LLVMBuildCondBr(self.builder, c, then_bb, else_bb);

					self.use_bb(then_bb);
					let then_v = self.block(then);

					let (else_v, post_bb) = match or.as_ref() {
						Some(e) => {
							let post_bb = self.new_bb("post-if");
							llvm::LLVMBuildBr(self.builder, post_bb);
							self.use_bb(else_bb);
							(self.expr(e), post_bb)
						},
						None => {
							(None, else_bb)
						}
					};

					llvm::LLVMBuildBr(self.builder, post_bb);

					self.use_bb(post_bb);

					self.info.tys.get(&e.info.id).and_then(|ty| {
						then_v.map(|t| {
							let phi = llvm::LLVMBuildPhi(self.builder, self.ll_ty(ty).unwrap(), c_str("join").as_ptr());
							llvm::LLVMAddIncoming(
								phi,
								&[t, else_v.unwrap()] as *const ValueRef,
								&[then_bb, else_bb] as *const BasicBlockRef,
								2);
							phi
						})
					})
				}
				Expr::BinOp(ref lhs, op, ref rhs) => {
					if op == OP_EQ {
						let l = self.expr(lhs);
						let r = self.expr(rhs);
						Some(match l {
							Some(lv) => {
								llvm::LLVMBuildICmp(self.builder, llvm::IntEQ as c_uint, lv, r.unwrap(), c_str("eq").as_ptr())
							}
							None => llvm::LLVMConstInt(self.ctx.i1, 1 as c_ulonglong, llvm::False)
						})
					} else {
						let l = self.expr(lhs).unwrap();
						let r = self.expr(rhs).unwrap();
						Some(llvm::LLVMBuildAdd(self.builder, l, r, c_str("add").as_ptr()))
					}
				}
				Expr::Ref(name, id, ref substs) => {
					let p = match self.vars.get(&id) {
						Some(v) => *v,
						None => self.get_ref(id, self.info.refs.get(&e.info.id).unwrap()),
					};
					//Some(llvm::LLVMBuildLoad(self.builder, p, c_str("ref").as_ptr()))
					None
				}
				Expr::Return(ref ret) => {
					match ret.as_ref().and_then(|v| self.expr(&v)) {
						Some(v) => llvm::LLVMBuildRet(self.builder, v),
						None => llvm::LLVMBuildRetVoid(self.builder)
					};
					None
				}
				Expr::Num(num) => {
					let t = llvm::LLVMInt64TypeInContext(self.ctx.ll_ctx);
					Some(llvm::LLVMConstIntOfString(t, c_str(&self.ctx.infer.ctx.get_num(num)).as_ptr(), 10))
				}
				Expr::Break => {
					llvm::LLVMBuildBr(self.builder, self.post_loop.unwrap());
					let bb = self.new_bb("post_break");
					self.use_bb(bb);
					None
				}
				_ => {
					panic!("Unknown AST node");
				}
			}
		}
	}
}
