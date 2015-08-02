use std::collections::HashMap;
use ast::*;
use std::rc::Rc;
use ty;
use misc::interned::*;
use libc::{c_uint, c_ulonglong};
use infer::{GroupInfo, RefMap};
use llvm;
use llvm::{TypeRef, ValueRef, BasicBlockRef, BuilderRef};
use codegen::{GenContext, c_str};

pub struct GenExpr<'g, 'c: 'g> {
	pub ctx: &'g GenContext<'c>,
	pub def: Option<&'c FnDef>,
	pub builder: BuilderRef,
	pub bb: BasicBlockRef,
	pub map: &'g RefMap<'c>,
	pub func: ValueRef,
	pub info: Rc<GroupInfo<'c>>,
	pub vars: HashMap<Id, Option<ValueRef>>,
	pub post_loop: Option<BasicBlockRef>,
}

impl<'g, 'c> GenExpr<'g, 'c> {
	fn is_id_fn(&self, id: Id) -> bool {
		match *self.ctx.infer.node_map.get(&id).unwrap() {
			Lookup::Item(item) => match item.val {
				Item::Fn(..) => true,
				_ => false,
			},
			_ => false,
		}
	}

	pub fn new_bb(&mut self) -> BasicBlockRef {
		unsafe {
			llvm::LLVMAppendBasicBlockInContext(self.ctx.ll_ctx, self.func, c_str("").as_ptr())
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
		if let Some((last, prefix)) = b.val.vals[..].split_last() {
			for e in prefix {
				self.expr(e);
			};

			self.expr(last)
		} else {
			None
		}
	}

	pub fn fixed_ty(&self, ty: ty::Ty<'c>) -> ty::Ty<'c> {
		self.ctx.fixed_ty(ty, (self.map, &self.info.vars))
	}

	pub fn ll_ty(&self, ty: ty::Ty<'c>) -> Option<TypeRef> {
		self.ctx.ll_ty(self.fixed_ty(ty))
	}

	pub fn get_ref(&self, id: Id, map: &RefMap<'c>) -> String {
		let new_map = RefMap {
			params: map.params.iter().map(|(k,v)| (*k, self.fixed_ty(v))).collect()
		};
		self.ctx.gen(id, &new_map);
		self.ctx.mangle(id, &new_map)
	}

	pub fn expr(&mut self, e: &'c Expr_) -> Option<ValueRef> {
		macro_rules! llvm {
			($s:ident, $($x:expr),*) => {{
				llvm::$s(self.builder, $($x,)* c_str("").as_ptr())
			}}
		}

		unsafe {
			match e.val {
				Expr::Call(ref f_obj, ref f_args) => {
					match f_obj.val {
						Expr::Ref(_, id, _) if self.is_id_fn(id) => {
							let function = self.get_ref(id, self.info.refs.get(&f_obj.info.id).unwrap());
							let function = llvm::LLVMGetNamedFunction(self.ctx.ll_mod, c_str(&function).as_ptr());
							let args: Vec<ValueRef> = f_args.iter().map(|a| self.expr(a)).filter(|a| a.is_some()).map(|a| a.unwrap()).collect();
							Some(llvm!(LLVMBuildCall, function, args.as_ptr(), args.len() as c_uint))
						}
						_ => {
							/*
							let obj = self.expr(f_obj).unwrap();
							let mut args: Vec<ValueRef> = f_args.iter().map(|a| self.expr(a)).filter(|a| a.is_some()).map(|a| a.unwrap()).collect();
							args.insert(0, llvm::LLVMConstNull(self.ctx.void_ptr));
							Some(llvm!(LLVMBuildCall, obj, args.as_ptr(), args.len() as c_uint))*/
							None
						}
					}
				}
				Expr::Loop(ref b) => {
					let bb = self.new_bb();
					llvm::LLVMBuildBr(self.builder, bb);
					self.use_bb(bb);
					let old_post = self.post_loop;
					let post_loop = self.new_bb();
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
					let then_bb = self.new_bb();
					let else_bb = self.new_bb();
					llvm::LLVMBuildCondBr(self.builder, c, then_bb, else_bb);

					self.use_bb(then_bb);
					let then_v = self.block(then);

					let (else_v, post_bb) = match or.as_ref() {
						Some(e) => {
							let post_bb = self.new_bb();
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
							let phi = llvm!(LLVMBuildPhi, self.ll_ty(ty).unwrap());
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
				Expr::Ref(_, id, _) => {
					match self.vars.get(&id) {
						Some(v) => *v,
						None => {
							if self.is_id_fn(id) {
								let function = self.get_ref(id, self.info.refs.get(&e.info.id).unwrap());
								let function = llvm::LLVMGetNamedFunction(self.ctx.ll_mod, c_str(&function).as_ptr());
								None
							} else {
								None
							}
							/*
							let name = self.get_ref(id, self.info.refs.get(&e.info.id).unwrap());
							Some(llvm!(LLVMBuildLoad, llvm::LLVMGetNamedGlobal(self.ctx.ll_mod, c_str(&name).as_ptr())))*/
						}
					}
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
					let bb = self.new_bb();
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
