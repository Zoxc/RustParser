use std::collections::{HashMap, HashSet};
use ast;
use ast::*;
use std::rc::Rc;
use std::ptr;
use ty;
use node_map::NodeMap;
use std::ffi::CString;
use infer::{InferContext, GroupInfo, RefMap};
use llvm;
use llvm::{ContextRef, ValueRef, ModuleRef, BasicBlockRef, BuilderRef};
use codegen::{GenContext, c_str};

pub struct GenExpr<'g, 'i: 'g, 'c: 'i> {
	pub ctx: &'g GenContext<'i, 'c>,
	pub def: Option<&'c FnDef>,
	pub builder: BuilderRef,
	pub bb: BasicBlockRef,
	pub map: &'g RefMap<'c>,
	pub info: Rc<GroupInfo<'c>>,
}

impl<'g, 'i, 'c> GenExpr<'g, 'i, 'c> {
	pub fn bb(&mut self, s: &str) -> BasicBlockRef {
		unsafe {
			let old = self.bb;
			self.bb =  llvm::LLVMInsertBasicBlockInContext(self.ctx.ll_ctx, self.bb, c_str(s).as_ptr());
			llvm::LLVMPositionBuilderAtEnd(self.builder, self.bb);
			old
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
		self.ctx.fixed_ty(ty, self.map)
	}

	pub fn get_ref(&self, id: Id, map: &RefMap<'c>) -> String {
		let new_map = RefMap {
			params: map.params.iter().map(|(k,v)| (*k, self.fixed_ty(v))).collect()
		};
		self.ctx.gen(id, &new_map);
		self.ctx.mangle(id, &new_map)
	}

	pub fn expr(&mut self, e: &'c Expr_) -> Option<ValueRef>  {
		unsafe {
			match e.val {
				Expr::Call(ref obj, ref f_args) => {
					self.expr(obj);
					f_args.iter().map(|a| self.expr(a));
					None
				}
				Expr::Loop(ref b) => {
					self.block(b);
					None
				}
				Expr::Assign(_op, ref lhs, ref rhs) => {
					self.expr(lhs);
					self.expr(rhs);
					None
				}
				Expr::BinOp(ref lhs, _op, ref rhs) => {
					let l = self.expr(lhs).unwrap();
					let r = self.expr(rhs).unwrap();
					Some(llvm::LLVMBuildAdd(self.builder, l, r, c_str("add").as_ptr()))
				}
				Expr::Ref(name, id, ref substs) => {
					self.get_ref(id, self.info.refs.get(&e.info.id).unwrap());
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
					Some(llvm::LLVMConstIntOfString(t, c_str(&self.ctx.infer.src.get_num(num)).as_ptr(), 10))
				}
				Expr::Break => {
					None
				}
				_ => {
					panic!("Unknown AST node");
				}
			}
		}
	}
}
