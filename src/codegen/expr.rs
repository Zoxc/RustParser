use std::collections::{HashMap, HashSet};
use ast;
use ast::*;
use std::rc::Rc;
use node_map::NodeMap;
use std::ffi::CString;
use infer::InferContext;
use llvm;
use llvm::{ContextRef, ValueRef, ModuleRef, BasicBlockRef, BuilderRef};
use codegen::GenContext;

pub struct GenExpr<'g, 'i: 'g, 'c: 'i> {
	pub ctx: &'g GenContext<'i, 'c>,
	pub def: Option<&'c FnDef>,
	pub builder: BuilderRef,
	pub bb: BasicBlockRef,
}

impl<'g, 'i, 'c> GenExpr<'g, 'i, 'c> {
	pub fn block(&self, b: &'c Block_<Expr_>) -> Option<ValueRef>  {
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

	pub fn expr(&self, e: &'c Expr_) -> Option<ValueRef>  {
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
					self.expr(lhs);
					self.expr(rhs);
					None
				}
				Expr::Ref(name, id, ref substs) => {
					None
				}
				Expr::Return(ref ret) => {
					None
				}
				Expr::Num(num) => {
					let t = llvm::LLVMInt64TypeInContext(self.ctx.ll_ctx);
					Some(llvm::LLVMConstIntOfString(t, CString::new(self.ctx.infer.src.get_num(num)).unwrap().as_ptr(), 10))
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
