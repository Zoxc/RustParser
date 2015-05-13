use std::collections::{HashMap, HashSet};
use ast;
use ast::*;
use std::rc::Rc;
use node_map::NodeMap;
use std::ffi::CString;
use infer::InferContext;
use std::ptr;
use llvm;
use llvm::{ContextRef, ValueRef, ModuleRef};

mod expr;

pub struct GenContext<'i, 'c: 'i> {
	infer: &'i InferContext<'c>,
	ll_ctx: ContextRef,
	ll_mod: ModuleRef,
}

pub fn c_str(s: &str) -> CString {
	CString::new(s).unwrap()
}

impl<'i, 'c> GenContext<'i, 'c> {
	pub fn mangle(&self, id: Id) {
	}
	pub fn gen_id(&self, id: Id) {
		match *self.infer.node_map.get(&id).unwrap() {
			Lookup::Item(item) => match item.val {
				Item::Fn(ref d) => {
					unsafe {
						let ty = llvm::LLVMFunctionType(llvm::LLVMVoidTypeInContext(self.ll_ctx), ptr::null(), 0, llvm::False);
						let f =  llvm::LLVMAddFunction(self.ll_mod,
							c_str(&self.infer.src.get_name(d.name.0.val)).as_ptr(),
							ty);

						let e = llvm::LLVMAppendBasicBlockInContext(self.ll_ctx, f, c_str("entry").as_ptr());

						let builder = llvm::LLVMCreateBuilderInContext(self.ll_ctx);

						llvm::LLVMPositionBuilderAtEnd(builder, e);

						let gen = expr::GenExpr {
							ctx: &self,
							def: Some(d),
							builder: builder,
							bb: e,
						};

						gen.block(&d.block);

						llvm::LLVMDisposeBuilder(builder);
					}
				}
				_ => ()
			},
			_ => panic!(),
		};
	}
}

unsafe fn make_ctx<'i, 'c>(infer: &'i InferContext<'c>) -> GenContext<'i, 'c> {
	// from rustc

	let llcx = llvm::LLVMContextCreate();

	let mod_name = CString::new("module").unwrap();
	let llmod = llvm::LLVMModuleCreateWithNameInContext(mod_name.as_ptr(), llcx);

	let data_layout = CString::new("e".as_bytes()).unwrap();
	llvm::LLVMSetDataLayout(llmod, data_layout.as_ptr());

	let llvm_target = CString::new("x86_64-generic-generic".as_bytes()).unwrap();
	llvm::LLVMRustSetNormalizedTarget(llmod, llvm_target.as_ptr());

	GenContext {
		infer: infer,
		ll_ctx: llcx,
		ll_mod: llmod,
	}
}

struct GenPass<'a, 'i: 'a, 'c: 'i> {
	ctx: &'a GenContext<'i, 'c>,
}

impl<'a, 'ctx, 'c> Visitor<'c> for GenPass<'a, 'ctx, 'c> {
	// Ignore expressions
	fn visit_expr(&mut self, _: &'c Expr_) {
	}

	fn visit_item(&mut self, val: &'c Item_) {
		self.ctx.gen_id(val.info.id);
		ast::visit::visit_item(self, val);
	}
}

/*
LLVMWriteBitcodeToFile
LLVMRustWriteOutputFile
*/
pub fn run<'i, 'c>(block: &'c Block_<Item_>, ctx: &'i InferContext<'c>) {
	unsafe {
		let ctx = make_ctx(ctx);

		let mut pass = GenPass { ctx: &ctx };
		pass.visit_item_block(block);

		let tm = llvm::LLVMRustCreateTargetMachine(
			CString::new("x86_64-generic-generic").unwrap().as_ptr(),
			CString::new("").unwrap().as_ptr(),
			CString::new("").unwrap().as_ptr(),
			llvm::CodeModelDefault,
			llvm::RelocPIC,
			llvm::CodeGenLevelNone,
			true,
			false,
			false,
			true,
			true,
			true,
		);


		let cpm = llvm::LLVMCreatePassManager();
		//llvm::LLVMRustAddAnalysisPasses(tm, cpm, ctx.ll_mod);
		//llvm::LLVMRustAddLibraryInfo(cpm, llmod, no_builtins);
		llvm::LLVMRustPrintModule(cpm, ctx.ll_mod, CString::new("mod.ll").unwrap().as_ptr());
		llvm::LLVMDisposePassManager(cpm);

		llvm::LLVMRustDisposeTargetMachine(tm);
	}
}