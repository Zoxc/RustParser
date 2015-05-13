use std::collections::{HashMap, HashSet};
use ast;
use ast::*;
use std::rc::Rc;
use node_map::NodeMap;
use std::cell::RefCell;
use std::ffi::CString;
use ty;
use infer;
use infer::{InferContext, RefMap};
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
	pub fn fixed_ty(&self, ty: ty::Ty<'c>, map: &RefMap<'c>) -> ty::Ty<'c> {

		let info = infer::Vars {
			vars: RefCell::new(Vec::new()),
		};

		info.inst_ty(self.infer, ty, &map.params, false)
	}

	pub fn mangle_ty(&self, ty: ty::Ty<'c>, map: &RefMap<'c>) -> String {
		/*match *ty {
			Ty_::Error | Ty_::Infer(_) => panic!(),
			Ty_::Int => format!("int"),
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
		let scheme = &self.infer.type_map.borrow().get(&id).unwrap().0.clone();

		if scheme.params.is_empty() {
			"".to_string()
		} else {
			let vec: Vec<String> = scheme.params.iter().map(|p| self.mangle_ty(map.get(&p.id), map)).collect();
		    format!("[{}]", vec.connect(", "))
		}

		let name = mangle_name(id, map);
		match self.infer.parents.get(&id) {
			Some(p) => format!("{}.{}", self.mangle_(*p, map), name),
			_ => name,
		}*/
		"ty".to_string()
	}

	pub fn mangle_name(&self, id: Id, map: &RefMap<'c>) -> String {
		let scheme = &self.infer.type_map.borrow().get(&id).unwrap().0.clone();

		let params = if scheme.params.is_empty() {
			"".to_string()
		} else {
			let vec: Vec<String> = scheme.params.iter().map(|p| self.mangle_ty(map.params.get(&p.id).unwrap(), map)).collect();
		    format!("[{}]", vec.connect(", "))
		};

		format!("{}{}", self.infer.info(id).0, params)
	}

	pub fn mangle_(&self, id: Id, map: &RefMap<'c>) -> String {
		let name = self.mangle_name(id, map);
		match self.infer.parents.get(&id) {
			Some(p) => format!("{}.{}", self.mangle_(*p, map), name),
			_ => name,
		}
	}

	pub fn mangle(&self, id: Id, map: &RefMap<'c>) -> String {
		format!("_{}", self.mangle_(id, map))
	}

	pub fn gen(&self, id: Id, map: &RefMap<'c>) {
		let info = &self.infer.type_map.borrow().get(&id).unwrap().1.clone();

		{

			let p = map.params.iter().map(|(k,v)| format!("{}: {}, ", self.infer.path(*k), info.vars.format_ty(self.infer, v))).fold(String::new(), |mut a, b| {
		        a.push_str(&b);
		        a
		    });

			println!("gen {} map {{{}}}", self.infer.path(id), p);
		}

		match *self.infer.node_map.get(&id).unwrap() {
			Lookup::Item(item) => match item.val {
				Item::Fn(ref d) => {
					unsafe {
						let ty = llvm::LLVMFunctionType(llvm::LLVMVoidTypeInContext(self.ll_ctx), ptr::null(), 0, llvm::False);
						let f =  llvm::LLVMAddFunction(self.ll_mod,
							c_str(&self.mangle(id, map)).as_ptr(),
							ty);

						let entry = llvm::LLVMAppendBasicBlockInContext(self.ll_ctx, f, c_str("entry").as_ptr());

						let builder = llvm::LLVMCreateBuilderInContext(self.ll_ctx);

						llvm::LLVMPositionBuilderAtEnd(builder, entry);

						let gen = expr::GenExpr {
							ctx: &self,
							def: Some(d),
							builder: builder,
							bb: entry,
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
		self.ctx.gen(val.info.id, &RefMap { params: HashMap::new() });
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