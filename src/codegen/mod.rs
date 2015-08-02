use std::collections::{HashMap, HashSet};
use ast;
use ast::*;
use std::cell::RefCell;
use std::ffi::CString;
use ty;
use infer;
use infer::{InferContext, RefMap};
use llvm;
use llvm::{ContextRef, TypeRef, ModuleRef};
use libc;

mod expr;

type Ctx<'m, 'c> = (&'m RefMap<'c>, &'m infer::Vars<'c>);

pub struct GenContext<'c> {
	infer: InferContext<'c>,

	gen_list: RefCell<HashMap<Id, HashSet<Vec<ty::Ty<'c>>>>>,

	ll_ctx: ContextRef,
	ll_mod: ModuleRef,

	void_ptr: TypeRef,
	i1: TypeRef,
}

pub fn c_str(s: &str) -> CString {
	CString::new(s).unwrap()
}

impl<'c> GenContext<'c> {
	pub fn ll_ty_or_void(&self, ty: ty::Ty<'c>) -> TypeRef {
		unsafe { self.ll_ty(ty).unwrap_or(llvm::LLVMVoidTypeInContext(self.ll_ctx)) }
	}

	pub fn ll_ty(&self, ty: ty::Ty<'c>) -> Option<TypeRef> {
		unsafe {
			match *ty {
				ty::Ty_::Error | ty::Ty_::Infer(..) | ty::Ty_::Proj(..)  | ty::Ty_::Kind(..) => panic!(),
				ty::Ty_::Tuple(ref args) => if args.is_empty() {
					None
				} else {
					Some(llvm::LLVMInt64TypeInContext(self.ll_ctx))
				},
				ty::Ty_::Ptr(p) => self.ll_ty(p).map(|l| llvm::LLVMPointerType(l, 0)),
				ty::Ty_::Fn(ref args, ret) => {
					let mut vec: Vec<TypeRef> = args.iter().map(|a| self.ll_ty(a)).filter(|a| a.is_some()).map(|a| a.unwrap()).collect();
					vec.insert(0, self.void_ptr);
					Some(llvm::LLVMFunctionType(self.ll_ty_or_void(ret), vec.as_ptr(), vec.len() as libc::c_uint, llvm::False))
				}
				ty::Ty_::Ref(id, _) if id == self.infer.id_bool => Some(self.i1),
				ty::Ty_::Ref(id, _) if id == self.infer.id_int => Some(llvm::LLVMInt64TypeInContext(self.ll_ctx)),
				ty::Ty_::Ref(..) => Some(self.i1),
			}
		}
	}

	pub fn fixed_ty<'m>(&self, ty: ty::Ty<'c>, ctx: Ctx<'m, 'c>) -> ty::Ty<'c> {
		ctx.1.inst_ty(&self.infer, ty, &ctx.0.params, false)
	}

	pub fn mangle_ty(&self, ty: ty::Ty<'c>, map: &RefMap<'c>) -> String {
		macro_rules! connect {
			($vec:expr) => {{
				let vec: Vec<String> = $vec.iter().map(|t| self.mangle_ty(t, map)).collect();
				vec.join(", ")
			}}
		}

		match *ty {
			ty::Ty_::Error | ty::Ty_::Infer(_) | ty::Ty_::Proj(_, _, _) => panic!(),
			ty::Ty_::Tuple(ref vec) => format!("({})", connect!(vec)),
			ty::Ty_::Fn(ref args, ret) => format!("(({}) -> {})", connect!(args), self.mangle_ty(ret, map)),
			ty::Ty_::Kind(_) => format!("kind"),
			ty::Ty_::Ref(i, ref substs) => format!("{}[{}]", self.infer.path(i), connect!(substs)),
			ty::Ty_::Ptr(p) => format!("{}*", self.mangle_ty(p, map)),
		}
	}

	pub fn mangle_name(&self, id: Id, map: &RefMap<'c>) -> String {
		let scheme = &self.infer.type_map.borrow().get(&id).unwrap().0.clone();

		let params = if scheme.params.is_empty() {
			"".to_owned()
		} else {
			let vec: Vec<String> = scheme.params.iter().map(|p| self.mangle_ty(map.params.get(&p.id).unwrap(), map)).collect();
			format!("[{}]", vec.join(", "))
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
		let scheme = &self.infer.type_map.borrow().get(&id).unwrap().0.clone();

		{
			let mut list = self.gen_list.borrow_mut();
			let set = list.entry(id).or_insert(HashSet::new());

			let params: Vec<ty::Ty<'c>> = scheme.params.iter().map(|p| *map.params.get(&p.id).unwrap() ).collect();

			if !set.insert(params) {
				return;
			}
		}

		let info = self.infer.type_map.borrow().get(&id).unwrap().1.clone();

		let ty = self.fixed_ty(scheme.ty, (map, &info.vars));

		{

			let p = map.params.iter().map(|(k,v)| format!("{}: {}, ", self.infer.path(*k), info.vars.format_ty(&self.infer, v))).fold(String::new(), |mut a, b| {
				a.push_str(&b);
				a
			});

			println!("gen {} map {{{}}}", self.infer.path(id), p);
		}

		match *self.infer.node_map.get(&id).unwrap() {
			Lookup::Item(item) => match item.val {
				Item::Fn(ref d) => {
					unsafe {
						let (params_tys, ll_ty) = if let ty::Ty_::Fn(ref args, ret) = *ty {
							let params = RefCell::new(HashMap::new());
							let vec: Vec<TypeRef> = args.iter().enumerate().map(|(i, a)| {
								let ty = self.ll_ty(a);
								(d.params[i].info.id, ty)
							}).filter(|&(id, a)| {
								if !a.is_some() {
									params.borrow_mut().insert(id, None);
								}
								a.is_some()
							}
							).enumerate().map(|(i, (id, a))| {
								params.borrow_mut().insert(id, Some(i));
								a.unwrap()
							}).collect();

							//vec.insert(0, self.void_ptr);
							(params.into_inner(), llvm::LLVMFunctionType(self.ll_ty_or_void(ret), vec.as_ptr(), vec.len() as libc::c_uint, llvm::False))
						} else {
							panic!()
						};

						let f = llvm::LLVMAddFunction(self.ll_mod,
							c_str(&self.mangle(id, map)).as_ptr(),
							ll_ty);

						let entry = llvm::LLVMAppendBasicBlockInContext(self.ll_ctx, f, c_str("").as_ptr());

						let builder = llvm::LLVMCreateBuilderInContext(self.ll_ctx);

						llvm::LLVMPositionBuilderAtEnd(builder, entry);

						let mut gen = expr::GenExpr {
							ctx: &self,
							def: Some(d),
							builder: builder,
							vars: HashMap::new(),
							bb: entry,
							func: f,
							map: map,
							info: info,
							post_loop: None,
						};

						for (id, i) in params_tys {
							gen.vars.insert(id, i.map(|i| llvm::LLVMGetParam(f, i as libc::c_uint)));
						}

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

unsafe fn make_ctx<'c>(infer: InferContext<'c>) -> GenContext<'c> {
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
		gen_list: RefCell::new(HashMap::new()),
		ll_ctx: llcx,
		ll_mod: llmod,
		void_ptr: llvm::LLVMPointerType(llvm::LLVMInt8TypeInContext(llcx), 0),
		i1: llvm::LLVMInt1TypeInContext(llcx),
	}
}

struct GenPass<'c> {
	ctx: GenContext<'c>,
}

impl<'c> Visitor<'c> for GenPass<'c> {
	// Ignore expressions
	fn visit_expr(&mut self, _: &'c Expr_) {
	}

	fn visit_item(&mut self, val: &'c Item_) {
		if self.ctx.infer.type_map.borrow().get(&val.info.id).unwrap().0.params.is_empty() {
			self.ctx.gen(val.info.id, &RefMap { params: HashMap::new() });
			ast::visit::visit_item(self, val);
		}
	}
}

/*
LLVMWriteBitcodeToFile
LLVMRustWriteOutputFile
*/
pub fn run<'i, 'c>(ctx: InferContext<'c>) {
	unsafe {
		let ctx = make_ctx(ctx);

		let mut pass = GenPass { ctx: ctx };

		for src in pass.ctx.infer.ctx.srcs.iter() {
			let block = src.ast.as_ref().unwrap();
			pass.visit_item_block(block);
		}

		let tm = llvm::LLVMRustCreateTargetMachine(
			CString::new("x86_64-generic-generic").unwrap().as_ptr(),
			CString::new("").unwrap().as_ptr(),
			CString::new("").unwrap().as_ptr(),
			llvm::CodeModelDefault,
			llvm::RelocPIC,
			llvm::CodeGenLevelNone,
			false,
			false,
			true,
			true,
			true,
		);


		let cpm = llvm::LLVMCreatePassManager();
		//llvm::LLVMRustAddAnalysisPasses(tm, cpm, ctx.ll_mod);
		//llvm::LLVMRustAddLibraryInfo(cpm, llmod, no_builtins);
		llvm::LLVMRustPrintModule(cpm, pass.ctx.ll_mod, CString::new("mod.ll").unwrap().as_ptr());
		llvm::LLVMDisposePassManager(cpm);

		llvm::LLVMRustDisposeTargetMachine(tm);
	}
}