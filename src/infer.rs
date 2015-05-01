use std::collections::HashMap;
use ast;
use std::rc::Rc;
use ast::{Id, Info, Ident, FnParam_, Block_, Expr_, Item_, Folder};
use ty::{Ty, Ty_, Scheme, Var};
use node_map::NodeMap;
use recursion;
use arena::TypedArena;

struct InferContext<'c> {
	arena: &'c TypedArena<Ty_<'c>>,
	node_map: &'c NodeMap<'c>,
	type_map: HashMap<Id, Scheme<'c>>,
	recursion_map: HashMap<Id, Rc<Vec<Id>>>,
}

struct InferGroup<'ctx, 'c> {
	infer_vars: u32,
	ids: Vec<Id>,
	ctx: &'ctx InferContext<'c>,
}

impl<'ctx, 'c> InferGroup<'ctx, 'c> {
	fn alloc_ty(&self, ty: Ty_<'c>) -> Ty<'c> {
		self.ctx.arena.alloc(ty)
	}

	fn new_var(&mut self) -> Ty<'c> {
		let r = self.infer_vars;
		self.infer_vars += 1;
		self.alloc_ty(Ty_::Infer(Var(r)))
	}

	fn infer(&mut self) {

	}
}

impl<'c> InferContext<'c> {
	fn infer_id(&self, id: Id) -> Scheme<'c> {
		match self.type_map.get(&id) {
			Some(r) => return r.clone(),
			None => ()
		}
		let def = &[id];
		let ids = self.recursion_map.get(&id).map(|r| &r[..]).unwrap_or(def).to_vec();
		let mut group = InferGroup {
			infer_vars: 0,
			ids: ids,
			ctx: self,
		};
		group.infer();

		self.infer_id(id)
	}

} 

struct InferPass<'c> {
	ctx: &'c InferContext<'c>,
}

impl<'c> Folder<'c> for InferPass<'c> {
	fn fold_item(&mut self, val: &'c Item_) {
		self.ctx.infer_id(val.info.id);
		ast::fold::fold_item(self, val);
	}
}

pub fn run<'c>(block: &'c Block_<Item_>, node_map: &'c NodeMap<'c>) {
	let arena = TypedArena::new();
	let mut ctx = InferContext {
		arena: &arena,
		node_map: node_map,
		type_map: HashMap::new(),
		recursion_map: recursion::run(block),
	};

	let mut pass = InferPass { ctx: &ctx };
	pass.fold_item_block(block);
}