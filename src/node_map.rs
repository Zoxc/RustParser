use std::collections::HashMap;
use ast::*;

pub type NodeMap<'c> = HashMap<Id, Lookup<'c>>;

struct NodeMapPass<'c> {
	map: NodeMap<'c>,
}

impl<'c> Folder<'c> for NodeMapPass<'c> {
	fn fold_type_param(&mut self, val: &'c TypeParam_) {
		self.map.insert(val.info.id, Lookup::TypeParam(val));
		fold::fold_type_param(self, val);
	}

	fn fold_fn_param(&mut self, val: &'c FnParam_) {
		self.map.insert(val.info.id, Lookup::FnParam(val));
		fold::fold_fn_param(self, val);
	}

	fn fold_expr(&mut self, val: &'c Expr_) {
		self.map.insert(val.info.id, Lookup::Expr(val));
		fold::fold_expr(self, val);
	}

	fn fold_item(&mut self, val: &'c Item_) {
		self.map.insert(val.info.id, Lookup::Item(val));
		fold::fold_item(self, val);
	}
}

pub fn create<'c>(block: &'c Block_<Item_>) -> NodeMap<'c> {
	let mut pass = NodeMapPass { map: HashMap::new() };
	pass.fold_item_block(block);
	pass.map
}