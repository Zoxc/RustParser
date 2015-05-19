use std::collections::HashMap;
use ast::*;
use misc::Context;

pub type NodeMap<'c> = HashMap<Id, Lookup<'c>>;

struct NodeMapPass<'c> {
	map: NodeMap<'c>,
}

impl<'c> Visitor<'c> for NodeMapPass<'c> {
	fn visit_type_param(&mut self, val: &'c TypeParam_) {
		self.map.insert(val.info.id, Lookup::TypeParam(val));
		visit::visit_type_param(self, val);
	}

	fn visit_fn_param(&mut self, val: &'c FnParam_) {
		self.map.insert(val.info.id, Lookup::FnParam(val));
		visit::visit_fn_param(self, val);
	}

	fn visit_expr(&mut self, val: &'c Expr_) {
		self.map.insert(val.info.id, Lookup::Expr(val));
		visit::visit_expr(self, val);
	}

	fn visit_item(&mut self, val: &'c Item_) {
		self.map.insert(val.info.id, Lookup::Item(val));
		visit::visit_item(self, val);
	}
}

pub fn create<'c>(ctx: &'c Context) -> NodeMap<'c> {
	let mut pass = NodeMapPass { map: HashMap::new() };

	for src in ctx.srcs.iter() {
		let block = src.ast.as_ref().unwrap();
		pass.visit_item_block(block);
	}

	pass.map
}