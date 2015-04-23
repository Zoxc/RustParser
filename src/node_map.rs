use std::collections::HashMap;
use ast::*;

pub type NodeMap<'c> = HashMap<Id, Lookup<'c>>;

fn expr_block<'c>(map: &mut NodeMap<'c>, block: &'c Block_<Expr_>) {
	for v in block.val.vals.iter() {
		expr(map, v);
	}
}

fn item_block<'c>(map: &mut NodeMap<'c>, block: &'c Block_<Item_>) {
	for v in block.val.vals.iter() {
		item(map, v);
	}
}

fn expr<'c>(map: &mut NodeMap<'c>, node: &'c Expr_) {
	map.insert(node.info.id, Lookup::Expr(node));
	match node.val {
		Expr::Error => (),
		Expr::Ref(_, _) => (),
		Expr::If(ref cond, ref then, ref otherwise) => {
			expr(map, cond);
			expr_block(map, then);
			if let Some(ref v) = *otherwise {
				expr(map, v);
			};
		},
		Expr::Return(ref ret) => expr(map, ret),
		Expr::Block(ref b) => expr_block(map, b),
	};
}

fn item<'c>(map: &mut NodeMap<'c>, node: &'c Item_) {
	map.insert(node.info.id, Lookup::Item(node));
	match node.val {
		Item::Data(_, ref b) => item_block(map, b),
		Item::Fn(_, _, ref b) => expr_block(map, b),
	};
}

pub fn create<'c>(block: &'c Block_<Item_>) -> NodeMap<'c> {
	let mut map = HashMap::new();
	item_block(&mut map, block);
	map
}