use std::collections::{HashMap, HashSet};
use ast;
use misc::Context;
use ast::*;
use std::rc::Rc;
use node_map::NodeMap;

struct RecursionCtx<'a, 'c> {
	stack: Vec<Id>,
	node_map: &'c NodeMap<'c>,
	visited: &'a mut HashSet<Id>,
	map: &'a mut HashMap<Id, Rc<Vec<Id>>>,
}

impl<'a, 'c>  RecursionCtx<'a, 'c> {
	fn get_id(&mut self, id: Id) {
		if self.stack.last() == Some(&id) {
			return;
		}

		match self.stack.iter().rposition(|i| *i == id) {
			Some(i) => {
				let refs = Rc::new(self.stack[i..].to_vec());
				for id in refs.iter() {
					self.map.insert(*id, refs.clone());
				}
				return;
			}
			None => ()
		}

		if self.visited.contains(&id) {
			return;
		}

		self.visited.insert(id);

		self.stack.push(id);

		match *self.node_map.get(&id).unwrap() {
			Lookup::Item(item) => match item.val {
				Item::Fn(_) => self.visit_item(item),
				_ => (),
			},
			_ => (),
		}

		self.stack.pop();
	}
}

impl<'a, 'c> Visitor<'c> for RecursionCtx<'a, 'c> {
	fn visit_ref(&mut self, _: Ident, id: Id) {
		self.get_id(id);
	}
}

struct RecursionPass<'c> {
	node_map: &'c NodeMap<'c>,
	visited: HashSet<Id>,
	map: HashMap<Id, Rc<Vec<Id>>>,
}

impl<'c> Visitor<'c> for RecursionPass<'c> {
	fn visit_item(&mut self, val: &'c Item_) {
		RecursionCtx {
			stack: Vec::new(),
			node_map: self.node_map,
			visited: &mut self.visited,
			map: &mut self.map
		}.get_id(val.info.id);
		ast::visit::visit_item(self, val);
	}
}

pub fn run<'c>(ctx: &'c Context, node_map: &'c NodeMap<'c>) -> HashMap<Id, Rc<Vec<Id>>>{
	let mut pass = RecursionPass {
		node_map: node_map,
		visited: HashSet::new(),
		map: HashMap::new(),
	};

	for src in ctx.srcs.iter() {
		let block = src.ast.as_ref().unwrap();
		pass.visit_item_block(block);
	}

	pass.map
}