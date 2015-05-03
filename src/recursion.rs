use std::collections::HashMap;
use ast;
use ast::*;
use std::rc::Rc;

struct RecursionPass {
	stack: Vec<Id>,
	map: HashMap<Id, Rc<Vec<Id>>>,
}

impl<'c> Folder<'c> for RecursionPass {
	fn fold_ref(&mut self, _: Ident, id: Id) {
		match self.stack.iter().rposition(|i| *i == id) {
			Some(i) => {
				let refs = Rc::new(self.stack[i..].to_vec());
				for id in refs.iter() {
					self.map.insert(*id, refs.clone());
				}
			}
			None => ()
		}
	}

	fn fold_item(&mut self, val: &'c Item_) {
		self.stack.push(val.info.id);
		ast::fold::fold_item(self, val);
		self.stack.pop();
	}
}

pub fn run<'c>(block: &'c Block_<Item_>) -> HashMap<Id, Rc<Vec<Id>>>{
	let mut pass = RecursionPass {
		stack: Vec::new(),
		map: HashMap::new(),
	};
	pass.fold_item_block(block);
	pass.map
}