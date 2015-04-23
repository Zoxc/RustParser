use misc::{Source, Msg};
use ast::*;

struct ResolutionPass<'c> {
	src: &'c Source,
	parent: Option<&'c ResolutionPass<'c>>,
	symbols: &'c SymbolTable,
}

impl<'c> ResolutionPass<'c> {
	fn lookup(&self, ident: Ident) -> Option<Id> {
		self.symbols.get(ident).or_else(|| {
			match self.parent {
				Some(parent) => parent.lookup(ident),
				None => None
			}
		})
	}
}

impl<'c> Folder for ResolutionPass<'c> {
	fn fold_expr_block(&mut self, block: &mut Block_<Expr_>) {
		let mut pass = ResolutionPass { src: self.src, parent: Some(&self), symbols: &mut block.val.symbols };
		fold::fold_exprs(&mut pass, &mut block.val.vals);
	}

	fn fold_item_block(&mut self, block: &mut Block_<Item_>) {
		let mut pass = ResolutionPass { src: self.src, parent: Some(&self), symbols: &mut block.val.symbols };
		fold::fold_items(&mut pass, &mut block.val.vals);
	}

	fn fold_ref(&mut self, ident: Ident, id: &mut Id) {
		match self.lookup(ident) {
			Some(node) => *id = node,
			None => self.src.msg(ident.0.span, Msg::Resolution(ident.0.val)),
		}
	}
}

pub fn run(src: &Source, block: &mut Block_<Item_>) {
	let mut empty = SymbolTable::new();
	let mut pass = ResolutionPass { src: src, parent: None, symbols: &mut empty };
	pass.fold_item_block(block);
}