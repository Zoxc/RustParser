use ast::*;

struct DeclarePass<'c> {
	symbols: &'c mut SymbolTable
}

impl<'c> Folder for DeclarePass<'c> {
	fn fold_expr_block(&mut self, block: &mut Block_<Expr_>) {
		let mut pass = DeclarePass { symbols: &mut block.val.symbols };
		fold::fold_exprs(&mut pass, &mut block.val.vals);
	}

	fn fold_item_block(&mut self, block: &mut Block_<Item_>) {
		let mut pass = DeclarePass { symbols: &mut block.val.symbols };
		fold::fold_items(&mut pass, &mut block.val.vals);
	}

	fn fold_data(&mut self, info: Info, name: Ident, block: &mut Block_<Item_>) {
		self.symbols.name(info.id, name);
		fold::fold_item_block(self, block);
	}

	fn fold_fn(&mut self, info: Info, name: Ident, _params: &mut Vec<Ident>, block: &mut Block_<Expr_>) {
		self.symbols.name(info.id, name);
		fold::fold_expr_block(self, block);
	}

}

pub fn run(block: &mut Block_<Item_>) {
	let mut empty = SymbolTable::new();
	let mut pass = DeclarePass { symbols: &mut empty };
	pass.fold_item_block(block);
}