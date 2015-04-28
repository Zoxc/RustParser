use ast::*;

struct DeclarePass<'c> {
	symbols: &'c mut SymbolTable
}

impl<'c> FolderMut for DeclarePass<'c> {
	fn fold_expr_block(&mut self, block: &mut Block_<Expr_>) {
		let mut pass = DeclarePass { symbols: &mut block.val.symbols };
		fold_mut::fold_exprs(&mut pass, &mut block.val.vals);
	}

	fn fold_item_block(&mut self, block: &mut Block_<Item_>) {
		let mut pass = DeclarePass { symbols: &mut block.val.symbols };
		fold_mut::fold_items(&mut pass, &mut block.val.vals);
	}

	fn fold_data(&mut self, info: Info, name: Ident, block: &mut Block_<Item_>) {
		self.symbols.name(info.id, name);
		fold_mut::fold_item_block(self, block);
	}

	fn fold_fn_param(&mut self, param: &mut FnParam_) {
		self.symbols.name(param.info.id, param.val.0);
		fold_mut::fold_fn_param(self, param);
	}

	fn fold_fn(&mut self, info: Info, name: Ident, _params: &mut Vec<FnParam_>, block: &mut Block_<Expr_>) {
		self.symbols.name(info.id, name);
		let mut pass = DeclarePass { symbols: &mut block.val.symbols };
		fold_mut::fold_exprs(&mut pass, &mut block.val.vals);
		fold_mut::fold_fn_params(&mut pass, _params);
	}
}

pub fn run(block: &mut Block_<Item_>) {
	let mut empty = SymbolTable::new();
	let mut pass = DeclarePass { symbols: &mut empty };
	pass.fold_item_block(block);
}