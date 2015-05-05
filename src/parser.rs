use lexer;
use lexer::{Token, Indent, Span, Spanned, Bracket};
use ast::*;
use print;
use misc;
use misc::interned::*;
use misc::{Source, Op, Name};

pub enum Msg {
	Expected(String, Token),
	ExpectedToken(Token, Token),
}

impl Msg {
	pub fn msg(&self, _src: &Source) -> String {
		match *self {
			Msg::Expected(ref expected, found) => format!("Expected {}, but found {:?}", expected, found),
			Msg::ExpectedToken(expected, found) => format!("Expected {:?}, but found {:?}", expected, found),
		}
	}
}

macro_rules! spanned {
    ($this:expr, $c:expr) => {{
		let start = $this.lexer.span;
		let val = $c;
		Spanned::new($this.span(start), val)
    }};
}

macro_rules! noded {
    ($this:expr, $c:expr) => {{
		let start = $this.lexer.span;
		let val = $c;
		N::new($this.lexer.src, $this.span(start), val)
    }};
}

macro_rules! extend {
    ($this:expr, $node:expr, $c:expr) => {{
		let start = $node.info.span;
		let val = $c;
		N::new($this.lexer.src, $this.span(start), val)
    }};
}

macro_rules! node_wrap {
    ($this:expr, $c:expr) => {{
		let start = $this.lexer.span;
		$c.map(|v| N::new($this.lexer.src, $this.span(start), v))
    }};
}

struct Parser<'c> {
	lexer: lexer::Lexer<'c>,
	last_ended: u32,
}

impl<'c> Parser<'c> {
	pub fn new(src: &'c Source) -> Parser<'c> {
		Parser {
			lexer: lexer::Lexer::new(src),
			last_ended: 0,
		}
	}

	fn is_op_prefix(&self, c: char) -> bool {
		match self.tok() {
			Token::Op(op) => self.lexer.src.get_op(op).chars().next().unwrap() == c,
			_ => false
		}
	}

	fn skip_op_prefix(&mut self) {
		match self.tok() {
			Token::Op(_) => {
				if self.lexer.span.len == 1 {
					self.step();
				} else {
				self.last_ended = self.lexer.span.start;
				self.lexer.span.start += 1;
				self.lexer.span.len -= 1;
				self.lexer.token = Token::Op(self.lexer.intern(&self.lexer.src.ctx.interners.op, self.lexer.span));
    			//bself.print(&format!("SkipTok {:?}", self.lexer.token));
					
				}
			}
			_ => panic!()
		}
	}

	fn msg(&self, span: Span, msg: Msg) {
		self.lexer.src.msg(span, misc::Msg::Parser(msg));
	}

	fn tok(&self) -> Token {
		self.lexer.token
	}

	fn is(&self, tok: Token) -> bool {
		self.lexer.token == tok
	}

	#[allow(dead_code)]
	fn print(&self, s: &str) {
		println!("{}\n{}", s, self.lexer.src.format_span(self.lexer.span));
	}

	fn bracket<F, R>(&mut self, bracket: Bracket, f: F) -> Option<R> where F : FnOnce(&mut Self) -> R {
		self.expect(Token::Bracket(bracket, true));
		let r = if self.is(Token::Bracket(bracket, false)) {
			None
		} else {
			Some(f(self))
		};
		self.expect(Token::Bracket(bracket, false));
		r
	}

	fn seq<F, R>(&mut self, close: Token, mut f: F) -> Vec<R> where F : FnMut(&mut Self) -> Option<R> {
		let mut r = Vec::new();

		loop {
			match f(self) {
				Some(v) => r.push(v),
				None => break,
			}

			if self.is(Token::Op(OP_COMMA)) {
				self.step();
				self.skip(Token::Line);

				if self.is(close) {
					break
				}
			} else {
				break;
			}
		}

		r
	}

	fn bracket_seq<F, R>(&mut self, bracket: Bracket, f: F) -> Vec<R> where F : FnMut(&mut Self) -> Option<R> {
		self.bracket(bracket, |parser| {
			parser.seq(Token::Bracket(bracket, false), f)
		}).unwrap_or(Vec::new())
	}

	fn step(&mut self) {
		self.last_ended = self.lexer.span.start + self.lexer.span.len;
		self.lexer.next_token();
	}

	fn skip(&mut self, tok: Token) {
		if self.is(tok) {
			self.step()
		}
	}

	fn expect(&mut self, tok: Token) {
		if self.is(tok) {
			self.step()
		} else {
			self.msg(self.lexer.span, Msg::ExpectedToken(tok, self.lexer.token));
		}
	}

	fn expected(&mut self, str: &str) {
		self.msg(self.lexer.span, Msg::Expected(str.to_string(), self.lexer.token));
	}

	fn ident(&mut self) -> Ident {
		Ident(spanned!(self, {
			match self.tok() {
				Token::Name(name) => {
					self.step();
					name
				}
				_ => {
					self.expected("identifier");
					NAME_ERROR
				}
			}
		}))
	}

	fn is_term(&self) -> bool {
		self.is(Token::End) || self.is(Token::Deindent)
	}

	pub fn parse(&mut self) -> Block_<Item_> {
		let ast = noded!(self, { self.items() });

		println!("AST! {}", print::item_block(self.lexer.src, &ast));

		while !self.is(Token::End) {
			println!("Left! {:?}", self.tok());
			self.step();
		}

		ast
	}

	fn entries<F, R>(&mut self, entry: F) -> Block<R> where F : Fn(&mut Self) -> Option<R> {
		let mut list = Block::new();

		loop {
			let e = entry(self);
			match e {
				Some(e) => list.vals.push(e),
				_ => break
			}
			if self.is_term() {
				break
			}
			self.expect(Token::Line);
		}

		list
	}

	fn block_<F, R>(&mut self, baseline: Indent, pos: Option<Indent>, term: F) -> Option<N<R>> where F : FnOnce(&mut Self) -> R {
		if self.is(Token::Line) {
			if self.lexer.indent_newline(baseline, pos) {
				Some(noded!(self, {
					let r = term(self);

					if !self.is(Token::End) {
						self.expect(Token::Deindent);
					}

					r
				}))
			} else {
				None
			}
		} else {
			None
		}
	}

	fn block<T, F>(&mut self, baseline: Indent, f: F) -> Block_<T> where F : Fn(&mut Self) -> Option<T> {
		self.block_(baseline, None, |s| s.entries(f)).unwrap_or(N::new(self.lexer.src, lexer::SPAN_ERROR, Block::new()))
	}

	fn span(&self, start: Span) -> Span {
		Span {
			start: start.start,
			len: if start.start >= self.last_ended { 0 } else { self.last_ended - start.start },
		}
	}

	fn generics(&mut self) -> Generics {
		let params = if self.tok() == Token::Bracket(Bracket::Square, true) {
			self.bracket(Bracket::Square, |parser| {
				parser.seq(Token::Bracket(Bracket::Square, false), |parser| {
					match parser.tok() {
						Token::Name(_) => Some(noded!(parser, TypeParam { name: parser.ident() })),
						_ => None,
					}
				})
			})
		} else {
			None
		};

		Generics {
			params: params.unwrap_or(Vec::new())
		}
	}

	fn items(&mut self) -> Block<Item_> {
		self.entries(Parser::try_item)
	}
	
	fn try_item(&mut self) -> Option<Item_> {
		node_wrap!(self, {
			match self.tok() {
				Token::Name(KW_DATA) => {
					let baseline = self.lexer.indent;
					self.step();
					let ident = self.ident();
					let generics = self.generics();
					let block = self.block(baseline, Parser::try_item);

					Some(Item::Data(ident, generics, block))
				}
				Token::Name(KW_FN) => {
					let baseline = self.lexer.indent;
					self.step();
					let ident = self.ident();
					let generics = self.generics();
					let params = self.bracket_seq(Bracket::Parent, |parser| {
						Some(noded!(parser, {
						let (name, ty) = parser.name_and_type();
						FnParam(name, ty)
						}))
					});
					let returns = if self.tok() == Token::Op(OP_ARROW_RIGHT) {
						self.step();
						self.ty()
					} else {
						noded!(self, Ty::Infer)
					};
					let block = self.block(baseline, Parser::try_expr);

					Some(Item::Fn(FnDef {
						name: ident, 
						generics: generics,
						params: params, 
						returns: returns,
						block: block,
					}))
				}
				_ => None
			}
		})
	}

	fn name_and_type(&mut self) -> (Ident, Ty_) {
		let ty = self.ty();
		match self.tok() {
			Token::Name(_) => {
				(self.ident(), ty)
			}
			_ => {
				match ty.val {
					Ty::Ref(ident, _, None) => {
						(ident, noded!(self, Ty::Infer))
					}
					_ => {
						self.expected("variable name");
						(Ident(spanned!(self, NAME_ERROR)), noded!(self, Ty::Error))
					}
				}
			}
		}
	}

	fn is_ty(&self) -> bool {
		match self.tok() {
			Token::Bracket(_, true) | Token::Name(_) => true,
			_ => false
		}
	}

	fn ty(&mut self) -> Ty_ {
		self.ty_ptr()
	}

	fn ty_ptr(&mut self) -> Ty_ {
		let mut r = self.ty_factor();
		while self.is_op_prefix('*') {
			r = extend!(self, r, {
				self.skip_op_prefix();
				Ty::Ptr(Box::new(r))
			});
		}
		r
	}

	fn ty_factor(&mut self) -> Ty_ {
		noded!(self, {
			match self.tok() {
				Token::Name(_) => {
					let name = self.ident();
					let substs = self.ty_args();
					Ty::Ref(name, NONE, substs)
				}
				_ => {
					self.expected("type");
					Ty::Error
				}
			}
		})
	}

	fn ty_args(&mut self) -> Option<Vec<Ty_>> {
		if self.tok() == Token::Bracket(Bracket::Square, true) {
			self.bracket(Bracket::Square, |parser| {
				parser.seq(Token::Bracket(Bracket::Square, false), |parser| {
					if parser.is_ty() {
						Some(parser.ty())
					} else {
						None
					}
				})
			})
		} else {
			None
		}
	}

	fn try_expr(&mut self) -> Option<Expr_> {
		if self.is_expr() {
			Some(self.expr())
		} else {
			None
		}
	}

	fn is_expr(&self) -> bool {
		match self.tok() {
			Token::Bracket(_, true) | Token::Name(_) | Token::Num(_) => true,
			_ => false
		}
	}

	fn expr(&mut self) -> Expr_ {
		match self.tok() {
			Token::Name(KW_LOOP) => {
				noded!(self, {
					let baseline = self.lexer.indent;
					self.step();
					Expr::Loop(self.block(baseline, Parser::try_expr))
				})
			}
			Token::Name(KW_BREAK) => {
				noded!(self, {
					self.step();
					Expr::Break
				})
			},
			Token::Name(KW_IF) => noded!(self, self._if()),
			Token::Name(KW_RETURN) => {
				noded!(self, {
					self.step();
					Expr::Return(if self.is_expr() {
						Some(Box::new(self.expr()))
					} else {
						None
					})
				})
			}
			_ => self.assign_operator()
		}
	}

	fn assign_operator(&mut self) -> Expr_ {
		let u = self.unary();
		let r = self.prec_operator(u, 0);

		match self.tok() {
			Token::Op(OP_ASSIGN) => {
				extend!(self, r, {
					self.step();
					Expr::Assign(OP_ASSIGN, Box::new(r), Box::new(self.expr()))
				})
			},
			_ => r
		}
	}

	fn get_prec_op(&mut self, min: u32) -> Option<(Op, u32)> {
		match self.tok() {
			Token::Op(op) => {
				self.lexer.src.ctx.op_map.get(&op).and_then(|prec| {
					if *prec < min {
						None
					} else {
						Some((op, *prec))
					}
				})
			}
			_ => None,
		}
	}

	fn prec_operator(&mut self, mut left: Expr_, min: u32) -> Expr_ {
		loop {
			let (op, prec) = match self.get_prec_op(min) {
				Some(v) => v,
				None => break
			};

			self.step();
			let mut right = self.unary();

			loop {
				let next_prec = match self.get_prec_op(prec) {
					Some((_, v)) => v,
					None => break
				};
				right = self.prec_operator(right, next_prec + 1);
			}

			left = extend!(self, left, Expr::BinOp(Box::new(left), op, Box::new(right)))
		}

		left
	}

	fn unary(&mut self) -> Expr_ {
		match self.tok() {
			Token::Op(op @ OP_PLUS) => {
				noded!(self, {
					self.step();
					Expr::UnaryOp(op, Box::new(self.unary()))
				})
			}
			_ => self.chain()
		}
	}

	fn chain(&mut self) -> Expr_ {
		let mut r = self.factor();

		loop {
			match self.tok() {
				Token::Bracket(Bracket::Parent, true) => {
					r = extend!(self, r, {
						let params = self.bracket_seq(Bracket::Parent, |parser| {
							if parser.is_expr() {
								Some(parser.expr())
							} else {
								None
							}
						});

						Expr::Call(Box::new(r), params)
					});
				}
				_ => break
			}
		}

		r
	}

	fn factor(&mut self) -> Expr_ {
		noded!(self, {
			match self.tok() {
				Token::Num(n) => {
					self.step();
					Expr::Num(n)
				}
				Token::Name(_) => {
					let name = self.ident();
					let substs = self.ty_args();
					Expr::Ref(name, NONE, substs)
				}
				_ => {
					self.expected("expression");
					Expr::Error
				}
			}
		})
	}

	fn _if(&mut self) -> Expr {
		let pos = self.lexer.column();
		let baseline = self.lexer.indent;
		self.step();
		let cond = self.expr();

		let block = self.block_(baseline, Some(pos), |s| s.entries(Parser::try_expr)).unwrap_or(N::new(self.lexer.src, lexer::SPAN_ERROR, Block::new()));

		let else_block = if self.is(Token::Line) && self.lexer.peek_ident() == Some(KW_ELSE) {
			Some(Box::new(noded!(self, {
				self.step();
				debug_assert!(self.is(Token::Name(KW_ELSE)));
				let else_baseline = self.lexer.indent;
				self.step();

				if self.is(Token::Name(KW_IF)) {
					self._if()
				} else {
					Expr::Block(self.block(else_baseline, Parser::try_expr))
				}
			})))
		} else {
			None
		};

		Expr::If(Box::new(cond), block, else_block)
	}
}

pub fn parse(src: &Source) -> Block_<Item_> {
	Parser::new(&src).parse()
}

#[cfg(test)]
mod test {
	use std;
	use quickcheck;
	use super::*;

	#[test]
	fn test() {
		fn parser_test(mut xs: String) -> bool {
			parse(&xs);
			true
		}
		quickcheck::quickcheck(parser_test as fn (String) -> bool);
	}
}