use lexer;
use lexer::{Token, Indent, Span, Spanned, Bracket};
use ast::*;
use print;
use misc;
use misc::interned::*;
use misc::{Source, Name};

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

	fn msg(&self, span: Span, msg: Msg) {
		self.lexer.src.msg(span, misc::Msg::Parser(msg));
	}

	fn tok(&self) -> Token {
		self.lexer.token
	}

	fn is(&self, tok: Token) -> bool {
		self.lexer.token == tok
	}

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
    	self.print(&format!("Tok {:?}", self.lexer.token));
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
		self.print(&format!("Tok {:?}", self.lexer.token));

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
					self.print("New block");
					
					let r = term(self);

					self.print("Done block");

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
			len: self.last_ended - start.start
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
					let block = self.block(baseline, Parser::try_item);

					Some(Item::Data(ident, block))
				}
				Token::Name(KW_FN) => {
					let baseline = self.lexer.indent;
					self.step();
					let ident = self.ident();
					let params = self.bracket_seq(Bracket::Parent, |parser| Some(parser.ident()));
					let block = self.block(baseline, Parser::try_expr);

					Some(Item::Fn(ident, params, block))
				}
				_ => None
			}
		})
	}

	fn exprs(&mut self) -> Block<Expr_> {
		self.entries(Parser::try_expr)
	}
	
	fn expr(&mut self) -> Expr_ {
		match self.try_expr() {
			Some(e) => e,
			_ => {
				self.expected("expression");
				N::new(self.lexer.src, lexer::SPAN_ERROR, Expr::Error)
			}
		}
	}

	fn try_expr(&mut self) -> Option<Expr_> {
		node_wrap!(self, {
			match self.tok() {
				Token::Name(KW_IF) => Some(self._if()),
				Token::Name(KW_RETURN) => {
					self.step();
					Some(Expr::Return(Box::new(self.expr())))
				}
				Token::Name(_) => {
					Some(Expr::Ref(self.ident(), NONE))
				}
				_ => None
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
			node_wrap!(self, {
				self.step();
				debug_assert!(self.is(Token::Name(KW_ELSE)));
				let else_baseline = self.lexer.indent;
				self.step();

				if self.is(Token::Name(KW_IF)) {
					Some(self._if())
				} else {
					Some(Expr::Block(self.block(else_baseline, Parser::try_expr)))
				}
			}).map(|v| Box::new(v))
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