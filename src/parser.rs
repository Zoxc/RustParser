use lexer;
use std;
use std::rc::Rc;
use lexer::{Token, Indent, Span, Spanned, Bracket};
use ast::*;
use print;
use misc;
use misc::interned::*;
use misc::{Context, Source, Name};

pub enum Msg {
	Expected(String, Token),
	ExpectedToken(Token, Token),
}

impl Msg {
	pub fn msg(&self, src: &Source) -> String {
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

macro_rules! span_wrap {
    ($this:expr, $c:expr) => {{
		let start = $this.lexer.span;
		$c.map(|v| Spanned::new($this.span(start), v))
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

	pub fn parse(&mut self) {
		self.print(&format!("Tok {:?}", self.lexer.token));

		let ast = self.items();

		println!("AST! {}", print::item_block(self.lexer.src, &Some(Spanned::new(lexer::SPAN_ERROR, ast))));

		while !self.is(Token::End) {
			println!("Left! {:?}", self.tok());
			self.step();
		}

		print!("{}", self.lexer.src.format_msgs());
	}

	fn entries<F, R: std::fmt::Debug>(&mut self, entry: F) -> Vec<R> where F : Fn(&mut Self) -> Option<R> {
		let mut list = Vec::new();

		loop {
			let e = entry(self);
			match e {
				Some(e) => list.push(e),
				_ => break
			}
			if self.is_term() {
				break
			}
			self.expect(Token::Line);
		}

		list
	}

	fn block<F, R>(&mut self, baseline: Indent, pos: Option<Indent>, term: F) -> Option<Spanned<R>> where F : FnOnce(&mut Self) -> R {
		if self.is(Token::Line) {
			if self.lexer.indent_newline(baseline, pos) {
				Some(spanned!(self, {
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

	fn span(&self, start: Span) -> Span {
		Span {
			start: start.start,
			len: self.last_ended - start.start
		}
	}

	fn items(&mut self) -> Vec<Item_> {
		self.entries(Parser::try_item)
	}
	
	fn try_item(&mut self) -> Option<Item_> {
		span_wrap!(self, {
			match self.tok() {
				Token::Name(KW_DATA) => {
					let baseline = self.lexer.indent;
					self.step();
					let ident = self.ident();
					let block = self.block(baseline, None, |s| s.items());

					Some(Item::Data(ident, block))
				}
				Token::Name(KW_FN) => {
					let baseline = self.lexer.indent;
					self.step();
					let ident = self.ident();
					let params = self.bracket_seq(Bracket::Parent, |parser| Some(parser.ident()));
					let block = self.block(baseline, None, |s| s.exprs());

					Some(Item::Fn(ident, params, block))
				}
				_ => None
			}
		})
	}

	fn exprs(&mut self) -> Vec<Expr_> {
		self.entries(Parser::try_expr)
	}
	
	fn expr(&mut self) -> Expr_ {
		match self.try_expr() {
			Some(e) => e,
			_ => {
				self.expected("expression");
				Spanned::new(lexer::SPAN_ERROR, Expr::Error)
			}
		}
	}

	fn try_expr(&mut self) -> Option<Expr_> {
		span_wrap!(self, {
			match self.tok() {
				Token::Name(KW_IF) => Some(self._if()),
				Token::Name(KW_RETURN) => {
					self.step();
					Some(Expr::Return(Box::new(self.expr())))
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
		let block = self.block(baseline, Some(pos), |s| s.entries(Parser::try_expr));

		let else_block = if self.is(Token::Line) && self.lexer.peek_ident() == Some(KW_ELSE) {
			span_wrap!(self, {
				self.step();
				debug_assert!(self.is(Token::Name(KW_ELSE)));
				let else_baseline = self.lexer.indent;
				self.step();

				if self.is(Token::Name(KW_IF)) {
					Some(self._if())
				} else {
					self.block(else_baseline, None, |s| s.entries(Parser::try_expr)).map(|b| Expr::Block(Some(b)))
				}
			}).map(|v| Box::new(v))
		} else {
			None
		};

		Expr::If(Box::new(cond), block, else_block)
	}


}

pub fn parse(src: &str) {
	let src = Source::new(Rc::new(Context::new()), "input".to_string(), src);
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