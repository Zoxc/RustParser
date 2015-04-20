use lexer;
use std::rc::Rc;
use lexer::{Token, Indent, Span, Bracket};
use misc;
use misc::interned::*;
use misc::{Context, Source, Name};

pub fn token_parser(src: &str) {
	let src = Source::new(Rc::new(Context::new()), "input".to_string(), src);

	let mut ctx = lexer::Lexer::new(&src);

	loop {
		ctx.next_token();

		if ctx.token == Token::End {
			break
		}


    	println!("{:?}", ctx.token);
	}
}

pub enum Msg {
	ExpectedToken(Token, Token)
}

impl Msg {
	pub fn msg(&self, src: &Source) -> String {
		match *self {
			Msg::ExpectedToken(expected, found) => format!("Expected {:?}, but found {:?}", expected, found),
		}
	}
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

	fn step(&mut self) {
		self.last_ended = self.lexer.span.start + self.lexer.span.len;
		self.lexer.next_token();
    	self.print(&format!("Tok {:?}", self.lexer.token));
	}

	fn expect(&mut self, tok: Token) {
		if self.is(tok) {
			self.step()
		} else {
			self.msg(self.lexer.span, Msg::ExpectedToken(tok, self.lexer.token));
		}
	}

	fn is_term(&self) -> bool {
		self.is(Token::End) || self.is(Token::Deindent)
	}

	pub fn parse(&mut self) {
    	self.print(&format!("Tok {:?}", self.lexer.token));

		self.entries(Parser::global);


		while !self.is(Token::End) {
	    	println!("Left! {:?}", self.tok());
	    	self.step();
		}

		print!("{}", self.lexer.src.format_msgs());
	}

	fn entries<F, R>(&mut self, entry: F) -> Vec<R> where F : Fn(&mut Self) -> Option<R> {
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

	fn _if(&mut self) -> bool {
		let pos = self.lexer.column();
		let baseline = self.lexer.indent;
		self.step();
		self.scope(baseline, Some(pos), |s| s.entries(Parser::global));

    	self.print(&format!("ELSE Tok {:?}", self.lexer.token));
		if self.is(Token::Line) && self.lexer.peek_ident() == Some(KW_ELSE) {
			self.step();
			debug_assert!(self.is(Token::Name(KW_ELSE)));
			let else_baseline = self.lexer.indent;
			self.step();

			if self.is(Token::Name(KW_IF)) {
				self._if();
			} else {
				self.scope(else_baseline, None, |s| s.entries(Parser::global));
			}
		}

		true
	}

	fn global(&mut self) -> Option<bool> {
		match self.tok() {
			Token::Name(KW_IF) => Some(self._if()),
			Token::Name(KW_DATA) => {
				let baseline = self.lexer.indent;
				self.step();
				self.scope(baseline, None, |s| s.entries(Parser::global));
				Some(true)
			}
			Token::Name(KW_RETURN) => {
				self.step();
				self.global()
			}
			Token::Name(_) => {
				self.step();
				Some(true)
			}
			Token::Bracket(Bracket::Parent, true) => {
				self.step();
				let r = self.global();
				self.expect(Token::Bracket(Bracket::Parent, false));
				r
			}
			_ => None
		}
	}

	fn scope<F, R>(&mut self, baseline: Indent, pos: Option<Indent>, term: F) -> Option<R> where F : FnOnce(&mut Self) -> R {
		if self.is(Token::Line) {
			if self.lexer.indent_newline(baseline, pos) {
    			self.print("New scope");
    			
    			self.print(&format!("Tok {:?}", self.lexer.token));

				let r = term(self);

    			self.print("Done scope");

				if !self.is(Token::End) {
					self.expect(Token::Deindent);
				}


				return Some(r);
			}
		}

		None
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