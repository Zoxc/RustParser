use lexer;
use std::rc::Rc;
use lexer::{Token, Indent, Span};
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

	fn step(&mut self) {
		self.last_ended = self.lexer.span.start + self.lexer.span.len;
		self.lexer.next_token();
    	println!("Tok {:?}", self.lexer.token);
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

	fn global(&mut self) -> Option<bool> {
		match self.tok() {
			Token::Name(KW_DATA) => {
				let baseline = self.lexer.indent;
				self.step();
				self.scope(baseline, |s| s.entries(Parser::global));
				Some(true)
			}
			_ => None
		}
	}

	fn scope<F, R>(&mut self, baseline: Indent, term: F) -> Option<R> where F : FnOnce(&mut Self) -> R {
		if self.is(Token::Line) {
			if self.lexer.indent_newline(&baseline) {
				let r = term(self);

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