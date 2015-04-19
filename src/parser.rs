use lexer;
use interner::{Val, Interner, RcStr};
use std::borrow::Borrow;
use lexer::{Interners, Token, Indent, Name, Op};

pub fn token_parser(src: &str) {
	let i = Interners::new();
	let src = format!("{}\0", src);

	let mut ctx = lexer::Context::new(&src, &i);

	loop {
		let token = ctx.next_token();

		if token == Token::End {
			break
		}


    	println!("{:?}", token);
	}
}

struct Parser<'c> {
	lexer: lexer::Context<'c>,
	curr: Token,
}

fn get<T: Val + Copy>(interner: &Interner<T>, val: T) -> RcStr {
	interner.get(val)
}

impl<'c> Parser<'c> {
	pub fn new(src: &'c str, interners: &'c Interners) -> Parser<'c> {
		let mut lexer = lexer::Context::new(src, interners);
		let curr = lexer.next_token();

		Parser {
			lexer: lexer,
			curr: curr,
		}
	}

	fn step(&mut self) {
		self.curr = self.lexer.next_token()
	}

	fn expect(&mut self, tok: Token) {
		if self.curr == tok {
			self.step()
		} else {
			panic!("expected token!");
		}
	}

	fn is_term(&self) -> bool {
		self.curr == Token::End || self.curr == Token::Deindent
	}

	pub fn parse(&mut self) {
		self.entries(Parser::global);


		while self.curr != Token::End {
	    	println!("Left! {:?}", self.curr);
	    	self.step();
		}
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
		match self.curr {
			Token::Name(s) => {
				match get(&self.lexer.interners.name, s).borrow() {
					"data" => {
						self.step();
						Some(true)
					}
					_ => None
				}
			}
			_ => None
		}
	}

	fn scope<F, R>(&mut self, baseline: Indent, term: F) -> Option<R> where F : FnOnce(&mut Self) -> R {
		if self.curr == Token::Line {
			if self.lexer.indent_newline(&baseline) {
				let r = term(self);

				if self.curr != Token::End {
					self.expect(Token::Deindent);
				}

				return Some(r);
			}
		}

		None
	}
}

pub fn parse(src: &str) {
	let i = Interners::new();
	let src = format!("{}\0", src);
	Parser::new(&src, &i).parse()
}