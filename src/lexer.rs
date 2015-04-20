
use std;
use misc;
use misc::{Source, Context, Name, Op, Num};
use interner::{Interner, Val};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Span {
	pub start: u32,
	pub len: u32
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Spanned<T> {
	pub span: Span,
	pub val: T,
}

impl<T> Spanned<T> {
	pub fn new(span: Span, val: T) -> Spanned<T> {
		Spanned {
			span: span,
			val: val
		}
	}
}

pub enum Msg {
	IllegalTab,
	UnknownChars(String),
}

impl Msg {
	pub fn msg(&self, src: &Source) -> String {
		match *self {
			Msg::IllegalTab => format!("Tab indentation not allowed"),
			Msg::UnknownChars(ref s) => format!("Unknown charater(s) '{}'", s),
		}
	}
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Indent(usize);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Bracket {
	Parent,
	Square,
	Brace,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Token {
	End,
	Line,
	Deindent,
	Num(Num),
	Name(Name),
	Op(Op),
	Bracket(Bracket, bool),
}

#[derive(Copy, Clone)]
struct Block {
	levels: [usize; 3],
	indent: Indent,
}

impl Block {
	fn ignore(&self) -> bool {
		self.levels[0] + self.levels[1] + self.levels[2] != 0
	}
}

pub struct Lexer<'c> {
	pos: &'c u8,
	begin: &'c u8,
	end: &'c u8,
	jump_table: [fn (&mut Lexer<'c>) -> Spanned<Token>; 256],
	pub token: Token,
	pub span: Span,
	blocks: Vec<Block>,
	pub indent: Indent,
	deindent_level: usize,
	pub src: &'c Source,
	pub ctx: &'c Context,
}

fn offset<'c>(start: &'c u8, end: &'c u8) -> usize {
	end as *const u8 as usize - start as *const u8 as usize
}

fn bracket_type(c: u8) -> Bracket {
	match c as char {
		'(' | ')' => Bracket::Parent,
		'[' | ']' => Bracket::Square,
		'{' | '}' => Bracket::Brace,
		_ => panic!()
	}
}

fn is_op(c: u8) -> bool {
	match c as char {
		'+' | '-' | '&' | '*' | '%' | '=' |
		'<' | '>' | '|' | '^' | '~' | ':' |
		'!' | '/' | ',' | '.' => true,
		_ => false
	}
}

macro_rules! spanned {
    ($this:expr, $c:expr) => {{
		let start = $this.pos;
		let val = $c;
		Spanned::new($this.span(start, $this.pos), val)
    }};
}

macro_rules! span {
    ($this:expr, $c:expr) => {{
    	spanned!($this, $c).span
    }};
}

impl<'c> Lexer<'c> {
	pub fn new(source: &'c Source) -> Lexer<'c> {
		let src = source.src.as_bytes();

		let mut result = Lexer {
			token: Token::End,
			span: Span { start: 0, len: 0},
			begin: &src[0],
			end: &src[src.len() - 1],
			jump_table: [Lexer::unknown; 256],
			pos: &src[0],
			blocks: vec![],
			indent: Indent(0),
			deindent_level: 0,
			src: source,
			ctx: &source.ctx,
		};

		assert!(*result.end == 0);

		result.jump_table[0] = Lexer::end;

		macro_rules! set {
		    ($c:expr, $f:expr) => {{
				result.jump_table[$c as usize] = $f;
		    }};
		}

		macro_rules! set_range {
		    ($b:expr, $e:expr, $f:expr) => {{
		    	for c in ($b as usize)..($e as usize + 1) {
					set!(c as u8 as char, $f);
				}
		    }};
		}
		
		for c in 0..128 {
			if is_op(c) {
				set!(c as char, Lexer::op);
			}
		}

		set!(' ', Lexer::whitespace);
		set!(9, Lexer::whitespace);

		set!(10, Lexer::newline);
		set!(13, Lexer::carrage_return);

		set!('{', Lexer::push);
		set!('[', Lexer::push);
		set!('(', Lexer::push);

		set!('}', Lexer::pop);
		set!(']', Lexer::pop);
		set!(')', Lexer::pop);

		set!('_', Lexer::ident);
		set_range!('A', 'Z', Lexer::ident);
		set_range!('a', 'z', Lexer::ident);

		set_range!('0', '9', Lexer::num);

		result.indent = result.get_line_indent();

		result.next_token();

		result
	}

	fn msg(&self, span: Span, msg: Msg) {
		self.src.msg(span, misc::Msg::Lexer(msg));
	}

	fn to_slice(&self, span: Span) -> &'c [u8] {
		unsafe {
			std::slice::from_raw_parts((self.begin as *const u8 as usize + span.start as usize) as *const u8, span.len as usize)
		}
	}

	fn intern<T: Val + Copy>(&self, interner: &'c Interner<T>, span: Span) -> T {
		interner.intern(std::str::from_utf8(self.to_slice(span)).unwrap())
	}

	fn span(&self, start: &'c u8, end: &'c u8) -> Span {
		Span {
			start: (start as *const u8 as usize - self.begin as *const u8 as usize) as u32,
			len: offset(start, end) as u32,
		}
	}

	fn succ(&self) -> &'c u8 {
		debug_assert!(self.pos != self.end);
		unsafe {
			std::mem::transmute((self.pos as *const u8).offset(1))
		}
	}

	fn step(&mut self) {
		self.pos = self.succ();
	}

	fn c(&self) -> u8 {
		*self.pos
	}

	fn is(&self, test: u8) -> bool {
		self.c() == test
	}

	pub fn indent_newline(&mut self, baseline: &Indent) -> bool {
		debug_assert!(self.token == Token::Line);

		let indent = self.get_line_indent();

		if indent.0 > baseline.0 {
			self.blocks.push(Block { levels: [0; 3], indent: *baseline });
			self.indent = indent;
			self.next_token();

			true
		} else {
			false
		}
	}

	fn skip_newline(&mut self) {
		loop {
			match self.c() {
				0 => {
					if self.at_end() {
						break;
					}
					else {
						self.msg(self.span(self.pos, self.succ()), Msg::UnknownChars("\\x00".to_string()));
						self.step();
					}
				}
				13 => {
					self.step();
					if self.is(10) {
						self.step()
					}
					break;
				}
				10 => {
					self.step();
					break;
				}
				_ => self.step()
			}
		}
	}

	fn get_line_indent(&mut self) -> Indent {
		let start = self.pos;

		while self.is(32) {
			self.step()
		}

		if self.is(9) {
			self.msg(self.span(self.pos, self.succ()), Msg::IllegalTab);
		}
		self.skip_whitespace();

		match self.c() as char {
			'\x0D' => {
				self.step();
				if self.c() == 10 {
					self.step();
				}
				self.get_line_indent()
			}
			'\x0A' => {
				self.step();
				self.get_line_indent()
			}
			'#' => {
				self.skip_newline();
				self.get_line_indent()
			}
			_ => Indent(offset(start, self.pos))
		}
	}

	fn handle_line(&mut self) -> Spanned<Token> {
		let mut indent = Indent(0);

		let ret = match self.blocks[..].last() {
			Some(b) => {
				indent = b.indent;
				b.ignore()
			}
			None => true
		};

		if ret {
			return self.next();
		}

		if self.indent.0 <= indent.0 {
			self.blocks.pop();
			let mut i = 1;
			loop {
				match self.blocks[..].last() {
					Some(b) => {
						if b.ignore() {
							break;
						}

						if self.indent.0 > b.indent.0 {
							break;
						}
					}
					_ => break
				}
				i += 1;
				self.blocks.pop();
			}

			self.deindent_level = i * 2;
			spanned!(self, Token::Deindent)
		} else {
			self.next()
		}
 	}

	fn num(&mut self) -> Spanned<Token> {
		let s = span!(self, {
			self.step();

			loop {
				match self.c() as char {
					'_' | '0'...'9' | 'A'...'F' | 'a'...'f' => { self.step() },
					_ => break
				}
			}
		});

		Spanned::new(s, Token::Num(self.intern(&self.ctx.interners.num, s)))
	}

	fn ident(&mut self) -> Spanned<Token> {
		let s = span!(self, {
			self.step();

			loop {
				match self.c() as char {
					'_' | '0'...'9' | 'A'...'Z' | 'a'...'z' => { self.step() },
					_ => break
				}
			}
		});

		Spanned::new(s, Token::Name(self.intern(&self.ctx.interners.name, s)))
	}

	fn op(&mut self) -> Spanned<Token> {
		let s = span!(self, {
			loop {
				self.step();

				if !is_op(self.c()) {
					break
				}
			}
		});

		Spanned::new(s, Token::Op(self.intern(&self.ctx.interners.op, s)))
	}

	fn at_end(&self) -> bool {
		self.pos == self.end
	}

	fn push(&mut self) -> Spanned<Token> {
		spanned!(self, {
			let result = bracket_type(self.c());

			self.step();

			&mut self.blocks[..].last_mut().map(|b| b.levels[result as usize] += 1);

			Token::Bracket(result, true)
		})
	}

	fn pop_bracket_level(&mut self, bracket: Bracket) -> usize {
		let mut i = self.blocks.len();

		loop {
			if i == 0 {
				return 0;
			}

			i -= 1;

			if self.blocks[i].levels[bracket as usize] > 0 {
				self.blocks[i].levels[bracket as usize] -= 1;

				self.blocks.truncate(i + 1);
				return self.blocks.len() - i;
			}
		}
	}

	fn pop(&mut self) -> Spanned<Token> {
		fn pop_impl<'c>(this: &mut Lexer<'c>) -> Token {
			let result = bracket_type(this.c());

			if !this.blocks.is_empty() {
				let p = this.pop_bracket_level(result);
				if p > 0 {
					this.deindent_level = p * 2 - 1;
					return Token::Deindent;
				}
			}
	 
			this.step();

			Token::Bracket(result, false)
		}

		spanned!(self, pop_impl(self))
	}

	fn skip_whitespace(&mut self) {
		loop {
			match self.c() {
				32 | 9 => self.step(),
				_ => break
			}
		}
	}

	fn whitespace(&mut self) -> Spanned<Token> {
		self.step();
		self.skip_whitespace();
		self.next()
	}

	fn newline(&mut self) -> Spanned<Token> {
		self.step();
		self.deindent_level = 0;
		spanned!(self, Token::Line)
	}

	fn carrage_return(&mut self) -> Spanned<Token> {
		self.step();

		if self.is(10) {
			self.step();
		}

		self.deindent_level = 0;
		spanned!(self, Token::Line)
	}

	fn end(&mut self) -> Spanned<Token> {
		if self.at_end() {
			spanned!(self, Token::End)
		} else {
			self.unknown()
		}
	}

	fn unknown(&mut self) -> Spanned<Token> {
		let span = span!(self, {
			loop {
				self.step();

				if self.jump_table[self.c() as usize] as  *const u8 != Lexer::unknown as *const u8 {
					break
				}
			}
		});

		self.msg(span, Msg::UnknownChars(std::str::from_utf8(self.to_slice(span)).unwrap().to_string()));

		self.next()
	}

	fn next(&mut self) -> Spanned<Token> {
		self.jump_table[self.c() as usize](self)
	}

	pub fn next_token(&mut self) {
		let r = match self.token {
			Token::Line => {
				match self.deindent_level {
					0 => {
						self.indent = self.get_line_indent();
						self.handle_line()
					}
					1 => self.next(),
					_ => {
						self.deindent_level -= 1;
						Spanned::new(self.span, Token::Deindent)
					}
				}

			}
			Token::Deindent => {
				if self.deindent_level == 1 {
					self.next()
				} else {
					self.deindent_level -= 1;
					Spanned::new(self.span, Token::Line)
				}
			}
			_ => self.next(),
		};
		self.token = r.val;
		self.span = r.span;
	}
}

#[cfg(test)]
mod test {
	use std;
	use quickcheck;
	use super::*;

	#[test]
	fn test() {
		fn parser_test(xs: String) -> bool {
			use misc::{Context, Source};
			use std::rc::Rc;

			let src = Source::new(Rc::new(Context::new()), "input".to_string(), &xs);
			let mut lexer = Lexer::new(&src);

			loop {
				lexer.next_token();

				if lexer.token == Token::End {
					break
				}
			}
			true
		}
		quickcheck::quickcheck(parser_test as fn (String) -> bool);
	}
}