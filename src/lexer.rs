use std;
use interner::{Interner, Val};

macro_rules! intern_type {
    ($n:ident) => {

		#[derive(Copy, Clone, PartialEq, Eq, Debug)]
		pub struct $n(u32);

		impl Val for $n {
			fn new(val: u32) -> $n {
				$n(val)
			}

			fn usize(&self) -> usize {
				self.0 as usize
			}
		}
    };
}

intern_type!(Name);
intern_type!(Op);
intern_type!(Num);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Indent(usize);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Bracket {
	Parent,
	Square,
	Brace,
}

#[derive(PartialEq, Eq, Debug)]
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

#[derive(PartialEq, Eq)]
enum TokenAction {
	None,
	Line,
	Deindent
}

pub struct Interners {
	pub name: Interner<Name>,
	pub op: Interner<Op>,
	pub num: Interner<Num>,
}

impl Interners {
	pub fn new() -> Interners {
		Interners {
			name: Interner::new(),
			op: Interner::new(),
			num: Interner::new(),
		}
	}
}

pub struct Context<'c> {
	end: &'c u8,
	jump_table: [fn (&mut Context<'c>) -> Token; 256],
	pos: &'c u8,
	start: &'c u8,
	last_ended: &'c u8,
	blocks: Vec<Block>,
	indent: Indent,
	deindent_level: usize,
	action: TokenAction,
	pub interners: &'c Interners,
}

fn offset<'c>(start: &'c u8, end: &'c u8) -> usize {
	end as *const u8 as usize - start as *const u8 as usize
}


fn slice<'c>(start: &'c u8, end: &'c u8) -> &'c [u8] {
	unsafe {
		std::slice::from_raw_parts(start as *const u8, offset(start, end))
	}
}

fn str<'c>(start: &'c u8, end: &'c u8) -> String {
	String::from_utf8_lossy(slice(start, end)).into_owned()
}

fn intern<'c, T: Val + Copy>(interner: &'c Interner<T>, start: &'c u8, end: &'c u8) -> T {
	interner.intern(std::str::from_utf8(slice(start, end)).unwrap())
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
		'!' | '/'  | ',' => true,
		_ => false
	}
}

impl<'c> Context<'c> {
	pub fn new(src: &'c str, interners: &'c Interners) -> Context<'c> {
		let src = src.as_bytes();

		let mut result = Context {
			end: &src[src.len() - 1],
			jump_table: [Context::unknown; 256],
			pos: &src[0],
			start: &src[0],
			last_ended: &src[0],
			blocks: vec![],
			indent: Indent(0),
			deindent_level: 0,
			action: TokenAction::None,
			interners: interners,
		};

		assert!(*result.end == 0);

		result.jump_table[0] = Context::end;

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
				set!(c as char, Context::op);
			}
		}

		set!(' ', Context::whitespace);
		set!(9, Context::whitespace);

		set!(10, Context::newline);
		set!(13, Context::carrage_return);

		set!('{', Context::push);
		set!('[', Context::push);
		set!('(', Context::push);

		set!('}', Context::pop);
		set!(']', Context::pop);
		set!(')', Context::pop);

		set!('_', Context::ident);
		set_range!('A', 'Z', Context::ident);
		set_range!('a', 'z', Context::ident);

		set_range!('0', '9', Context::num);

		result.indent = result.get_line_indent();

		result
	}

	fn succ(&self) -> &'c u8 {
		assert!(self.pos != self.end);
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
		assert!(self.action == TokenAction::Line);

		let indent = self.get_line_indent();

		if indent.0 > baseline.0 {
			self.blocks.push(Block { levels: [0; 3], indent: *baseline });
			self.indent = indent;
			self.next();

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
						panic!("err");
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
			panic!("tab indentation not allowed");
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

	fn handle_line(&mut self) -> Token {
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

			self.start = self.pos;
			self.action = TokenAction::Deindent;
			self.deindent_level = i * 2;
			Token::Deindent
		} else {
			self.next()
		}
 	}

	fn num(&mut self) -> Token {
		self.step();

		loop {
			match self.c() as char {
				'_' | '0'...'9' | 'A'...'F' | 'a'...'f' => { self.step() },
				_ => break
			}
		}

		Token::Num(intern(&self.interners.num, self.start, self.pos))
	}

	fn ident(&mut self) -> Token {
		self.step();

		loop {
			match self.c() as char {
				'_' | '0'...'9' | 'A'...'Z' | 'a'...'z' => { self.step() },
				_ => break
			}
		}

		Token::Name(intern(&self.interners.name, self.start, self.pos))
	}

	fn at_end(&self) -> bool {
		self.pos == self.end
	}

	fn push(&mut self) -> Token {
		let result = bracket_type(self.c());

		self.step();

		&mut self.blocks[..].last_mut().map(|b| b.levels[result as usize] += 1);

		Token::Bracket(result, true)
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

	fn pop(&mut self) -> Token {
		let result = bracket_type(self.c());

		if !self.blocks.is_empty() {
			let p = self.pop_bracket_level(result);
			if p > 0 {
				self.deindent_level = p * 2 - 1;
				self.action = TokenAction::Deindent;
				return Token::Deindent;
			}
		}
 
		self.step();

		Token::Bracket(result, false)
	}

	fn op(&mut self) -> Token {
		loop {
			self.step();

			if !is_op(self.c()) {
				break
			}
		}

		Token::Op(intern(&self.interners.op, self.start, self.pos))
	}

	fn skip_whitespace(&mut self) {
		loop {
			match self.c() {
				32 | 9 => self.step(),
				_ => break
			}
		}
	}

	fn whitespace(&mut self) -> Token {
		self.step();
		self.skip_whitespace();
		self.next()
	}

	fn newline(&mut self) -> Token {
		self.step();
		self.deindent_level = 0;
		self.action = TokenAction::Line;
		Token::Line
	}

	fn carrage_return(&mut self) -> Token {
		self.step();

		if self.is(10) {
			self.step();
		}

		self.deindent_level = 0;
		self.action = TokenAction::Line;
		Token::Line
	}

	fn end(&mut self) -> Token {
		if self.at_end() {
			Token::End
		} else {
			self.unknown()
		}
	}

	fn unknown(&mut self) -> Token {
		loop {
			self.step();

			if self.jump_table[self.c() as usize] as  *const u8 != Context::unknown as *const u8 {
				break
			}
		}

    	println!("Unknown tokens {}", str(self.start, self.pos));

		self.next()
	}

	fn next(&mut self) -> Token {
		self.action = TokenAction::None;
		self.last_ended = self.pos;
		self.start = self.pos;
		self.jump_table[self.c() as usize](self)
	}

	pub fn next_token(&mut self) -> Token {
		match self.action {
			TokenAction::None => self.next(),
			TokenAction::Line => {
				match self.deindent_level {
					0 => {
						self.indent = self.get_line_indent();
						self.handle_line()
					}
					1 => self.next(),
					_ => {
						self.deindent_level -= 1;
						self.action = TokenAction::Deindent;
						Token::Deindent
					}
				}

			}
			TokenAction::Deindent => {
				if self.deindent_level == 1 {
					self.next()
				} else {
					self.deindent_level -= 1;
					self.action = TokenAction::Line;
					Token::Line
				}
			}
		}
	}
}
