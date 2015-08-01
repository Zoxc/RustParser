
use std;
use misc;
use misc::{Source, Context, Name, Op, Num};
use interner::{Interner, Val};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Span {
	pub start: usize,
	pub len: usize
}

pub const SPAN_ERROR: Span = Span {
	start: -1i32 as usize,
	len: 0
};

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
	pub fn msg(&self, _ctx: &Context) -> String {
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
	fn new(indent: Indent) -> Block {
		Block { levels: [0; 3], indent: indent }
	}

	fn ignore(&self) -> bool {
		self.levels[0] + self.levels[1] + self.levels[2] != 0
	}
}

pub struct Lexer<'c> {
	pos: &'c u8,
	begin: &'c u8,
	end: &'c u8,
	line_start: &'c u8,
	jump_table: [fn (&mut Lexer<'c>) -> Spanned<Token>; 256],
	pub token: Token,
	pub span: Span,
	blocks: Vec<Block>,
	pub indent: Indent,
	deindent_bracket: bool,
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

fn is_ident(c: u8) -> bool {
	match c as char {
		'_' | '0'...'9' | 'A'...'Z' | 'a'...'z' => true,
		_ => false
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
	pub fn new(ctx: &'c Context, source: &'c Source) -> Lexer<'c> {
		let src = source.src.as_bytes();

		let mut result: Lexer = Lexer {
			token: Token::End,
			span: Span { start: source.span_start, len: 0},
			begin: &src[0],
			line_start: &src[0],
			end: &src[src.len() - 1],
			jump_table: [Lexer::unknown; 256],
			pos: &src[0],
			blocks: vec![Block::new(Indent(0))],
			indent: Indent(0),
			deindent_bracket: false,
			deindent_level: 0,
			src: source,
			ctx: ctx,
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

		let mut pos = result.pos;
		result.indent = result.get_line_indent(&mut pos, true).0;
		result.pos = pos;

		result.next_token();

		result
	}

	pub fn column(&self) -> Indent {
		Indent((self.span.start as usize - self.src.span_start) - offset(self.begin, self.line_start))
	}

	fn msg(&self, span: Span, msg: Msg) {
		self.src.msg(span, misc::Msg::Lexer(msg));
	}

	fn to_slice(&self, span: Span) -> &'c [u8] {
		unsafe {
			std::slice::from_raw_parts((self.begin as *const u8 as usize + span.start as usize - self.src.span_start) as *const u8, span.len as usize)
		}
	}

	pub fn intern<T: Val + Copy>(&self, interner: &'c Interner<T>, span: Span) -> T {
		interner.intern(std::str::from_utf8(self.to_slice(span)).unwrap())
	}

	fn span(&self, start: &'c u8, end: &'c u8) -> Span {
		Span {
			start: self.src.span_start + start as *const u8 as usize - self.begin as *const u8 as usize,
			len: offset(start, end),
		}
	}

	fn succ(&self, pos: &'c u8) -> &'c u8 {
		debug_assert!(pos != self.end);
		unsafe {
			std::mem::transmute((pos as *const u8).offset(1))
		}
	}

	fn step_(&self, pos: &mut &'c u8) {
		*pos = self.succ(*pos);
	}

	fn step(&mut self) {
		self.pos = self.succ(self.pos);
	}

	fn c(&self) -> u8 {
		*self.pos
	}

	fn is(&self, test: u8) -> bool {
		self.c() == test
	}

	pub fn indent_newline(&mut self, baseline: Indent, fallback: Option<Indent>) -> bool {
		debug_assert!(self.token == Token::Line);
		debug_assert!(fallback.map(|i| baseline.0 <= i.0).unwrap_or(true));

		if self.indent.0 > baseline.0 {
			self.blocks.push(Block::new(fallback.unwrap_or(baseline)));
			self.next_token();

			true
		} else {
			false
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

			while is_ident(self.c()) {
				self.step();
			}
		});

		Spanned::new(s, Token::Name(self.intern(&self.ctx.interners.name, s)))
	}

	pub fn peek_ident(&self) -> Option<Name> {
		let mut pos = self.pos;
		self.get_line_indent(&mut pos, false);

		let start = pos;

		if !is_ident(*pos) {
			return None;
		} else {
			self.step_(&mut pos);
		}

		while is_ident(*pos) {
			self.step_(&mut pos);
		}

		Some(self.intern(&self.ctx.interners.name, self.span(start, pos)))
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

				let r = self.blocks.len() - i - 1;
				self.blocks.truncate(i + 1);
				return r;
			}
		}
	}

	fn pop(&mut self) -> Spanned<Token> {
		spanned!(self, {
			let result = bracket_type(self.c());
			let p = self.pop_bracket_level(result);
			if p > 0 {
				self.deindent_level = p;
				self.deindent_bracket = true;
				Token::Deindent
			} else {
				self.step();
				Token::Bracket(result, false)
			}
		})
	}

	fn skip_whitespace(&self, pos: &mut &'c u8) {
		loop {
			match **pos {
				32 | 9 => self.step_(pos),
				_ => break
			}
		}
	}

	fn whitespace(&mut self) -> Spanned<Token> {
		self.step();
		let mut pos = self.pos;
		self.skip_whitespace(&mut pos);
		self.pos = pos;
		self.next()
	}

	fn can_deindent(&mut self) -> bool {
		let b = self.blocks[..].last().unwrap();

		if b.ignore() {
			false
		} else {
			self.blocks.len() > 1 && self.indent.0 <= b.indent.0 
		}
	}

	fn skip_newline(&self, pos: &mut &'c u8, report: bool) {
		loop {
			match **pos {
				0 => {
					if self.end == *pos {
						break;
					}
					else {
						if report {
							self.msg(self.span(*pos, self.succ(*pos)), Msg::UnknownChars("\\x00".to_string()));
						}
						self.step_(pos);
					}
				}
				13 => {
					self.step_(pos);
					if **pos == 10 {
						self.step_(pos)
					}
					break;
				}
				10 => {
					self.step_(pos);
					break;
				}
				_ => self.step_(pos)
			}
		}
	}

	fn get_line_indent(&self, pos: &mut &'c u8, report: bool) -> (Indent, &'c u8) {
		let start = *pos;

		while **pos == 32 {
			self.step_(pos);
		}

		if **pos == 9 && report {
			self.msg(self.span(*pos, self.succ(*pos)), Msg::IllegalTab);
		}
		self.skip_whitespace(pos);

		match **pos as char {
			'\x0D' => {
				self.step_(pos);
				if **pos == 10 {
					self.step_(pos);
				}
				self.get_line_indent(pos, report)
			}
			'\x0A' => {
				self.step_(pos);
				self.get_line_indent(pos, report)
			}
			'#' => {
				self.skip_newline(pos, report);
				self.get_line_indent(pos, report)
			}
			_ => (Indent(offset(start, *pos)), start)
		}
	}

	fn make_line(&mut self) -> Spanned<Token> {
		let mut pos = self.pos;
		let (indent, line_start) = self.get_line_indent(&mut pos, true);
		self.pos = pos;
		self.indent = indent;
		self.line_start = line_start;

		let mut i = 0;

		while self.can_deindent() {
			self.blocks.pop();
			i += 1;
		}

		self.deindent_bracket = false;
		self.deindent_level = i;
		spanned!(self, if i > 0 { Token::Deindent } else { Token::Line })
	}

	fn newline(&mut self) -> Spanned<Token> {
		self.step();
		self.make_line()
	}

	fn carrage_return(&mut self) -> Spanned<Token> {
		self.step();

		if self.is(10) {
			self.step();
		}

		self.make_line()
	}

	fn end(&mut self) -> Spanned<Token> {
		if self.pos == self.end {
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
			Token::Deindent => {
				if self.deindent_level == 1 {
					if self.deindent_bracket {
						spanned!(self, {
							let result = bracket_type(self.c());
							self.step();
							Token::Bracket(result, false)
						})
					} else {
						spanned!(self, Token::Line)
					}
				} else {
					self.deindent_level -= 1;
					Spanned::new(self.span, Token::Deindent)
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