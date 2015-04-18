use std;

#[derive(PartialEq, Eq, Debug)]
struct Name {
	val: String
}

#[derive(PartialEq, Eq, Debug)]
struct Op {
	val: String
}

#[derive(PartialEq, Eq, Debug)]
struct Indent {
	val: String
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Bracket {
	Parent,
	Square,
	Brace,
}

#[derive(PartialEq, Eq, Debug)]
enum Token {
	End,
	Line,
	Indent,
	Deindent,
	Number(String),
	Name(Name),
	Op(Op),
	Bracket(Bracket, bool),
}

struct Block {
	levels: [usize; 3],
	indent: Indent
}

struct State<'c> {
	ctx: &'c Context<'c>,
	pos: &'c u8,
	start: &'c u8,
	last_ended: &'c u8,
	blocks: Vec<Block>,
	indent: Indent,
	deindent_level: usize
}

fn slice<'c>(start: &'c u8, end: &'c u8) -> &'c [u8] {
	unsafe {
		std::slice::from_raw_parts(start as *const u8,
			end as *const u8 as usize - start as *const u8 as usize)
	}
}

fn str<'c>(start: &'c u8, end: &'c u8) -> String {
	String::from_utf8_lossy(slice(start, end)).into_owned()
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
		'+' | '-' | '&' | '*' | '%' | '=' | '<' | '>' | '|' | '^' | '~' | ':' | '!' | '/' => true,
		_ => false
	}
}

impl<'c> State<'c> {
	fn succ(&self) -> &'c u8 {
		assert!(self.pos != self.ctx.end);
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

	fn num(&mut self) -> Token {
		self.step();

		loop {
			match self.c() as char {
				'_' | '0'...'9' | 'A'...'F' | 'a'...'f' => { self.step() },
				_ => break
			}
		}

		Token::Number(str(self.start, self.pos))
	}

	fn ident(&mut self) -> Token {
		self.step();

		loop {
			match self.c() as char {
				'_' | '0'...'9' | 'A'...'Z' | 'a'...'z' => { self.step() },
				_ => break
			}
		}

		Token::Name(Name { val: str(self.start, self.pos) })
	}

	fn at_end(&self) -> bool {
		self.pos == self.ctx.end
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

		Token::Op(Op { val: str(self.start, self.pos) })
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
		Token::Line
	}

	fn carrage_return(&mut self) -> Token {
		self.step();

		if self.is(10) {
			self.step();
		}

		self.deindent_level = 0;
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

			if self.ctx.jump_table[self.c() as usize] as  *const u8 != State::unknown as *const u8 {
				break
			}
		}

    	println!("Unknown tokens {}", str(self.start, self.pos));

		self.next()
	}

	fn next(&mut self) -> Token {
		self.last_ended = self.pos;
		self.start = self.pos;
		self.ctx.jump_table[self.c() as usize](self)
	}
}

type Handler<'c> = fn (&mut State<'c>) -> Token;

struct Context<'c> {
	src: &'c [u8],
	end: &'c u8,
	jump_table: [Handler<'c>; 256],
}


impl<'c> Context<'c> {
	fn new(src: &'c str) -> Context<'c> {
		let src = src.as_bytes();
		let mut result = Context {
			src: src,
			end: &src[src.len() - 1],
			jump_table: [State::unknown; 256]
		};

		result.jump_table[0] = State::end;

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
				set!(c as char, State::op);
			}
		}

		set!(' ', State::whitespace);
		set!(9, State::whitespace);

		set!(10, State::newline);
		set!(13, State::carrage_return);

		set!('{', State::push);
		set!('[', State::push);
		set!('(', State::push);

		set!('}', State::pop);
		set!(']', State::pop);
		set!(')', State::pop);

		set!('_', State::ident);
		set_range!('A', 'Z', State::ident);
		set_range!('a', 'z', State::ident);

		set_range!('0', '9', State::num);

		result
	}

	fn new_state(&'c self) -> State<'c> {
		State {
			ctx: self,
			pos: &self.src[0],
			start: &self.src[0],
			last_ended: &self.src[0],
			blocks: vec![],
			indent: Indent { val: "".to_string() },
			deindent_level: 0
		}
	}
}

pub fn lex(src: &str) {
	let src = format!("{}\0", src);
	let ctx = Context::new(&src);

	let mut st = ctx.new_state();

	loop {
		let token = st.next();

		if token == Token::End {
			break
		}


    	println!("{:?}", token);
	}
}