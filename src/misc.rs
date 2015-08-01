use std;
use std::cell::RefCell;
use std::collections::HashMap;
use parser;
use lexer;
use infer;
use lexer::Span;
use interner::{Val, Interner};
use ast::{Id, Block_, Item_};

macro_rules! intern_type {
    ($n:ident) => {

		#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
		pub struct $n(pub u32);

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

pub struct Interners {
	pub name: Interner<Name>,
	pub op: Interner<Op>,
	pub num: Interner<Num>,
}

pub enum Msg {
	Lexer(lexer::Msg),
	Parser(parser::Msg),
	Resolution(Name),
	Infer(infer::Msg)
}

impl Msg {
	pub fn msg(&self, ctx: &Context) -> String {
		match *self {
			Msg::Parser(ref msg) => msg.msg(ctx),
			Msg::Lexer(ref msg) => msg.msg(ctx),
			Msg::Infer(ref msg) => msg.msg(ctx),
			Msg::Resolution(name) => format!("Unknown identifier '{}'", ctx.get_name(name)),
		}
	}
}

pub struct Message {
	span: Span,
	msg: Msg,
}

macro_rules! count_exprs {
    ($head:ident+) => (1);
    ($head:ident+ $($tail:ident+)*) => (1 + count_exprs!($($tail+)*));
}

macro_rules! interned_consts {
    ($t:path: $c:expr; $n:ident+) => {
    	pub const $n: $t = $t(($c) - 1);
    };
    ($t:path: $c:expr; $n:ident+ $($tail:ident+)*) => {
    	pub const $n: $t = $t(($c) - (count_exprs!($($tail+)*)) - 1);
    	interned_consts!($t: $c; $($tail+)*);
    };
}

macro_rules! interned {
    ($t:path: $i:ident: $($n:ident, $x:expr; )* ) => {
        interned_consts!($t: count_exprs!($($n+)*); $($n+)*);

        fn $i() -> Interner<$t> {
        	let interner = Interner::new();
        	$(
        		let n = interner.intern($x);
        		debug_assert!($n == n);
        	)*
        	interner
        }
    };
}

pub mod interned {
	use super::*;
	use interner::Interner;

	interned!(Name: make_name_interner:
		NAME_ERROR, "<error>";
		KW_DATA, "data";
		KW_IF, "if";
		KW_FN, "fn";
		KW_RETURN, "return";
		KW_ELSE, "else";
		KW_USE, "use";
		KW_BREAK, "break";
		KW_LOOP, "loop";
		KW_CONTINUE, "continue";
	);

	interned!(Op: make_op_interner:
		OP_COMMA, ",";
		OP_STAR, "*";
		OP_PLUS, "+";
		OP_ASSIGN, "=";
		OP_EQ, "==";
		OP_ARROW_RIGHT, "->";
	);

	pub fn new_interners() -> Interners {
		Interners {
			name: interned::make_name_interner(),
			op: interned::make_op_interner(),
			num: Interner::new(),
		}
	}
}

pub type OpInfo = u32;

pub struct Context {
	pub interners: Interners,
	pub op_map: HashMap<Op, OpInfo>,
	pub srcs: Vec<Source>,
	id_count: RefCell<u32>,
}

impl Context {
	fn op_map() -> HashMap<Op, OpInfo> {
		let mut map = HashMap::new();
		let mut i = 0;
		map.insert(interned::OP_EQ, i);
		i += 1;
		map.insert(interned::OP_PLUS, i);
		i += 1;
		map.insert(interned::OP_STAR, i);
		map
	}

	pub fn new() -> Context {
		Context {
			interners: interned::new_interners(),
			op_map: Context::op_map(),
			srcs: Vec::new(),
			id_count: RefCell::new(0),
		}
	}

	pub fn new_id(&self) -> Id {
		*self.id_count.borrow_mut() += 1;
		Id(*self.id_count.borrow_mut())
	}

	pub fn get_name(&self, n: Name) -> String {
		self.interners.name.get(n).to_string()
	}

	pub fn get_num(&self, n: Num) -> String {
		self.interners.num.get(n).to_string()
	}

	pub fn get_op(&self, o: Op) -> String {
		self.interners.op.get(o).to_string()
	}

	pub fn failed(&self) -> bool {
		self.srcs.iter().fold(false, |v, src| {
		    if src.has_msgs() {
		        print!("{}", src.format_msgs(self));
		        true
		    } else {
		    	v
		    }
		})
	}

	pub fn src_from_span<'c>(&'c self, sp: Span) -> &'c Source {
		for src in self.srcs.iter().rev() {
			if sp.start as usize >= src.span_start {
				return src;
			}
		}
		panic!("Span without source");
	}

	pub fn get_core(&self, name: &str) -> Id {
		*self.srcs[0].ast.as_ref().unwrap().val.symbols.map.get(&self.interners.name.intern(name)).unwrap()
	}
}

pub struct Source {
	pub filename: String,
	pub src: String,
	pub span_start: usize,
	pub ast: Option<Block_<Item_>>,
	msgs: RefCell<Vec<Message>>,
}

impl Source {
	pub fn create<'c>(ctx: &'c mut Context, filename: String, src: &str) -> usize {
		let s = Source {
			filename: filename,
			src: format!("{}\0", src),
			ast: None,
			span_start: ctx.srcs.last().map(|l| l.span_start + l.src.len()).unwrap_or(0),
			msgs: RefCell::new(Vec::new()),
		};
		ctx.srcs.push(s);
		ctx.srcs.len() - 1
	}

	pub fn msg(&self, span: Span, msg: Msg) {
		self.msgs.borrow_mut().push(Message {
			span: span,
			msg: msg
		});
	}

	fn line_info(&self, pos: usize) -> (usize, usize) {
		let src = self.src.as_bytes();

		let mut c = 0;
		let mut ls = 0;

		let mut line = 1;

		loop {
			if c == pos {
				break;
			}
			if src[c] == 10 {
				line += 1;
				ls = c + 1;
			} else if src[c] == 13 {
				line += 1;
				if src[c + 1] == 10 {
					c += 1;
				}
				ls = c + 1;
			}
			c += 1
		}

		(line, ls)
	}

	fn line_end(&self, mut c: usize) -> usize {
		let src = self.src.as_bytes();

		loop {
			if src.len() - 1 == c {
				break;
			}

			match src[c] {
				10 | 13 => break,
				_ => ()
			}

			c += 1
		}

		c
	}

	pub fn format_span(&self, s: Span) -> String {
		let span_start = s.start as usize - self.span_start;
		let (line_nr, start) = self.line_info(span_start);
		let end = self.line_end(start);
		let line = &self.src[start..end];
		let pos = format!("{}:{}: ", self.filename, line_nr);
		let space = std::iter::repeat(" ").take(span_start - start + pos.len()).collect::<String>();
		let cursor = if s.len > 1 {
			std::iter::repeat("~").take(std::cmp::min(s.len as usize, end - span_start)).collect::<String>()
		} else {
			"^".to_string()
		};
		format!("{}{}\n{}{}\n", pos, line, space, cursor)
	}

	pub fn has_msgs(&self) -> bool {
		!self.msgs.borrow().is_empty()
	}

	pub fn format_msgs(&self, ctx: &Context) -> String {
		let m: Vec<String> = self.msgs.borrow().iter().rev().map(|m| format!("error: {}\n{}", m.msg.msg(ctx), self.format_span(m.span))).collect();
		m.concat()
	}
}
