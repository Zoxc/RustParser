use std::rc::Rc;
use std::cell::RefCell;
use parser;
use lexer;
use lexer::Span;
use interner::{Val, Interner};

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

pub enum Msg {
	Lexer(lexer::Msg),
	Parser(parser::Msg),
}

impl Msg {
	pub fn msg(&self, src: &Source) -> String {
		match *self {
			Msg::Parser(ref msg) => msg.msg(src),
			Msg::Lexer(ref msg) => msg.msg(src),
		}
	}
}

pub struct Message {
	span: Span,
	msg: Msg,
}

pub struct Context {
	pub interners: Interners,
}

impl Context {
	pub fn new() -> Context {
		Context {
			interners: Interners::new(),
		}
	}
}

pub struct Source {
	pub ctx: Rc<Context>,
	pub filename: String,
	pub src: String,
	msgs: RefCell<Vec<Message>>,
}

impl Source {
	pub fn new(ctx: Rc<Context>, filename: String, src: &str) -> Source {
		Source {
			ctx: Rc::new(Context::new()),
			filename: "input".to_string(),
			src: format!("{}\0", src),
			msgs: RefCell::new(Vec::new()),
		}
	}

	pub fn msg(&self, span: Span, msg: Msg) {
		self.msgs.borrow_mut().push(Message {
			span: span,
			msg: msg
		});
	}

	pub fn format_msgs(&self) -> String {
		let m: Vec<String> = self.msgs.borrow().iter().map(|m| format!("{}: {}\n", self.filename, m.msg.msg(self)) ).collect();
		m.concat()
	}
}
