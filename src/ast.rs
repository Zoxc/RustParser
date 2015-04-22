use lexer;
use std::rc::Rc;
use lexer::{Token, Indent, Span, Spanned, Bracket};
use misc;
use misc::interned::*;
use misc::{Context, Source, Name};

pub struct Ident(pub Spanned<Name>);

pub type Block<T> = Option<Spanned<Vec<T>>>;

pub type Item_ = Spanned<Item>;
pub enum Item {
	Data(Ident, Block<Item_>)
}

pub type Expr_ = Spanned<Expr>;
pub enum Expr {
	Error,
	If(Box<Expr_>, Block<Expr_>, Option<Box<Expr_>>),
	Return(Box<Expr_>),
	Block(Block<Expr_>),
}
