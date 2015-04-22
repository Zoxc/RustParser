use lexer;
use std::rc::Rc;
use lexer::{Token, Indent, Span, Spanned, Bracket};
use misc;
use misc::interned::*;
use misc::{Context, Source, Name};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Ident(pub Spanned<Name>);

pub type Block<T> = Option<Spanned<Vec<T>>>;

pub type Item_ = Spanned<Item>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Item {
	Data(Ident, Block<Item_>),
	Fn(Ident, Vec<Ident>, Block<Expr_>),
}

pub type Expr_ = Spanned<Expr>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expr {
	Error,
	If(Box<Expr_>, Block<Expr_>, Option<Box<Expr_>>),
	Return(Box<Expr_>),
	Block(Block<Expr_>),
}
