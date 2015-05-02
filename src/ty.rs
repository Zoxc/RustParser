use misc::Name;
use ast::Id;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct Var(pub u32);

#[derive(Clone, PartialEq, Eq)]
pub struct Substs<'t>(pub Vec<Ty<'t>>);

pub type Ty<'t> = &'t Ty_<'t>;

#[derive(Clone, PartialEq, Eq)]
pub enum Ty_<'t> {
	Error,
	Int,
	Tuple(Vec<Ty<'t>>),
	Infer(Var),
	Param(Name),
	Ref(Id, Substs<'t>),
}

#[derive(Clone, PartialEq, Eq)]
pub struct TyParam<'t> {
	pub name: Name,
	pub hm: Ty<'t>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Scheme<'t> {
	pub ty: Ty<'t>,
	pub params: Vec<TyParam<'t>>,
}
