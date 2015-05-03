use misc::Name;
use ast::Id;

#[derive(Clone, PartialEq, Eq)]
pub struct Substs<'t>(pub Vec<Ty<'t>>);

pub type Ty<'t> = &'t Ty_<'t>;

#[derive(Clone, PartialEq, Eq)]
pub enum Ty_<'t> {
	Error,
	Int,
	Tuple(Vec<Ty<'t>>),
	Infer(u32),
	Param(Id),
	Fn(Vec<Ty<'t>>, Ty<'t>),
	Ref(Id, Substs<'t>),
}

impl<'t> Ty_<'t> {
	pub fn occurs_in_list(&'t self, list: &[Ty<'t>]) -> bool {
		list.iter().any(|i| self.occurs_in(i))
	}

	pub fn occurs_in(&'t self, t: Ty<'t>) -> bool {
		if self == t {
			return true;
		}

		match *t {
			Ty_::Error | Ty_::Int | Ty_::Param(_) | Ty_::Infer(_) => false,
			Ty_::Tuple(ref vec) => self.occurs_in_list(&vec[..]),
			Ty_::Fn(ref args, ret) => self.occurs_in_list(&args[..]) || self.occurs_in(ret),
			Ty_::Ref(_, ref substs) => self.occurs_in_list(&substs.0[..]),
		}
	}
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
