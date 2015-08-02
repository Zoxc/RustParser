use ast::Id;

pub type Ty<'t> = &'t Ty_<'t>;

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Ty_<'t> {
	Error,
	Tuple(Vec<Ty<'t>>),
	Infer(usize),
	Fn(Vec<Ty<'t>>, Ty<'t>),
	Kind(Id),
	Ref(Id, Vec<Ty<'t>>),
	Proj(Id, Vec<Ty<'t>>, Id),
	Ptr(Ty<'t>),
}

impl<'t> Ty_<'t> {
	pub fn occurs_in_list(&'t self, list: &[Ty<'t>]) -> bool {
		list.iter().any(|i| self.occurs_in(i))
	}

	pub fn occurs_in(&'t self, t: Ty<'t>) -> bool {
		let s = if let Ty_::Infer(s) = *self { s } else { panic!() };
		
		match *t {
			Ty_::Infer(v) if v == s => true,
			Ty_::Error | Ty_::Infer(_) | Ty_::Kind(_) => false,
			Ty_::Tuple(ref vec) => self.occurs_in_list(&vec[..]),
			Ty_::Fn(ref args, ret) => self.occurs_in_list(&args[..]) || self.occurs_in(ret),
			Ty_::Ref(_, ref substs) => self.occurs_in_list(&substs[..]),
			Ty_::Proj(_, ref substs, _) => self.occurs_in_list(&substs[..]),
			Ty_::Ptr(p) => self.occurs_in(p),
		}
	}
}

pub enum Level<'t> {
	Value(Ty<'t>),
	Type(Ty<'t>),
	Kind(Ty<'t>),
}

#[derive(Clone)]
pub struct TyParam<'t> {
	pub id: Id,
	pub scheme: Scheme<'t>,
}

#[derive(Clone)]
pub struct Scheme<'t> {
	pub ty: Ty<'t>,
	pub value: bool,
	pub params: Vec<TyParam<'t>>,

}

impl<'t> Scheme<'t> {
	pub fn plain(ty: Ty<'t>) -> Scheme<'t> {
		Scheme {
			ty: ty,
			value: true,
			params: Vec::new(),
		}
	}
}