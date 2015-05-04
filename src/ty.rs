use ast::Id;

pub type Ty<'t> = &'t Ty_<'t>;

#[derive(Clone)]
pub enum Ty_<'t> {
	Error,
	Int,
	Tuple(Vec<Ty<'t>>),
	Infer(u32),
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
		let s = match *self {
			Ty_::Infer(s) => s,
			_ => panic!(),
		};

		match *t {
			Ty_::Infer(v) if v == s => true,
			Ty_::Error | Ty_::Int | Ty_::Infer(_) | Ty_::Kind(_) => false,
			Ty_::Tuple(ref vec) => self.occurs_in_list(&vec[..]),
			Ty_::Fn(ref args, ret) => self.occurs_in_list(&args[..]) || self.occurs_in(ret),
			Ty_::Ref(_, ref substs) => self.occurs_in_list(&substs[..]),
			Ty_::Proj(_, ref substs, _) => self.occurs_in_list(&substs[..]),
			Ty_::Ptr(p) => self.occurs_in(p),
		}
	}
}

#[derive(Clone)]
pub struct TyParam<'t> {
	pub id: Id,
	pub scheme: Scheme<'t>,
}

#[derive(Clone)]
pub struct Scheme<'t> {
	pub ty: Ty<'t>,
	pub params: Vec<TyParam<'t>>,

}

impl<'t> Scheme<'t> {
	pub fn plain(ty: Ty<'t>) -> Scheme<'t> {
		Scheme {
			ty: ty,
			params: Vec::new(),
		}
	}
}