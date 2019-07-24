use crate::preamble::*;
use syn::{
    fold::*, spanned::Spanned, ArgCaptured, ArgSelf, ArgSelfRef, FnArg, Ident, Pat, PatIdent,
    PatStruct, PatTupleStruct, Token, Type, TypePath, TypeReference,
};

pub struct SelfReplacer<'a> {
    pub self_ty: &'a Type,
    pub errors: Vec<Error>,
    //pub diagnostics: Vec<Diagnostic>,
}
// impl Drop for SelfReplacer<'_> {
//     fn drop(&mut self) {
//         self.emit_diagnostics();
//     }
// }
impl<'a> SelfReplacer<'a> {
    pub fn new(self_ty: &'a Type) -> Self {
        //Self { self_ty, diagnostics: vec![] }
        Self { self_ty, errors: vec![] }
    }

    pub fn try_replace<T>(self_ty: &'a Type, mut ast: T, op: fn(&mut Self, T) -> T) -> Result<T> {
        let mut sr = Self::new(self_ty);
        ast = op(&mut sr, ast);
        let mut errors = sr.errors.drain(..);
        if let Some(e) = errors.next() { Err(e) } else { Ok(ast) }
    }

    // pub fn clear_diagnostics(&mut self) {
    //     self.diagnostics.clear()
    // }

    // pub fn emit_diagnostics(&mut self) {
    //     self.diagnostics.drain(..).for_each(Diagnostic::emit);
    // }
}

impl Fold for SelfReplacer<'_> {
    fn fold_type(&mut self, i: Type) -> Type {
        match i {
            Type::Path(ref path) if path.path.is_ident("Self") => self.self_ty.clone(),
            _ => fold_type(self, i),
        }
    }

    // This fold_pat_struct transforms the struct pattern `Self { ... }` into `#self_ty { ... }`.
    //
    // There is a slight complication in that a struct pattern can only start with a path (i.e. a
    // syn::Path) so one can't just slot in any arbitrary type. Inherent impls (i.e. `impl Foo
    // {...}`) can only implement a path (a "base type", in the error messages) so no problem occurs
    // there, but trait impls don't have that limitation.

    fn fold_pat_struct(&mut self, mut i: PatStruct) -> PatStruct {
        if i.path.is_ident("Self") {
            if let Type::Path(TypePath { qself: None, ref path }) = *self.self_ty {
                i.path = path.clone();
            } else {
                // self.diagnostics.push(
                //     Diagnostic::spanned(
                //         path.span().unwrap(),
                //         Level::Error,
                //         "Cannot replace Self in this struct pattern with the actual self type",
                //     )
                //     .help("The self type needs to be a path such as `Foo`")
                //     .span_note(
                //         self.self_ty.full_span().unwrap(),
                //         "We tried to use this as the self type",
                //     ),
                // );
                self.errors.push(Error::new("cannot replace Self in non-path types", &i.path));
            }
        }
        fold_pat_struct(self, i)
    }

    fn fold_pat_tuple_struct(&mut self, mut i: PatTupleStruct) -> PatTupleStruct {
        if i.path.is_ident("Self") {
            if let Type::Path(TypePath { qself: None, ref path }) = self.self_ty {
                i.path = path.clone();
            } else {
                // self.diagnostics.push(
                //     Diagnostic::spanned(
                //         path.span().unwrap(),
                //         Level::Error,
                //         "Cannot replace Self in this struct pattern with the actual self type",
                //     )
                //     .help("The self type needs to be a path such as `Foo`")
                //     .span_note(
                //         self.self_ty.full_span().unwrap(),
                //         "We tried to use this as the self type",
                //     ),
                // );
                self.errors.push(Error::new("cannot replace Self in non-path types", &i.path));
            }
        }
        fold_pat_tuple_struct(self, i)
    }

    fn fold_fn_arg(&mut self, i: FnArg) -> FnArg {
        let span = i.span();

        match i {
            FnArg::SelfValue(ArgSelf { mutability, .. }) => FnArg::Captured(ArgCaptured {
                pat: Pat::Ident(PatIdent {
                    by_ref: None,
                    mutability,
                    ident: Ident::new("self", span),
                    subpat: None,
                }),
                colon_token: Token![:](span),
                ty: self.self_ty.clone(),
            }),

            FnArg::SelfRef(ArgSelfRef { ref lifetime, mutability, .. }) =>
                FnArg::Captured(ArgCaptured {
                    pat: Pat::Ident(PatIdent {
                        by_ref: None,
                        mutability: None,
                        ident: Ident::new("self", span),
                        subpat: None,
                    }),
                    colon_token: Token![:](span),
                    ty: Type::Reference(TypeReference {
                        and_token: Token![&](span),
                        lifetime: lifetime.clone(),
                        mutability,
                        elem: Box::new(self.self_ty.clone()),
                    }),
                }),

            _ => fold_fn_arg(self, i),
        }
    }

    // FIXME: we probably eventually want to deal with the case of structs defined inside functions,
    // since that changes the Self type. It doesn't affect our current usage where we're only
    // replacing Self in function signatures.
}

#[test]
fn test_self_replacer() -> Result<()> {
    use quote::{quote, ToTokens};
    use syn::*;

    let transformations = vec![
        (
            quote! { fn testing_testing_one(self) -> Self {} },
            quote! { fn testing_testing_one(self: RealSelf) -> RealSelf {} },
            //quote! { fn testing_testing_one(self) -> RealSelf {} },
        ),
        (
            quote! { fn testing_two(&self) -> &Self {} },
            quote! { fn testing_two(self: &RealSelf) -> &RealSelf {} },
            //quote! { fn testing_two(&self) -> &RealSelf {} },
        ),
        (
            quote! { fn three(&mut self) -> &mut Self {} },
            quote! { fn three(self: &mut RealSelf) -> &mut RealSelf {} },
            //quote! { fn three(&mut self) -> &mut RealSelf {} },
        ),
        (
            quote! { fn four(self: Box<Self>) -> Box<Self> {} },
            quote! { fn four(self: Box<RealSelf>) -> Box<RealSelf> {} },
        ),
        (
            quote! { fn five() -> Result<Self, &Self> {} },
            quote! { fn five() -> Result<RealSelf, &RealSelf> {} },
        ),
        (
            quote! { fn six(Self {x, y} : Self ) {} },
            quote! { fn six(RealSelf {x, y} : RealSelf ) {} },
        ),
        (
            quote! { fn seven(Self(x, y) : Self ) {} },
            quote! { fn seven(RealSelf(x, y) : RealSelf ) {} },
        ),
        /* (
         *     quote! {
         *         fn inner(self) { struct Foo; impl Foo { fn new() -> Self { Self } } }
         *     },
         *     quote! {
         *         fn inner(self: Self) { struct Foo; impl Foo { fn new() -> Self { Self } } }
         *     },
         * ),
         */
    ];

    let self_ty = &parse2::<Type>(quote! { RealSelf }).unwrap();

    for (from, to) in transformations.into_iter() {
        let from_text = from.clone().to_string();
        let to_text = to.clone().to_string();

        let from_ast = parse2::<Item>(from).unwrap();
        //dbg! { &from_ast };
        let to_ast = parse2::<Item>(to).unwrap();
        //dbg! { &to_ast };
        let mut sr = SelfReplacer::new(self_ty);
        let got_ast = sr.fold_item(from_ast.clone());

        let got_text = got_ast.clone().into_token_stream().to_string();

        let log = format!(
            "\nFrom: \x1b[33m{}\x1b[0m\n Got: \x1b[33m{}\x1b[0m\nWant: \x1b[33m{}\x1b[0m",
            from_text, got_text, to_text
        );

        assert_eq!(&to_ast, &got_ast, "{}", log);
        assert!(sr.errors.is_empty(), "{}", log);
    }
    Ok(())
}
