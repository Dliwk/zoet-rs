use crate::prelude::*;
use quote::quote_spanned;
use syn::{
    parse2, spanned::Spanned, visit_mut::*, FnArg, PatStruct, PatTupleStruct, Path, Receiver, Type,
    TypePath,
};

pub(crate) struct SelfReplacer<'a> {
    self_ty: &'a Type,
}

impl<'a> SelfReplacer<'a> {
    pub(crate) fn new(self_ty: &'a Type) -> Self {
        Self { self_ty }
    }

    pub(crate) fn try_replace_mut<T>(self_ty: &'a Type, ast: &mut T, op: fn(&mut Self, &mut T)) {
        let mut sr = Self::new(self_ty);
        op(&mut sr, ast);
    }

    fn replace_self_in_path(&self, path: &mut Path) {
        if path.is_ident("Self") {
            if let Type::Path(TypePath {
                qself: None,
                path: ref self_path,
            }) = *self.self_ty
            {
                *path = self_path.clone();
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
                abort!(path, "cannot replace Self in non-path types");
            }
        }
    }
}

impl VisitMut for SelfReplacer<'_> {
    fn visit_type_mut(&mut self, i: &mut Type) {
        match *i {
            Type::Path(ref path) if path.path.is_ident("Self") => *i = self.self_ty.clone(),
            _ => visit_type_mut(self, i),
        }
    }

    // This fold_pat_struct transforms the struct pattern `Self { ... }` into `#self_ty { ... }`.
    //
    // There is a slight complication in that a struct pattern can only start with a path (i.e. a
    // syn::Path) so one can't just slot in any arbitrary type. Inherent impls (i.e. `impl Foo
    // {...}`) can only implement a path (a "base type", in the error messages) so no problem occurs
    // there, but trait impls don't have that limitation.

    fn visit_pat_struct_mut(&mut self, i: &mut PatStruct) {
        self.replace_self_in_path(&mut i.path);
        visit_pat_struct_mut(self, i);
    }

    fn visit_pat_tuple_struct_mut(&mut self, i: &mut PatTupleStruct) {
        self.replace_self_in_path(&mut i.path);
        visit_pat_tuple_struct_mut(self, i);
    }

    fn visit_fn_arg_mut(&mut self, i: &mut FnArg) {
        let span = i.span();
        let self_ty = self.self_ty.clone();

        if let FnArg::Receiver(ref receiver) = *i {
            *i = match *receiver {
                Receiver {
                    ref attrs,
                    reference: None,
                    ..
                } => parse2::<FnArg>(quote_spanned!( span => #(#attrs)* self: #self_ty )),

                Receiver {
                    ref attrs,
                    reference: Some((ref and, ref lifetime)),
                    mutability,
                    ..
                } => parse2::<FnArg>(
                    quote_spanned!( span => #(#attrs)* self: #and #lifetime #mutability #self_ty ),
                ),
            }
            .expect_or_abort("Mis-interpolated quote!(); please report a bug");
        } else {
            visit_fn_arg_mut(self, i);
        }
    }

    // FIXME: we probably eventually want to deal with the case of structs defined inside functions,
    // since that changes the Self type. It doesn't affect our current usage where we're only
    // replacing Self in function signatures.
}

#[test]
fn test_self_replacer() -> Result<()> {
#![allow(clippy::panic_in_result_fn)]
    use quote::{quote, ToTokens};
    use syn::*;

    let transformations = [
        (
            quote! { fn testing_testing_one(self) -> Self {} },
            quote! { fn testing_testing_one(self: RealSelf) -> RealSelf {} },
            // quote! { fn testing_testing_one(self) -> RealSelf {} },
        ),
        (
            quote! { fn testing_two(&self) -> &Self {} },
            quote! { fn testing_two(self: &RealSelf) -> &RealSelf {} },
            // quote! { fn testing_two(&self) -> &RealSelf {} },
        ),
        (
            quote! { fn three(&mut self) -> &mut Self {} },
            quote! { fn three(self: &mut RealSelf) -> &mut RealSelf {} },
            // quote! { fn three(&mut self) -> &mut RealSelf {} },
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
        // (
        //     quote! {fn inner(self) { struct Foo; impl Foo { fn new() -> Self { Self } } }},
        //     quote! {fn inner(self: Self) { struct Foo; impl Foo { fn new() -> Self { Self } } }},
        // ),
    ];

    let self_ty = &parse2::<Type>(quote! { RealSelf })?;

    for (from, to) in IntoIterator::into_iter(transformations) {
        let from_text = from.clone().to_string();
        let to_text = to.clone().to_string();

        let from_ast = parse2::<Item>(from)?;
        // dbg! { &from_ast };
        let to_ast = parse2::<Item>(to)?;
        // dbg! { &to_ast };
        let mut sr = SelfReplacer::new(self_ty);
        let mut got_ast = from_ast.clone();
        sr.visit_item_mut(&mut got_ast);

        let got_text = got_ast.clone().into_token_stream().to_string();

        let log = format!(
            "\nFrom: \x1b[33m{}\x1b[0m\n Got: \x1b[33m{}\x1b[0m\nWant: \x1b[33m{}\x1b[0m",
            from_text, got_text, to_text
        );

        assert_eq!(&to_ast, &got_ast, "{}", log);
        // assert!(sr.errors.is_empty(), "{}", log);
    }
    Ok(())
}
