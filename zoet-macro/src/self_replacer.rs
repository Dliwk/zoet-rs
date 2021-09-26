use crate::prelude::*;
use quote::quote_spanned;
use syn::{
    fold::*, parse2, spanned::Spanned, FnArg, ItemImpl, ItemTrait, PatStruct, PatTupleStruct, Path,
    Receiver, Type, TypeGroup, TypePath,
};

// fold_pat_struct` transforms the destructuring pattern `Self { ... }` into `#self_ty { ... }`, and
// similarly for `fold_pat_tuple_struct`. This requires `#self_ty` to be a path. For simplicity, we
// require the self-type to be a path even if we never replace it into destructuring binds, as the
// `zoet` only transforms inherent impls, not traits, and so the self-type is always a path anyway.

pub(crate) struct SelfReplacer<'a> {
    self_ty: &'a Type,
    path: &'a Path,
}

impl<'a> SelfReplacer<'a> {
    pub(crate) fn new(self_ty: &'a Type) -> Self {
        match *self_ty {
            // Some uses of macros cause the type to be wrapped in an invisible group
            Type::Group(TypeGroup { ref elem, .. }) => Self::new(elem),
            // A named type, which is what we want.
            Type::Path(TypePath {
                qself: None,
                ref path,
            }) => Self { self_ty, path },
            // We've no idea what to do with any other type.
            _ => abort!(self_ty, "this self-type is not a path"),
        }
    }

    fn replace_receiver(&self, receiver: Receiver) -> FnArg {
        let Receiver {
            attrs,
            reference,
            mutability,
            self_token,
        } = receiver;

        let self_ty = self.self_ty;
        let toks = match reference {
            // `(mut) self: Self`  -> `(mut) self: Self`
            None => quote_spanned! {
                self_ty.span() =>
                    #(#attrs)* #mutability #self_token : #self_ty
            },
            // `&('foo) (mut) self` -> `self: &('foo) (mut) Self`
            Some((and, lifetime)) => quote_spanned! {
                self_ty.span() =>
                    #(#attrs)* #self_token : #and #mutability #lifetime #self_ty
            },
        };

        parse2(toks).expect_or_abort("Mis-interpolated quote!(); please report a bug")
    }
}

impl Fold for SelfReplacer<'_> {
    fn fold_fn_arg(&mut self, i: FnArg) -> FnArg {
        match i {
            FnArg::Receiver(receiver) => self.replace_receiver(receiver),
            _ => fold_fn_arg(self, i),
        }
    }

    fn fold_item_impl(&mut self, i: ItemImpl) -> ItemImpl {
        // we do not want to recursively update `Self`s inside an `impl` block.
        i
    }

    fn fold_item_trait(&mut self, i: ItemTrait) -> ItemTrait {
        // we do not want to recursively update `Self`s inside a `trait` block.
        i
    }

    fn fold_pat_struct(&mut self, i: PatStruct) -> PatStruct {
        fold_pat_struct(self, PatStruct {
            path: self.path.clone(),
            ..i
        })
    }

    fn fold_pat_tuple_struct(&mut self, i: PatTupleStruct) -> PatTupleStruct {
        fold_pat_tuple_struct(self, PatTupleStruct {
            path: self.path.clone(),
            ..i
        })
    }

    fn fold_type(&mut self, i: Type) -> Type {
        match i {
            Type::Path(ref path) if path.path.is_ident("Self") => self.self_ty.clone(),
            _ => fold_type(self, i),
        }
    }
}

#[test]
fn test_self_replacer() -> Result<()> {
    #![allow(clippy::panic_in_result_fn)]
    use proc_macro2::TokenStream;
    use quote::{quote, ToTokens};
    use syn::*;

    let self_ty = &parse2::<Type>(quote! { RealSelf })?;
    let mut sr = SelfReplacer::new(self_ty);

    let mut check = move |from: TokenStream, to: TokenStream| -> Result<()> {
        let from_text = from.to_string();
        let to_text = to.to_string();

        let from_ast = parse2::<Item>(from)?;
        let to_ast = parse2::<Item>(to)?;
        let got_ast = sr.fold_item(from_ast);

        let got_text = got_ast.clone().into_token_stream().to_string();

        let log = format!(
            "\nFrom: \x1b[33m{}\x1b[0m\n Got: \x1b[33m{}\x1b[0m\nWant: \x1b[33m{}\x1b[0m",
            from_text, got_text, to_text
        );

        assert_eq!(&to_ast, &got_ast, "{}", log);
        // assert!(sr.errors.is_empty(), "{}", log);

        Ok(())
    };

    check(
        quote! { fn owned(self) -> Self {} },
        quote! { fn owned(self: RealSelf) -> RealSelf {} },
    )?;

    check(
        quote! { fn owned_mut(mut self) -> Self {} },
        quote! { fn owned_mut(mut self: RealSelf) -> RealSelf {} },
    )?;

    check(
        quote! { fn borrowed(&self) -> &Self {} },
        quote! { fn borrowed(self: &RealSelf) -> &RealSelf {} },
    )?;

    check(
        quote! { fn borrowed_mut(&mut self) -> &mut Self {} },
        quote! { fn borrowed_mut(self: &mut RealSelf) -> &mut RealSelf {} },
    )?;

    check(
        quote! { fn boxed(self: Box<Self>) -> Box<Self> {} },
        quote! { fn boxed(self: Box<RealSelf>) -> Box<RealSelf> {} },
    )?;

    check(
        quote! { fn five() -> Result<Self, &Self> {} },
        quote! { fn five() -> Result<RealSelf, &RealSelf> {} },
    )?;

    check(
        quote! { fn destructure(Self {x, y} : Self ) {} },
        quote! { fn destructure(RealSelf {x, y} : RealSelf ) {} },
    )?;

    check(
        quote! { fn tuple_destructure(Self(x, y) : Self ) {} },
        quote! { fn tuple_destructure(RealSelf(x, y) : RealSelf ) {} },
    )?;

    check(
        quote! {
            fn inner_impl(self) {
                struct Inner;
                impl Inner {
                    fn new() -> Self { Self }
                }
            }
        },
        quote! {
            fn inner_impl(self: RealSelf) {
                struct Inner;
                impl Inner {
                    fn new() -> Self { Self }
                }
            }
        },
    )?;

    check(
        quote! {
            fn inner_trait(self) {
                trait Inner {
                    fn new() -> Self { Self }
                }
            }
        },
        quote! {
            fn inner_trait(self: RealSelf) {
                trait Inner {
                    fn new() -> Self { Self }
                }
            }
        },
    )?;

    check(
        quote! {
            fn wat(self, rhs: <Self as Add>::Rhs) -> <Self as Add>::Output {}
        },
        quote! {
            fn wat(self: RealSelf, rhs: <RealSelf as Add>::Rhs) -> <RealSelf as Add>::Output {}
        },
    )?;

    Ok(())
}
