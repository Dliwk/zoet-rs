use crate::{
    function_args::{FunctionArgs, FunctionMeta},
    prelude::*,
    traits::{get_trait_fn, GenFn},
    with_tokens::WithTokens,
};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse::Parser, parse2, punctuated::Punctuated, spanned::Spanned, token, Attribute, Generics,
    ImplItem, ImplItemMethod, Item, ItemFn, ItemImpl, Meta, NestedMeta, Signature, Type,
};

fn trait_impl_from_fn(
    tokens: &mut TokenStream,
    copied_attrs: &[Attribute],
    attr: impl ToTokens, // basically here only for one error message
    nested_metas: impl IntoIterator<Item = NestedMeta>,
    signature: &Signature,
    self_type: Option<&Type>,
    generics: &Generics,
) {
    let ident = &signature.ident;
    let to_call = &if let Some(self_type) = self_type {
        quote! { < #self_type> :: #ident }
    } else {
        quote! { #ident }
    };
    let input = signature
        .inputs
        .iter()
        .map(|fn_arg| WithTokens::from_fn_arg(fn_arg, self_type))
        .collect::<Box<_>>();

    let extra_attrs = quote! {
        #( #copied_attrs )*
        // #[allow(clippy::pedantic)]
        //#[automatically_derived]
        //#[allow(unused_qualifications)]
    };

    let mut is_empty = true;
    for (derive_span, attr_trait_fn) in trait_fns(nested_metas) {
        is_empty = false;
        match attr_trait_fn {
            Err(error) => emit_error!(error),
            Ok(attr_trait_fn) => {
                let meta = FunctionMeta {
                    derive_span,
                    signature,
                    to_call,
                    generics,
                    extra_attrs: &extra_attrs,
                };
                let args = FunctionArgs {
                    input: input.clone(),
                    output: WithTokens::from_return_type(&signature.output, self_type),
                    meta,
                };
                match attr_trait_fn(args) {
                    Ok(trait_tokens) => {
                        // eprintln!("{}", &trait_tokens);
                        for copied_attr in copied_attrs {
                            tokens.extend(copied_attr.into_token_stream());
                        }
                        tokens.extend(trait_tokens);
                    }
                    Err(error) => emit_error!(error),
                }
            }
        }
    }
    if is_empty {
        emit_warning!(attr, "attribute should contain a trait or list of traits");
    }
}

fn zoet_free_fn(attr: &TokenStream, item_fn: ItemFn) -> TokenStream {
    let mut tokens = TokenStream::new();

    // TODO: rather than clone-and-drain then tossing the results, fix filter_attrs
    let (_fn_zoet_attrs, copied_attrs) = filter_attrs(&mut item_fn.attrs.clone());
    // assert!(fn_zoet_attrs.is_empty());

    let nested_metas = {
        let parser = <Punctuated<NestedMeta, token::Comma>>::parse_terminated;
        parser
            .parse2(attr.clone())
            .unwrap_or_else(|err| abort!(attr, "can't parse attribute: {}", err))
    };

    trait_impl_from_fn(
        &mut tokens,
        &copied_attrs,
        attr,
        nested_metas,
        &item_fn.sig,
        None,
        &item_fn.sig.generics,
    );

    tokens.extend(item_fn.into_token_stream());
    tokens
}

fn add_generics(mut lhs: Generics, rhs: Generics) -> Generics {
    add_assign_generics(&mut lhs, rhs);
    lhs
}

fn add_assign_generics(lhs: &mut Generics, rhs: Generics) {
    lhs.lt_token = lhs.lt_token.or(rhs.lt_token);
    lhs.gt_token = lhs.gt_token.or(rhs.gt_token);

    lhs.params.extend(rhs.params.into_iter());

    if let Some(rhs_where) = rhs.where_clause {
        match lhs.where_clause {
            Some(ref mut lhs_where) => lhs_where.predicates.extend(rhs_where.predicates),
            ref mut lhs_where @ None => *lhs_where = Some(rhs_where),
        };
    }
}

// Given a vector of function/method attributes, removes `zoet` attributes from there (otherwise
// macro users will get an "unused attribute" warning) and returns the removed `zoet` attributes
// plus other `cfg` and `cfg`-like attributes which should be applied to the generated trait impl.
fn filter_attrs(attrs: &mut Vec<Attribute>) -> (Vec<Attribute>, Vec<Attribute>) {
    let mut copied_attrs = Vec::new();
    // drain_filter would be nice, but it's not stable yet.
    let (zoet_attrs, fn_attrs) = attrs.drain(..).partition(|attr| {
        if attr.path.is_ident("cfg") || attr.path.is_ident("doc_cfg") {
            copied_attrs.push(attr.clone());
        }
        attr.path.is_ident("zoet")
    });
    *attrs = fn_attrs;
    (zoet_attrs, copied_attrs)
}

fn zoet_inherent_impl(attr: &TokenStream, mut item_impl: ItemImpl) -> TokenStream {
    if !attr.is_empty() {
        emit_warning!(attr, "ignored parameters here");
    }

    let mut tokens = TokenStream::new();

    for item in &mut item_impl.items {
        if let ImplItem::Method(ref mut method @ ImplItemMethod { .. }) = *item {
            let (fn_zoet_attrs, copied_attrs) = filter_attrs(&mut method.attrs);
            let generics = add_generics(item_impl.generics.clone(), method.sig.generics.clone());

            // iterate in case #[zoet] is applied more than once to a method.
            for attr in fn_zoet_attrs {
                let nested_metas = {
                    let attr = attr
                        .parse_meta()
                        .unwrap_or_else(|err| abort!(attr, "can't parse attribute: {}", err));
                    match attr.clone() {
                        Meta::List(meta_list) => meta_list.nested,
                        _ => abort!(attr, "attribute takes a parenthesised list"),
                    }
                };

                trait_impl_from_fn(
                    &mut tokens,
                    &copied_attrs,
                    attr,
                    nested_metas,
                    &method.sig,
                    Some(&item_impl.self_ty),
                    &generics,
                );
            }
        }
    }
    tokens.extend(item_impl.into_token_stream());
    tokens
}

fn trait_fns(
    nested_metas: impl IntoIterator<Item = NestedMeta>,
) -> impl Iterator<Item = (Span, Result<GenFn>)> {
    nested_metas.into_iter().map(|nested_meta| {
        let result = match nested_meta {
            NestedMeta::Lit(ref lit) => Err(diagnostic_error!(lit, "literals are not valid here")),
            NestedMeta::Meta(ref meta) => match *meta {
                Meta::List(ref value) =>
                    Err(diagnostic_error!(value, "this does not take parameters")),
                Meta::NameValue(ref value) =>
                    Err(diagnostic_error!(value, "this does not take a parameter")),
                Meta::Path(ref value) => value
                    .get_ident()
                    .ok_or_else(|| diagnostic_error!(value, "this is not a valid trait name"))
                    .and_then(|ident| get_trait_fn(ident)),
            },
        };
        (nested_meta.span(), result)
    })
}

pub(crate) fn zoet(attr: &TokenStream, item: &TokenStream) -> TokenStream {
    // The job of this macro is to take a function and generate a trait impl which uses the
    // function. In the case of free functions, this is simple: just apply the attribute to the
    // function, and the function will be replaced with the trait and the function.
    //
    // When it comes to _methods_, this doesn't work. We do not know the type of `self`, and the
    // method can't be replaced with the trait impl and the method, because methods are in impl
    // scope and traits can't be implemented there. In this case, the attribute is _also_ applied to
    // the impl, and then the macro has enough information and access to the right scope.

    // FIXME: clone is only needed for error-reporting due to Span needing to walk the TokenStream.
    // Fixed in unstable.

    let item: Item =
        parse2(item.clone()).unwrap_or_else(|error| abort!(item, "this is not an item: {}", error));

    match item {
        Item::Fn(item_fn) => match item_fn {
            ItemFn {
                sig: Signature { variadic: None, .. },
                ..
            } => zoet_free_fn(attr, item_fn),
            _ => abort!(item_fn, "cannot apply to variadic functions"),
        },
        Item::Impl(item_impl) => match item_impl {
            ItemImpl { trait_: None, .. } => zoet_inherent_impl(attr, item_impl),
            _ => abort!(item_impl, "can only apply to trait impls"),
        },
        item => abort!(item, "can only apply to functions or inherent impls"),
    }
}
