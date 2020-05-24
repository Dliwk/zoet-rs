use crate::{
    error::{Error, Result, ResultExt},
    function_args::*,
    traits::*,
    with_tokens::*,
};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::borrow::Cow;
use syn::{
    parse::Parser, parse2, punctuated::Punctuated, spanned::Spanned, token, ImplItem, Item, ItemFn,
    ItemImpl, Meta, *,
};

fn zoet_free_fn(attr: &TokenStream, item_fn: ItemFn) -> Result<TokenStream> {
    let parser = <Punctuated<NestedMeta, token::Comma>>::parse_terminated;
    let attr = parser.parse2(attr.clone()).context("can't parse this attribute", attr)?;

    let mut tokens = TokenStream::new();

    let sig = &item_fn.sig;
    let to_call = &(&sig.ident).into_token_stream();

    let input = sig
        .inputs
        .iter()
        .map(|fn_arg| WithTokens::from_fn_arg(fn_arg, None))
        .collect::<Result<Cow<_>>>()?;

    let meta = FunctionMeta {
        item_span: item_fn.span(),
        ident_to_tokens: &sig.ident,
        to_call,
        generics: &sig.generics,
    };
    for (_nested_meta, attr_trait_fn) in trait_fns(&attr)? {
        //        meta.item_span = nested_meta.span();
        let attr_trait_fn = attr_trait_fn?;
        let args = FunctionArgs {
            input: Cow::clone(&input),
            output: WithTokens::from_return_type(&sig.output, None)?,
            meta,
        };
        let trait_tokens = attr_trait_fn(args)?;
        tokens.extend(trait_tokens);
    }

    tokens.extend(item_fn.into_token_stream());
    Ok(tokens)
}

fn merge_generics(mut lhs: Generics, rhs: &Generics) -> Generics {
    lhs.lt_token = lhs.lt_token.or(rhs.lt_token);
    lhs.gt_token = lhs.gt_token.or(rhs.gt_token);

    lhs.params.extend(rhs.params.iter().cloned());

    match (lhs.where_clause.as_mut(), rhs.where_clause.as_ref()) {
        (_, None) => {}
        (None, Some(_)) => lhs.where_clause = rhs.where_clause.clone(),
        (Some(lhs_where), Some(rhs_where)) =>
            lhs_where.predicates.extend(rhs_where.predicates.clone()),
    };

    lhs
}

fn zoet_inherent_impl(attr: &TokenStream, mut item_impl: ItemImpl) -> Result<TokenStream> {
    if !attr.is_empty() {
        return Error::err("impl attribute does not take parameters", attr);
    }

    let mut tokens = TokenStream::new();

    for item in &mut item_impl.items {
        if let ImplItem::Method(ref mut method @ ImplItemMethod { .. }) = item {
            let mut copied_attrs = Vec::new();
            // drain_filter would be nice, but it's not stable yet.
            let (fn_zoet_attrs, fn_other_attrs) = method.attrs.drain(..).partition(|attr| {
                if attr.path.is_ident("cfg") || attr.path.is_ident("doc_cfg") {
                    copied_attrs.push(attr.clone());
                }
                attr.path.is_ident("zoet")
            });
            method.attrs = fn_other_attrs;

            for attr in fn_zoet_attrs {
                // TODO: this is very similar to fn-parsing in zoet_free_fn. Consider
                // refactoring.

                let sig = &method.sig;
                let method_ident = &sig.ident;
                let self_ty = &item_impl.self_ty;
                let to_call = &quote! { < #self_ty > :: #method_ident };

                let input = sig
                    .inputs
                    .iter()
                    .map(|fn_arg| WithTokens::from_fn_arg(fn_arg, Some(self_ty)))
                    .collect::<Result<Cow<_>>>()?;

                let attr = attr.parse_meta().context("can't parse attribute", attr)?;
                let attr = match attr {
                    Meta::List(meta_list) => meta_list.nested,
                    _ => return Error::err("expected a parenthesised list", attr)?,
                };

                // TODO: refactor so that item_span takes the nested_meta returned by trait_fns(),
                // but at the moment there are lifetime inference problems.

                let meta = FunctionMeta {
                    //item_span: method.span(),
                    item_span: attr.span(),
                    ident_to_tokens: &method.sig.ident,
                    to_call,
                    generics: &merge_generics(item_impl.generics.clone(), &sig.generics),
                };

                for (_nested_meta, attr_trait_fn) in trait_fns(&attr)? {
                    let attr_trait_fn = attr_trait_fn?;
                    let args = FunctionArgs {
                        input: Cow::clone(&input),
                        output: WithTokens::from_return_type(&sig.output, Some(self_ty))?,
                        meta,
                    };
                    let trait_tokens = attr_trait_fn(args)?;
                    // TODO: deal with inner attributes properly, rather than just cloddishly paste
                    // them in front of the generated code.
                    // TODO: also do this for free functions.
                    for copied_attr in &copied_attrs {
                        tokens.extend(copied_attr.into_token_stream());
                    }
                    tokens.extend(trait_tokens);
                }
            }
            //tokens.extend(quote!{ fn wat() {} });
            //dbg!{ method.into_token_stream().to_string() };
        }
    }
    tokens.extend(item_impl.into_token_stream());
    Ok(tokens)
}

fn trait_fns(
    nested_metas: &Punctuated<NestedMeta, Token![,]>,
) -> Result<impl Iterator<Item = (&NestedMeta, Result<GenFn>)>> {
    if nested_metas.is_empty() {
        return Error::err("attribute ought to contain a trait or list of traits", nested_metas);
    }

    Ok(nested_metas.iter().map(|nested_meta| {
        let result = match nested_meta {
            NestedMeta::Lit(lit) => Error::err("literals are not valid here", lit),
            NestedMeta::Meta(meta) => match meta {
                Meta::List(value) => Error::err("this does not take parameters", value),
                Meta::NameValue(value) => Error::err("this does not take a parameter", value),
                Meta::Path(value) => value
                    .get_ident()
                    .ok_or_else(|| Error::new("this is not a valid trait name", value))
                    .and_then(|ident| {
                        get_trait_fn(ident.to_string().as_str())
                            .context("this is not a known trait name", ident)
                    }),
            },
        };
        (nested_meta, result)
    }))
}

pub fn zoet(attr: &TokenStream, item: TokenStream) -> Result<TokenStream> {
    // The job of this macro is to take a function and generate a trait impl which uses the
    // function. In the case of free functions, this is simple: just apply the attribute to the
    // function, and the function will be replaced with the trait and the function.
    //
    // When it comes to _methods_, this doesn't work. We do not know the type of `self`, and the
    // method can't be replaced with the trait impl and the method, because methods are in impl
    // scope and traits can't be implemented there. In this case, the attribute is _also_ applied
    // to the impl, and then the macro has enough information and access to the right scope.

    // FIXME: clone is only needed for error-reporting due to Span needing to walk the TokenStream.
    // Fixed in unstable.

    let item = parse2::<Item>(item.clone()).context("this is not an item", item)?;

    match item {
        Item::Fn(item_fn) => match item_fn {
            ItemFn { sig: Signature { variadic: None, .. }, .. } => zoet_free_fn(attr, item_fn),
            _ => Error::err("cannot apply to this function", item_fn),
        },
        Item::Impl(item_impl) => match item_impl {
            ItemImpl { trait_: None, .. } => zoet_inherent_impl(attr, item_impl),
            _ => Error::err("cannot apply to this impl", item_impl),
        },
        item => Error::err("cannot apply to this type of item", item),
    }
}
