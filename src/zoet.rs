use crate::{
    error::{Error, Result, ResultExt},
    function_args::{FunctionArgs, FunctionMeta},
    traits::{GenFn, TRAIT_FNS},
    with_tokens::WithTokens,
};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::borrow::Cow;
use syn::{
    parse::{ParseStream, Parser},
    parse2,
    punctuated::Punctuated,
    spanned::Spanned,
    token, FnDecl, ImplItem, Item, ItemFn, ItemImpl, Meta, *,
};

fn zoet_free_fn(attr: &TokenStream, item_fn: &ItemFn) -> Result<TokenStream> {
    let mut tokens = TokenStream::new();

    let to_call = &item_fn.ident.clone().into_token_stream();

    let fn_decl = &item_fn.decl;

    let meta = FunctionMeta {
        item_span: item_fn.span(),
        ident_to_tokens: &item_fn.ident,
        to_call,
        generics: &fn_decl.generics,
    };

    let input = fn_decl
        .inputs
        .iter()
        .map(|fn_arg| WithTokens::from_fn_arg(fn_arg, None))
        .collect::<Result<Cow<_>>>()?;

    let parser = <Punctuated<Meta, token::Comma>>::parse_terminated;
    let attr_trait_fns = trait_fns(parser, attr, attr)?;

    for attr_trait_fn in attr_trait_fns {
        let attr_trait_fn = attr_trait_fn?;
        let args = FunctionArgs {
            input: Cow::clone(&input),
            output: WithTokens::from_return_type(&fn_decl.output, None)?,
            meta: &meta,
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
            // drain_filter would be nice, but it's not stable yet.
            let (fn_zoet_attrs, fn_other_attrs) =
                method.attrs.drain(..).partition(|attr| attr.path.is_ident("zoet"));
            method.attrs = fn_other_attrs;

            for attr in fn_zoet_attrs {
                // TODO: this is very similar to fn-parsing in zoet_free_fn. Consider
                // refactoring.

                let method_ident = &method.sig.ident;
                let self_ty = &item_impl.self_ty;
                let to_call = &quote! { < #self_ty > :: #method_ident };
                let fn_decl = &method.sig.decl;

                let meta = FunctionMeta {
                    item_span: method.span(),
                    ident_to_tokens: &method.sig.ident,
                    to_call,
                    generics: &merge_generics(item_impl.generics.clone(), &fn_decl.generics),
                };

                let input = fn_decl
                    .inputs
                    .iter()
                    .map(|fn_arg| WithTokens::from_fn_arg(fn_arg, Some(self_ty)))
                    .collect::<Result<Cow<_>>>()?;

                let parser = |input: ParseStream| -> syn::Result<_> {
                    let content;
                    parenthesized!(content in input);
                    <Punctuated<Meta, token::Comma>>::parse_terminated(&content)
                };
                let attr_trait_fns = trait_fns(parser, &attr.tts, &attr)?;

                for attr_trait_fn in attr_trait_fns {
                    let attr_trait_fn = attr_trait_fn?;
                    let args = FunctionArgs {
                        input: Cow::clone(&input),
                        output: WithTokens::from_return_type(&fn_decl.output, Some(self_ty))?,
                        meta: &meta,
                    };
                    let trait_tokens = attr_trait_fn(args)?;
                    tokens.extend(trait_tokens);
                }
            }
        }
    }

    tokens.extend(item_impl.into_token_stream());
    Ok(tokens)
}

fn trait_fns<P>(
    parser: impl Parser<Output = Punctuated<Meta, P>>,
    attr: &TokenStream,
    to_tokens: &dyn ToTokens,
) -> Result<impl Iterator<Item = Result<&'static GenFn>>>
{
    if attr.is_empty() {
        return Error::err("attribute should contain a trait or list of traits", to_tokens);
    }

    parser.parse2(attr.clone()).context("cannot parse attribute", attr).map(|punctuated| {
        punctuated.into_iter().map(|attr_arg| match attr_arg {
            Meta::Word(ident) => TRAIT_FNS
                .get(ident.to_string().as_str())
                .context("this is not a known trait name", ident),
            Meta::List(value) => Error::err("this does not take parameters", value),
            Meta::NameValue(value) => Error::err("this does not take a parameter", value),
        })
    })
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
        Item::Fn(ref item_fn) => match item_fn.decl.as_ref() {
            FnDecl { variadic: None, .. } => zoet_free_fn(attr, item_fn),
            _ => Error::err("cannot apply to this function", &item_fn),
        },
        Item::Impl(item_impl @ ItemImpl { trait_: None, .. }) =>
            zoet_inherent_impl(attr, item_impl),
        item => Error::err("cannot apply to this item", item),
    }
}
