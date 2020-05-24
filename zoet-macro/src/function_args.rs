//! Function signature checking and processing.
//!
//! Rather than hand-crufting parsers for function signatures to extract the parameter types,
//! `FunctionArgs` contains the function input, output, and useful metadata, and methods for
//! checking and transforming the input and output. Users of this library will normally use this as
//! a pipeline, starting with the relatively raw `syn` nodes and ultimately producing simple types
//! which can be easily slotted into the `quote!` macro.

use crate::{error::*, with_tokens::*};
use core::convert::TryFrom;
use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use std::borrow::Cow::{self, Borrowed, Owned};
use syn::{
    parse_quote, GenericArgument, Generics, Ident, PathArguments, PathSegment, ReturnType, Type,
    TypePath, TypeReference,
};

/// Processing of function input and output types.
///
/// This is a selection of methods which validate and transform function signatures into a form
/// suitable for placing into templates.

#[derive(Copy, Clone)]
pub struct FunctionMeta<'a> {
    // These are things we can turn into a TokenStream and thus a pair of Spans (for start and end
    // location) in error reporting.

    // the ident; a least-worst choice of thing to highlight in a function declaration, since we
    // cannot rely on other parts of the declaration to be always nonempty, an empty one
    pub ident_to_tokens: &'a dyn ToTokens,

    // This is the span of the function/method. TODO: this is probably redundant and one of the
    // above would suffice. (Span is both small and Copy, so we don't bother with a reference.)
    pub item_span: Span,

    // And these are bits pasted into the output traits

    // A path/type fragment which describes how to call the functions, e.g. "foo", "Foo::bar".
    pub to_call: &'a TokenStream,
    // The generic parameters; both the <...> part and where clause
    pub generics: &'a Generics,
}

#[derive(Clone)]
pub struct FunctionArgs<'a, I, O> {
    pub input: I,
    pub output: O,
    pub meta: FunctionMeta<'a>,
}

impl<'a, I, O> FunctionArgs<'a, I, O> {
    fn with_input<I2>(self, input: I2) -> FunctionArgs<'a, I2, O> {
        FunctionArgs { input, output: self.output, meta: self.meta }
    }

    fn with_output<O2>(self, output: O2) -> FunctionArgs<'a, I, O2> {
        FunctionArgs { input: self.input, output, meta: self.meta }
    }
}

impl<'a, I, O> FunctionArgs<'a, I, WithTokens<'a, O>> {
    fn with_output_value<O2>(self, output_value: O2) -> FunctionArgs<'a, I, WithTokens<'a, O2>> {
        FunctionArgs {
            input: self.input,
            output: self.output.with_value(output_value),
            meta: self.meta,
        }
    }
}

fn param_name(index: usize) -> Cow<'static, str> {
    match index {
        0 => Borrowed("first parameter"),
        1 => Borrowed("second parameter"),
        other => Owned(format!("parameter {}", other + 1)),
    }
}

fn check_ref<'a>(ty: &'a WithTokens<'a, Type>, name: &str) -> Result<()> {
    match &ty.value {
        Type::Reference(TypeReference { mutability: None, .. }) => Ok(()),
        _ => Error::err(format!("{} must be an immutable reference", name), ty.to_tokens),
    }
}

// TODO: handle lifetime arguments. This probably involves reworking our data model somewhat.
fn unwrap_ref<'a>(ty: &'a WithTokens<'a, Type>, name: &str) -> Result<Type> {
    match &ty.value {
        Type::Reference(TypeReference { lifetime: None, mutability: None, elem, .. }) =>
            Ok(elem.as_ref().clone()),
        _ => Error::err(
            format!("{} must be an immutable reference and not have a lifetime argument", name),
            ty.to_tokens,
        ),
    }
}

// TODO: handle lifetime arguments
fn unwrap_mut<'a>(ty: &'a WithTokens<'a, Type>, name: &str) -> Result<Type> {
    match &ty.value {
        Type::Reference(TypeReference { lifetime: None, mutability: Some(_), elem, .. }) =>
            Ok(elem.as_ref().clone()),
        _ => Error::err(
            format!("{} must be a mutable reference and not have a lifetime argument", name),
            ty.to_tokens,
        ),
    }
}

fn unwrap_param_type<'a>(ty: &'a WithTokens<'a, Type>) -> Result<Option<(&'a Ident, Box<[Type]>)>> {
    if let Type::Path(TypePath { qself: None, path }) = &ty.value {
        let last = path.segments.last().expect("TypePath::path is always nonempty");
        if let PathSegment { ident, arguments: PathArguments::AngleBracketed(abga) } = last {
            let args = abga
                .args
                .iter()
                .map(|arg| match arg {
                    GenericArgument::Type(ty) => Ok(ty.clone()),
                    _ => Error::err("this generic parameter must be a type", arg),
                })
                .collect::<Result<_>>()?;
            return Ok(Some((ident, args)));
        }
    }
    Ok(None)
}

fn unwrap_result<'a>(ty: &'a WithTokens<'a, Type>, name: &str) -> Result<(Type, Type)> {
    if let Some((ident, boxed)) = unwrap_param_type(ty)? {
        match (ident.to_string().as_str(), &*boxed) {
            ("Result", [ref a, ref b]) => return Ok((a.clone(), b.clone())),
            ("Result", [ref a]) | ("Fallible", [ref a]) => {
                return Ok((a.clone(), parse_quote! { Error }));
            }
            _ => (),
        }
    }
    Error::err(
        format!("{} must be `Result<_>`, `Result<_,_>` or `Fallible<_>`", name),
        ty.to_tokens,
    )
}

fn unwrap_option<'a>(ty: &'a WithTokens<'a, Type>, name: &str) -> Result<Type> {
    if let Some((ident, boxed)) = unwrap_param_type(ty)? {
        if let ("Option", [ref a]) = (ident.to_string().as_str(), &*boxed) {
            return Ok(a.clone());
        }
    }
    Error::err(format!("{} must be `Option<_>`", name), ty.to_tokens)
}

impl<'a, O> FunctionArgs<'a, Cow<'a, [WithTokens<'a, Type>]>, O> {
    pub fn check_ref_param(mut self, index: usize) -> Result<Self> {
        match self.input.to_mut().get_mut(index) {
            None =>
                Error::err(format!("{} is missing", param_name(index)), self.meta.ident_to_tokens),
            Some(param) => {
                check_ref(param, &param_name(index))?;
                Ok(self)
            }
        }
    }

    pub fn unwrap_ref_param(mut self, index: usize) -> Result<Self> {
        match self.input.to_mut().get_mut(index) {
            None =>
                Error::err(format!("{} is missing", param_name(index)), self.meta.ident_to_tokens),
            Some(param) => {
                let result = unwrap_ref(param, &param_name(index))?;
                param.value = result;
                Ok(self)
            }
        }
    }

    pub fn unwrap_mut_param(mut self, index: usize) -> Result<Self> {
        match self.input.to_mut().get_mut(index) {
            None =>
                Error::err(format!("{} is missing", param_name(index)), self.meta.ident_to_tokens),
            Some(param) => {
                let result = unwrap_mut(param, &param_name(index))?;
                param.value = result;
                Ok(self)
            }
        }
    }

    pub fn nullary(self) -> Result<FunctionArgs<'a, (), O>> {
        if self.input.is_empty() {
            Ok(self.with_input(()))
        } else {
            Error::err("function should have zero parameters", self.meta.ident_to_tokens)
        }
    }

    pub fn unary(self) -> Result<FunctionArgs<'a, Type, O>> {
        match <&[_; 1]>::try_from(self.input.as_ref()) {
            Ok([a]) => {
                let a = a.value.clone();
                Ok(self.with_input(a))
            }
            _ => Error::err("function should have one parameter", self.meta.ident_to_tokens),
        }
    }

    pub fn binary(self) -> Result<FunctionArgs<'a, (Type, Type), O>> {
        match <&[_; 2]>::try_from(self.input.as_ref()) {
            Ok([a, b]) => {
                let a = a.value.clone();
                let b = b.value.clone();
                Ok(self.with_input((a, b)))
            }
            _ => Error::err("function should have two parameters", self.meta.ident_to_tokens),
        }
    }
}

impl<'a, I> FunctionArgs<'a, I, WithTokens<'a, ReturnType>> {
    fn try_return(self) -> Result<FunctionArgs<'a, I, WithTokens<'a, Type>>> {
        match &self.output.value {
            ReturnType::Type(_, ty) => {
                let ty = ty.as_ref().clone();
                Ok(self.with_output_value(ty))
            }
            ReturnType::Default =>
                Error::err("function must return a value", self.meta.ident_to_tokens),
        }
    }

    /// Checks the function returns something, i.e. is `fn(...) -> T;`.

    pub fn has_return(self) -> Result<FunctionArgs<'a, I, Type>> {
        let fa = self.try_return()?;
        let value = fa.output.value.clone();
        Ok(fa.with_output(value))
    }

    /// Checks the function returns nothing, i.e. is `fn(...);`.

    pub fn default_return(self) -> Result<FunctionArgs<'a, I, ()>> {
        match *self.output {
            ReturnType::Type(..) =>
                Error::err("function must return nothing", self.meta.ident_to_tokens),
            ReturnType::Default => Ok(self.with_output(())),
        }
    }

    /// Checks the function returns a reference, i.e. is `fn(...) -> &A;`, and sets the output to
    /// `A`.

    pub fn unwrap_ref_return(self) -> Result<FunctionArgs<'a, I, Type>> {
        let fa = self.try_return()?;
        let ty = unwrap_ref(&fa.output, "return type")?;
        Ok(fa.with_output(ty))
    }

    /// Checks the function returns a mutable reference, i.e. is (`fn(...) -> &mut A;`, and sets
    /// the output to `A`.

    pub fn unwrap_mut_return(self) -> Result<FunctionArgs<'a, I, Type>> {
        let fa = self.try_return()?;
        let ty = unwrap_mut(&fa.output, "return type")?;
        Ok(fa.with_output(ty))
    }

    /// Checks the function returns a Result, i.e. is `fn(...) -> Result<U>;` or `fn(...) ->
    /// Result<T,U>;`, and sets the output to `(A, Option<B>)`.

    pub fn unwrap_result_return(self) -> Result<FunctionArgs<'a, I, (Type, Type)>> {
        let fa = self.try_return()?;
        let tys = unwrap_result(&fa.output, "return type")?;
        Ok(fa.with_output(tys))
    }

    /// Checks the function returns an Option, i.e. is `fn(...) -> Option<U>;`, and sets the output
    /// to `A`.

    pub fn unwrap_option_return(self) -> Result<FunctionArgs<'a, I, Type>> {
        let fa = self.try_return()?;
        let tys = unwrap_option(&fa.output, "return type")?;
        Ok(fa.with_output(tys))
    }
}
