//! Function signature checking and processing.
//!
//! Rather than hand-crufting parsers for function signatures to extract the parameter types,
//! `FunctionArgs` contains the function input, output, and useful metadata, and methods for
//! checking and transforming the input and output. Users of this library will normally use this as
//! a pipeline, starting with the relatively raw `syn` nodes and ultimately producing simple types
//! which can be easily slotted into the `quote!` macro.

use crate::preamble::*;
use core::convert::TryFrom;
use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use std::borrow::Cow::{self, Borrowed, Owned};
use syn::{
    GenericArgument, Generics, PathArguments, PathSegment, ReturnType, Type, TypePath,
    TypeReference,
};

/// Processing of function input and output types.
///
/// This is a selection of methods which validate and transform function signatures into a form
/// suitable for placing into templates.

pub struct FunctionMeta<'a> {
    // These are things we can turn into a TokenStream and thus a pair of Spans (for start and end
    // location) in error reporting.

    // the ident; a least-worst choice of thing to highlight in a function declaration, since we
    // cannot rely on other parts of the declaration to be always nonempty, an empty one
    pub ident_to_tokens: &'a dyn ToTokens,

    // This is the span of the function/method. TODO: this is probably redundant and one of the
    // above would suffice.
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
    pub meta: &'a FunctionMeta<'a>,
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

fn try_ref<'a>(ty: &'a WithTokens<'a, Type>, name: &str) -> Result<Type> {
    match &ty.value {
        Type::Reference(TypeReference { lifetime: None, mutability: None, elem, .. }) =>
            Ok(elem.as_ref().clone()),
        _ => Error::err(format!("{} must be an immutable reference", name), ty.to_tokens),
    }
}

fn try_mut<'a>(ty: &'a WithTokens<'a, Type>, name: &str) -> Result<Type> {
    match &ty.value {
        Type::Reference(TypeReference { lifetime: None, mutability: Some(_), elem, .. }) =>
            Ok(elem.as_ref().clone()),
        _ => Error::err(format!("{} must be a mutable reference", name), ty.to_tokens),
    }
}

fn try_param_type<'a>(
    ty: &'a WithTokens<'a, Type>,
    required_type: &str,
    position_name: &str,
) -> Result<Vec<Type>>
{
    if let Type::Path(TypePath { qself: None, path }) = &ty.value {
        let last = path.segments.last().expect("TypePath::path is always nonempty");
        if let PathSegment { ident, arguments: PathArguments::AngleBracketed(abga) } = last.value()
        {
            if ident == required_type {
                let args: Result<Vec<Type>> = abga
                    .args
                    .iter()
                    .map(|arg| match arg {
                        GenericArgument::Type(ty) => Ok(ty.clone()),
                        _ => Error::err("this generic parameter must be a type", arg),
                    })
                    .collect();
                return args;
            }
        }
    }
    Error::err(format!("{} type must be `{}<...>`", position_name, required_type), ty.to_tokens)
}

fn try_result<'a>(ty: &'a WithTokens<'a, Type>, name: &str) -> Result<(Type, Option<Type>)> {
    try_param_type(ty, "Result", name).and_then(|types| match types.as_slice() {
        [a] => Ok((a.clone(), None)),
        [a, b] => Ok((a.clone(), Some(b.clone()))),
        _ => Error::err("`Result` must have one or two type parameters", ty),
    })
}

fn try_option<'a>(ty: &'a WithTokens<'a, Type>, name: &str) -> Result<Type> {
    try_param_type(ty, "Option", name).and_then(|types| match types.as_slice() {
        [a] => Ok(a.clone()),
        _ => Error::err("`Option` must have one type parameter", ty),
    })
}

impl<'a, O> FunctionArgs<'a, Cow<'a, [WithTokens<'a, Type>]>, O> {
    pub fn ref_param(mut self, index: usize) -> Result<Self> {
        match self.input.to_mut().get_mut(index) {
            None =>
                Error::err(format!("{} is missing", param_name(index)), self.meta.ident_to_tokens),
            Some(param) => {
                let result = try_ref(param, &param_name(index))?;
                param.value = result;
                Ok(self)
            }
        }
    }

    pub fn mut_param(mut self, index: usize) -> Result<Self> {
        match self.input.to_mut().get_mut(index) {
            None =>
                Error::err(format!("{} is missing", param_name(index)), self.meta.ident_to_tokens),
            Some(param) => {
                let result = try_mut(param, &param_name(index))?;
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

    pub fn ref_return(self) -> Result<FunctionArgs<'a, I, Type>> {
        let fa = self.try_return()?;
        let ty = try_ref(&fa.output, "return type")?;
        Ok(fa.with_output(ty))
    }

    /// Checks the function returns a mutable reference, i.e. is (`fn(...) -> &mut A;`, and sets
    /// the output to `A`.

    pub fn mut_return(self) -> Result<FunctionArgs<'a, I, Type>> {
        let fa = self.try_return()?;
        let ty = try_mut(&fa.output, "return type")?;
        Ok(fa.with_output(ty))
    }

    /// Checks the function returns a Result, i.e. is `fn(...) -> Result<U>;` or `fn(...) ->
    /// Result<T,U>;`, and sets the output to `(A, Option<B>)`.

    pub fn result_return(self) -> Result<FunctionArgs<'a, I, (Type, Option<Type>)>> {
        let fa = self.try_return()?;
        let tys = try_result(&fa.output, "return type")?;
        Ok(fa.with_output(tys))
    }

    /// Checks the function returns an Option, i.e. is `fn(...) -> Option<U>;`, and sets the output
    /// to `A`.

    pub fn option_return(self) -> Result<FunctionArgs<'a, I, Type>> {
        let fa = self.try_return()?;
        let tys = try_option(&fa.output, "return type")?;
        Ok(fa.with_output(tys))
    }
}
