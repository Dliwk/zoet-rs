//! Function signature checking and processing.
//!
//! Rather than hand-crufting parsers for function signatures to extract the parameter types,
//! `FunctionArgs` contains the function input, output, and useful metadata, and methods for
//! checking and transforming the input and output. Users of this library will normally use this as
//! a pipeline, starting with the relatively raw `syn` nodes and ultimately producing simple types
//! which can be easily slotted into the `quote!` macro.

use crate::{prelude::*, with_tokens::*};
use core::convert::TryFrom;
use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use std::borrow::Cow;
use syn::{
    parse_quote, spanned::Spanned, GenericArgument, Generics, Ident, PathArguments, PathSegment,
    ReturnType, Signature, Type, TypePath, TypeReference,
};
/// Processing of function input and output types.
///
/// This is a selection of methods which validate and transform function signatures into a form
/// suitable for placing into templates.

#[derive(Clone, Copy)]
pub(crate) struct FunctionMeta<'a> {
    // the original function signature, used in errors for when the entire shape of the function is
    // wrong so a more precise location can't be given
    pub(crate) signature: &'a Signature,

    // the attribute fragment (e.g. "Default") which caused this trait to be generated
    pub(crate) derive_span: Span,

    // And these are bits pasted into the output traits

    // A path/type fragment which describes how to call the functions, e.g. "foo", "Foo::bar".
    pub(crate) to_call: &'a TokenStream,
    // The generic parameters; both the <...> part and where clause
    pub(crate) generics: &'a Generics,
    // Extra attributes to add to the trait
    pub(crate) extra_attrs: &'a dyn ToTokens,
}

impl FunctionMeta<'_> {
    fn diagnostic_error(&self, spanned: &impl Spanned, message: &impl ToString) -> Diagnostic {
        diagnostic_error!(
            self.derive_span, "function `{}` cannot be adapted to this trait", self.signature.ident,
                ; note = spanned.span() => message,
        )
    }

    fn result<T>(&self, spanned: &impl Spanned, message: &impl ToString) -> Result<T> {
        Err(self.diagnostic_error(spanned, message))
    }

    fn check_ref<'a>(&self, ty: &'a WithTokens<'a, Type>, name: &str) -> Result<()> {
        match ty.value {
            Type::Reference(TypeReference {
                mutability: None, ..
            }) => Ok(()),
            _ => self.result(ty, &format!("{} must be an immutable reference", name)),
        }
    }

    // TODO: handle lifetime arguments. This probably involves reworking our data model somewhat.
    fn unwrap_ref<'a>(&self, ty: &'a WithTokens<'a, Type>, name: &str) -> Result<Type> {
        match ty.value {
            Type::Reference(TypeReference {
                lifetime: None,
                mutability: None,
                ref elem,
                ..
            }) => Ok(elem.as_ref().clone()),
            _ => self.result(
                ty,
                &format!(
                    "{} must be an immutable reference and not have a lifetime argument",
                    name
                ),
            ),
        }
    }

    // TODO: handle lifetime arguments
    fn unwrap_mut<'a>(&self, ty: &'a WithTokens<'a, Type>, name: &str) -> Result<Type> {
        match ty.value {
            Type::Reference(TypeReference {
                lifetime: None,
                mutability: Some(_),
                ref elem,
                ..
            }) => Ok(elem.as_ref().clone()),
            _ => self.result(
                ty,
                &format!(
                    "{} must be a mutable reference and not have a lifetime argument",
                    name
                ),
            ),
        }
    }

    #[allow(clippy::type_complexity)]
    fn unwrap_param_type<'a>(
        &self, ty: &'a WithTokens<'a, Type>,
    ) -> Result<Option<(&'a Ident, Box<[Type]>)>> {
        if let Type::Path(TypePath {
            qself: None,
            ref path,
        }) = ty.value
        {
            let last = path
                .segments
                .last()
                .expect_or_abort("TypePath::path is always nonempty");
            if let PathSegment {
                ref ident,
                arguments: PathArguments::AngleBracketed(ref abga),
            } = *last
            {
                let args = abga
                    .args
                    .iter()
                    .map(|arg| match *arg {
                        GenericArgument::Type(ref ty) => Ok(ty.clone()),
                        _ => self.result(arg, &"this generic parameter must be a type"),
                    })
                    .collect::<Result<_>>()?;
                return Ok(Some((ident, args)));
            }
        }
        Ok(None)
    }

    fn unwrap_result<'a>(&self, ty: &'a WithTokens<'a, Type>, name: &str) -> Result<(Type, Type)> {
        if let Some((ident, boxed)) = self.unwrap_param_type(ty)? {
            match (ident.to_string().as_str(), &*boxed) {
                ("Result", &[ref a, ref b]) => return Ok((a.clone(), b.clone())),
                ("Result", &[ref a]) => return Ok((a.clone(), parse_quote! { Error })),
                _ => (),
            }
        }
        self.result(
            ty,
            &format!(
                "{} must be `Result<_>`, `Result<_,_>` or `Fallible<_>`",
                name
            ),
        )
    }

    fn unwrap<'a>(&self, ty: &'a WithTokens<'a, Type>, wrapper: &str, name: &str) -> Result<Type> {
        if let Some((ident, boxed)) = self.unwrap_param_type(ty)? {
            if let (w, &[ref a]) = (ident.to_string().as_str(), &*boxed) {
                if w == wrapper {
                    return Ok(a.clone());
                }
            }
        }
        self.result(ty, &format!("{} must be `{}<_>`", name, wrapper))
    }
}

#[derive(Clone, Copy)]
pub(crate) struct FunctionArgs<'a, I, O> {
    pub(crate) input: I,
    pub(crate) output: O,
    pub(crate) meta: FunctionMeta<'a>,
}

impl<'a, I, O> FunctionArgs<'a, I, O> {
    fn with_input<I2>(self, input: I2) -> FunctionArgs<'a, I2, O> {
        FunctionArgs {
            input,
            output: self.output,
            meta: self.meta,
        }
    }

    fn with_output<O2>(self, output: O2) -> FunctionArgs<'a, I, O2> {
        FunctionArgs {
            input: self.input,
            output,
            meta: self.meta,
        }
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

impl<'a, O> FunctionArgs<'a, Box<[WithTokens<'a, Type>]>, O> {
    fn param_name(index: usize) -> Cow<'static, str> {
        match index {
            0 => Cow::Borrowed("first parameter"),
            1 => Cow::Borrowed("second parameter"),
            other => Cow::Owned(format!("parameter {}", other + 1)),
        }
    }

    pub(crate) fn check_ref_param(mut self, index: usize) -> Result<Self> {
        match self.input.get_mut(index) {
            None => Err(self.meta.diagnostic_error(
                self.meta.signature,
                &format!("{} is missing", Self::param_name(index)),
            )),
            Some(param) => {
                self.meta.check_ref(param, &Self::param_name(index))?;
                Ok(self)
            }
        }
    }

    pub(crate) fn unwrap_ref_param(mut self, index: usize) -> Result<Self> {
        match self.input.get_mut(index) {
            None => Err(self.meta.diagnostic_error(
                self.meta.signature,
                &format!("{} is missing", Self::param_name(index)),
            )),
            Some(param) => {
                let result = self.meta.unwrap_ref(param, &Self::param_name(index))?;
                param.value = result;
                Ok(self)
            }
        }
    }

    pub(crate) fn unwrap_mut_param(mut self, index: usize) -> Result<Self> {
        match self.input.get_mut(index) {
            None => Err(self.meta.diagnostic_error(
                self.meta.signature,
                &format!("{} is missing", Self::param_name(index)),
            )),
            Some(param) => {
                let result = self.meta.unwrap_mut(param, &Self::param_name(index))?;
                param.value = result;
                Ok(self)
            }
        }
    }

    pub(crate) fn unwrap_param(mut self, index: usize, wrapper: &str) -> Result<Self> {
        match self.input.get_mut(index) {
            None => Err(self.meta.diagnostic_error(
                self.meta.signature,
                &format!("{} is missing", Self::param_name(index)),
            )),
            Some(param) => {
                let result = self.meta.unwrap(param, wrapper, &Self::param_name(index))?;
                param.value = result;
                Ok(self)
            }
        }
    }

    pub(crate) fn nullary(self) -> Result<FunctionArgs<'a, (), O>> {
        if self.input.is_empty() {
            Ok(self.with_input(()))
        } else {
            Err(self
                .meta
                .diagnostic_error(self.meta.signature, &"function should take zero parameters"))
        }
    }

    pub(crate) fn unary(self) -> Result<FunctionArgs<'a, Type, O>> {
        let Self {
            input,
            output,
            meta,
        } = self;

        <Box<[_; 1]>>::try_from(input)
            .map_err(|_box| {
                meta.diagnostic_error(meta.signature, &"function should take one parameter")
            })
            .map(|boxed| match *boxed {
                [a] => FunctionArgs {
                    input: a.value,
                    output,
                    meta,
                },
            })
    }

    pub(crate) fn binary(self) -> Result<FunctionArgs<'a, (Type, Type), O>> {
        let Self {
            input,
            output,
            meta,
        } = self;

        <Box<[_; 2]>>::try_from(input)
            .map_err(|_box| {
                meta.diagnostic_error(meta.signature, &"function should take two parameters")
            })
            .map(|boxed| match *boxed {
                [a, b] => FunctionArgs {
                    input: (a.value, b.value),
                    output,
                    meta,
                },
            })
    }
}

impl<'a, I> FunctionArgs<'a, I, WithTokens<'a, ReturnType>> {
    fn try_return(self) -> Result<FunctionArgs<'a, I, WithTokens<'a, Type>>> {
        match self.output.value {
            ReturnType::Type(_, ref ty) => {
                let ty = *ty.clone();
                Ok(self.with_output_value(ty))
            }
            ReturnType::Default => Err(self
                .meta
                .diagnostic_error(self.meta.signature, &"function must return a value")),
        }
    }

    /// Checks the function returns something, i.e. is `fn(...) -> T;`.

    pub(crate) fn has_return(self) -> Result<FunctionArgs<'a, I, Type>> {
        let fa = self.try_return()?;
        let value = fa.output.value.clone();
        Ok(fa.with_output(value))
    }

    /// Checks the function returns nothing, i.e. is `fn(...);`.

    pub(crate) fn default_return(self) -> Result<FunctionArgs<'a, I, ()>> {
        match *self.output {
            ReturnType::Type(..) => Err(self
                .meta
                .diagnostic_error(self.meta.signature, &"function must return nothing")),
            ReturnType::Default => Ok(self.with_output(())),
        }
    }

    /*
    pub(crate) fn any_return(self) -> FunctionArgs<'a, I, Type> {
        match self.output.value {
            ReturnType::Type(_, ref ty) => {
                let ty = ty.as_ref().clone();
                self.with_output(ty)
            }
            ReturnType::Default => self.with_output(parse_quote! {()}),
        }
    }
     */

    /// Checks the function returns a reference, i.e. is `fn(...) -> &A;`, and sets the output to
    /// `A`.

    pub(crate) fn unwrap_ref_return(self) -> Result<FunctionArgs<'a, I, Type>> {
        let fa = self.try_return()?;
        let ty = fa.meta.unwrap_ref(&fa.output, "return type")?;
        Ok(fa.with_output(ty))
    }

    /// Checks the function returns a mutable reference, i.e. is (`fn(...) -> &mut A;`, and sets
    /// the output to `A`.

    pub(crate) fn unwrap_mut_return(self) -> Result<FunctionArgs<'a, I, Type>> {
        let fa = self.try_return()?;
        let ty = fa.meta.unwrap_mut(&fa.output, "return type")?;
        Ok(fa.with_output(ty))
    }

    /// Checks the function returns a Result, i.e. is `fn(...) -> Result<U>;` or `fn(...) ->
    /// Result<T,U>;`, and sets the output to `(A, Option<B>)`.

    pub(crate) fn unwrap_result_return(self) -> Result<FunctionArgs<'a, I, (Type, Type)>> {
        let fa = self.try_return()?;
        let tys = fa.meta.unwrap_result(&fa.output, "return type")?;
        Ok(fa.with_output(tys))
    }

    /// Checks the function returns a wrapped type, i.e. is `fn(...) -> #wrapper<U>;`, and sets the
    /// output to `A`.

    pub(crate) fn unwrap_return(self, wrapper: &str) -> Result<FunctionArgs<'a, I, Type>> {
        let fa = self.try_return()?;
        let tys = fa.meta.unwrap(&fa.output, wrapper, "return type")?;
        Ok(fa.with_output(tys))
    }
}
