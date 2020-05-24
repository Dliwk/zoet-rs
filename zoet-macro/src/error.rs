use core::{
    fmt,
    result::{self, Result as StdResult},
};
use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};
use std::{borrow::Cow, error::Error as StdError};
use syn::spanned::Spanned;

pub type Result<T> = result::Result<T, Error>;
#[derive(Debug)]
pub struct Error {
    source: Option<Box<dyn StdError>>,
    message: Cow<'static, str>,
    span: Span,
}
impl Error {
    pub fn new(message: impl Into<Cow<'static, str>>, span: impl Spanned) -> Self {
        Self { source: None, message: message.into(), span: span.span() }
    }

    pub fn err<T>(message: impl Into<Cow<'static, str>>, span: impl Spanned) -> Result<T> {
        Err(Self::new(message, span))
    }
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.message)
    }
}
pub trait ResultExt<T> {
    fn context(self, message: impl Into<Cow<'static, str>>, span: impl Spanned) -> Result<T>;
}
impl<T, E: 'static + StdError> ResultExt<T> for StdResult<T, E> {
    fn context(self, message: impl Into<Cow<'static, str>>, span: impl Spanned) -> Result<T> {
        self.map_err(|err| Error {
            source: Some(Box::new(err)),
            message: message.into(),
            span: span.span(),
        })
    }
}
impl<T> ResultExt<T> for Option<T> {
    fn context(self, message: impl Into<Cow<'static, str>>, span: impl Spanned) -> Result<T> {
        self.ok_or_else(|| Error { source: None, message: message.into(), span: span.span() })
    }
}

impl ToTokens for Error {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let error = format!("#[zoet]: {}", self.message);
        let error = quote_spanned! {
            self.span =>
                compile_error!( #error );
        };
        tokens.extend(error);
    }
}
