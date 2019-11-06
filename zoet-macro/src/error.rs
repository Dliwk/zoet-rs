use core::{
    fmt,
    result::{self, Result as StdResult},
};
use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream};
use quote::{ToTokens, TokenStreamExt};
use std::{borrow::Cow, error::Error as StdError};

#[derive(Clone, Debug)]
struct SpanPair {
    start: Span,
    end: Span,
}
impl Default for SpanPair {
    fn default() -> Self {
        let default = Span::call_site();
        Self { start: default, end: default }
    }
}
impl From<TokenStream> for SpanPair {
    fn from(value: TokenStream) -> Self {
        // Inspired by syn::Error::new_spanned.
        let mut iter = value.into_iter();
        let start = iter.next().map_or_else(Span::call_site, |t| t.span());
        let end = iter.last().map_or(start, |t| t.span());
        Self { start, end }
    }
}

pub type Result<T> = result::Result<T, Error>;
#[derive(Debug)]
pub struct Error {
    source: Option<Box<dyn StdError>>,
    message: Cow<'static, str>,
    span: SpanPair,
}
impl Error {
    pub fn new(message: impl Into<Cow<'static, str>>, span: impl ToTokens) -> Self {
        Self { source: None, message: message.into(), span: span.into_token_stream().into() }
    }

    pub fn err<T>(message: impl Into<Cow<'static, str>>, span: impl ToTokens) -> Result<T> {
        Err(Self::new(message, span))
    }
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.message)
    }
}
pub trait ResultExt<T> {
    fn context(self, message: impl Into<Cow<'static, str>>, span: impl ToTokens) -> Result<T>;
}
impl<T, E: 'static + StdError> ResultExt<T> for StdResult<T, E> {
    fn context(self, message: impl Into<Cow<'static, str>>, span: impl ToTokens) -> Result<T> {
        self.map_err(|err| Error {
            source: Some(Box::new(err)),
            message: message.into(),
            span: span.into_token_stream().into(),
        })
    }
}
impl<T> ResultExt<T> for Option<T> {
    fn context(self, message: impl Into<Cow<'static, str>>, span: impl ToTokens) -> Result<T> {
        self.ok_or_else(|| Error {
            source: None,
            message: message.into(),
            span: span.into_token_stream().into(),
        })
    }
}

fn append_error(tokens: &mut TokenStream, message: &str, span: &SpanPair) {
    let SpanPair { start, end } = *span;

    // Somewhat inspired by syn::Error::to_compile_error.
    tokens.append(Ident::new("compile_error", start));
    tokens.append({
        let mut punct = Punct::new('!', Spacing::Alone);
        punct.set_span(end);
        punct
    });
    tokens.append({
        let mut group = Group::new(Delimiter::Brace, {
            let mut ts = TokenStream::new();
            ts.append({
                let mut string = Literal::string(message);
                string.set_span(end);
                string
            });
            ts
        });
        group.set_span(end);
        group
    });
}

impl ToTokens for Error {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        // let mut error: String = self.to_string();
        // let mut err: &dyn StdError = self;
        // while let Some(source) = err.source() {
        //     error.push_str("\ncontext: ");
        //     error.push_str(&source.to_string());
        //     err = source;
        // }
        // //if let Some(backtrace) = self.backtrace() {
        //     //append_error(tokens, &backtrace.to_string(), None, None);
        // //}
        // append_error(tokens, &error)
        append_error(tokens, &format!("#[zoet]: {}", self.message), &self.span);

        // append_error(tokens, &format!("DEBUG: {:?}", &self.span.token_stream), &self.span);
    }
}
