/// Processed data and its source tokens.
///
/// When transforming data, we sometimes want to keep track of the original source in case we need
/// to report an error. For example, when parsing a function signature, we may want to highlight
/// the `foo: &Foo` in the error message, but the actual value we want to work on is the `Foo`.
use crate::prelude::*;
use crate::self_replacer::*;
use core::{fmt, ops::Deref};
use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{fold::Fold, FnArg, ReturnType, Type};

/// Processed data and its source tokens.
#[derive(Clone, Copy)]
pub(crate) struct WithTokens<'a, T> {
    // The value.
    pub(crate) value: T,
    // The tokens representing the value, for diagnostic purposes.
    to_tokens: &'a dyn ToTokens,
}
impl<T> Deref for WithTokens<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}
impl<T> ToTokens for WithTokens<'_, T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.to_tokens.into_token_stream());
    }
}
impl<T: fmt::Debug> fmt::Debug for WithTokens<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("SpanPair")
            .field("value", &self.value)
            //.field("to_tokens", &self.to_tokens)
            .finish()
    }
}
impl<'a, T> WithTokens<'a, T> {
    // fn new(value: T, to_tokens: &'a dyn ToTokens) -> Self { Self { value, to_tokens } }

    #[must_use = "This returns a new object with updated values. Were you expecting a mutator?"]
    pub(crate) fn with_value<U>(&self, value: U) -> WithTokens<'a, U> {
        WithTokens {
            value,
            to_tokens: self.to_tokens,
        }
    }

    // #[must_use = "This returns a new object with updated values. Were you expecting a mutator?"]
    // fn with_tokens(&self, to_tokens: &'a dyn ToTokens) -> Self where T: Copy {
    //     Self { value: self.value, to_tokens }
    // }
}

impl<'a> WithTokens<'a, &'a ReturnType> {
    pub(crate) fn from_return_type(
        return_type: &'a ReturnType,
        self_ty: Option<&'a Type>,
    ) -> WithTokens<'a, ReturnType> {
        let to_tokens = return_type;
        let mut value = return_type.clone();

        if let Some(self_ty) = self_ty {
            value = SelfReplacer::new(self_ty).fold_return_type(value);
        }

        WithTokens { value, to_tokens }
    }
}

impl<'a> WithTokens<'a, Type> {
    pub(crate) fn from_fn_arg(
        fn_arg: &'a FnArg,
        self_ty: Option<&'a Type>,
    ) -> WithTokens<'a, Type> {
        let to_tokens = fn_arg;
        let mut value = fn_arg.clone();

        if let Some(self_ty) = self_ty {
            value = SelfReplacer::new(self_ty).fold_fn_arg(value);
        }

        let value = match value {
            FnArg::Typed(pat_type) => *pat_type.ty,
            FnArg::Receiver(_) => abort!(
                fn_arg,
                "unexpected `self`: did you forget to add #[zoet] to this method's impl?"
            ),
        };

        WithTokens { value, to_tokens }
    }
}
