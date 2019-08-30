/// Processed data and its source tokens.
///
/// When transforming data, we sometimes want to keep track of the original source in case we need
/// to report an error. For example, when parsing a function signature, we may want to highlight
/// the `foo: &Foo` in the error message, but the actual value we want to work on is the `Foo`.
use crate::error::*;
use crate::self_replacer::*;
use core::{fmt, ops::Deref};
use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{fold::Fold, FnArg, ReturnType, Type};

/// Processed data and its source tokens.
#[derive(Clone, Copy)]
pub struct WithTokens<'a, T> {
    // The value.
    pub value: T,
    // The tokens representing the value, for diagnostic purposes.
    pub to_tokens: &'a dyn ToTokens,
}
impl<T> Deref for WithTokens<'_, T> {
    type Target = T;

    fn deref(&self) -> &T { &self.value }
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
    //fn new(value: T, to_tokens: &'a dyn ToTokens) -> Self { Self { value, to_tokens } }

    #[must_use = "This returns a new object with updated values. Were you expecting a mutator?"]
    pub fn with_value<U>(&self, value: U) -> WithTokens<'a, U> {
        WithTokens { value, to_tokens: self.to_tokens }
    }

    // #[must_use = "This returns a new object with updated values. Were you expecting a mutator?"]
    // fn with_tokens(&self, to_tokens: &'a dyn ToTokens) -> Self where T: Copy {
    //     Self { value: self.value, to_tokens }
    // }
}

impl<'a> WithTokens<'a, &'a ReturnType> {
    pub fn from_return_type(
        return_type: &'a ReturnType,
        self_ty: Option<&'a Type>,
    ) -> Result<WithTokens<'a, ReturnType>>
    {
        let to_tokens = return_type;
        let mut return_type = return_type.clone();

        if let Some(self_ty) = self_ty {
            return_type =
                SelfReplacer::try_replace(self_ty, return_type, SelfReplacer::fold_return_type)?;
        }

        Ok(WithTokens { to_tokens, value: return_type })
    }
}

impl<'a> WithTokens<'a, Type> {
    pub fn from_fn_arg(
        fn_arg: &'a FnArg,
        self_ty: Option<&'a Type>,
    ) -> Result<WithTokens<'a, Type>>
    {
        let to_tokens = fn_arg;
        let mut fn_arg = fn_arg.clone();

        if let Some(self_ty) = self_ty {
            fn_arg = SelfReplacer::try_replace(self_ty, fn_arg, SelfReplacer::fold_fn_arg)?;
        }

        match fn_arg {
            FnArg::Typed(pat_type) => Ok(WithTokens { to_tokens, value: *(pat_type.ty) }),
            FnArg::Receiver(_) => Error::err(
                "unexpected `self`: did you forget to add #[zoet] to this method's impl?",
                fn_arg,
            ),
        }
    }
}
