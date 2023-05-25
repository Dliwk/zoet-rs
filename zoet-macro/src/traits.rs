use crate::{function_args::*, prelude::*, with_tokens::*};
use proc_macro2::TokenStream;
use quote::quote_spanned;
use syn::{parse_quote as pq, Ident, Path, ReturnType, Type};

pub(crate) type GenIn<'a> =
    FunctionArgs<'a, Box<[WithTokens<'a, Type>]>, WithTokens<'a, ReturnType>>;
pub(crate) type GenOut = Result<TokenStream>;
pub(crate) type GenFn = fn(GenIn) -> GenOut;

pub(crate) fn get_trait_fn(ident: &Ident) -> Result<GenFn> {
    let key = ident.to_string();

    let has_alloc = || -> Result<bool> {
        if cfg!(feature = "alloc") {
            Ok(true)
        } else {
            Err(diagnostic_error! {
                ident, "this trait requires an allocator"
                    ; help = "enable zoet's `alloc` feature"
            })
        }
    };

    /*
    let has_nightly = || -> Result<bool> {
        if cfg!(feature = "nightly") {
            Ok(true)
        } else {
            Err(diagnostic_error! {
                ident, "this trait requires a nightly compiler"
                    ; help = "enable zoet's `nightly` feature"
            })
        }
    };
     */

    let is_marker = || {
        Err(diagnostic_error! {
            ident, "this is a marker trait"
                ; help = "`#[derive({})]` on the struct/enum/union definition instead", key
        })
    };

    let func: GenFn = match &*key {
        // std::alloc
        // "Allocator" => Requires two functions to be implemented.
        // "GlobalAlloc" => Requires two functions to be implemented.

        // std::any
        // "Any" => possible, but it's already automatically-implemented by the compiler.
        // "Provider" => nightly.

        // std::borrow
        "Borrow" => |f| borrow_shaped(f, &pq!(::core::borrow::Borrow), &pq!(borrow)),
        "BorrowMut" => |f| borrow_mut_shaped(f, &pq!(::core::borrow::BorrowMut), &pq!(borrow_mut)),
        "ToOwned" if has_alloc()? => to_owned,

        // std::clone
        "Clone" => clone,

        // std::cmp
        // "Eq" => is a marker trait, #[derive(Eq)] instead.
        "Eq" => return is_marker(),
        "Ord" => ord,
        "PartialEq" => partial_eq,
        "PartialOrd" => partial_ord,

        // std::convert
        "AsMut" => |f| borrow_mut_shaped(f, &pq!(::core::convert::AsMut), &pq!(as_mut)),
        "AsRef" => |f| borrow_shaped(f, &pq!(::core::convert::AsRef), &pq!(as_ref)),
        "From" => from,
        "Into" => into,
        "TryFrom" => try_from,
        "TryInto" => try_into,

        // std::default
        "Default" => default,

        // std::error
        // "Error" => possible because all methods are defaulted, but which method should we
        // implement? Use e.g. thiserror instead.

        // std::fmt
        "Binary" => |f| debug_shaped(f, &pq!(::core::fmt::Binary)),
        "Debug" => |f| debug_shaped(f, &pq!(::core::fmt::Debug)),
        "Display" => |f| debug_shaped(f, &pq!(::core::fmt::Display)),
        "LowerExp" => |f| debug_shaped(f, &pq!(::core::fmt::LowerExp)),
        "LowerHex" => |f| debug_shaped(f, &pq!(::core::fmt::LowerHex)),
        "Octal" => |f| debug_shaped(f, &pq!(::core::fmt::Octal)),
        "Pointer" => |f| debug_shaped(f, &pq!(::core::fmt::Pointer)),
        "UpperExp" => |f| debug_shaped(f, &pq!(::core::fmt::UpperExp)),
        "UpperHex" => |f| debug_shaped(f, &pq!(::core::fmt::UpperHex)),
        "Write" => write,

        // std::future
        "Future" => future,
        "IntoFuture" => into_future,

        // std::hash
        // "BuildHasher" => possible, arguably TODO.
        "Hash" => hash,
        // "Hasher" => Requires two functions to be implemented.

        // std::io
        // Read/Seek are single-function traits, but BufRead/Write require two. Probably best to
        // avoid std::io completely. Also namespace clash with std::fmt::Write.

        // std::iterator
        // DoubleEndedIterator::next_back
        // ExactSizeIterator::len
        // Extend::extend // fn has a generic type
        // "FromIterator" => from_iterator, // fn is rather complex
        // "FusedIterator" => is a marker trait.
        "IntoIterator" => into_iterator,
        "Iterator" => iterator,
        // Product::product // fn has a generic type
        // Step => Requires several functions to be implemented.
        // Sum::sum // fn has a generic type
        // TrustedLen // marker trait
        // TrustedStep // marker trait

        // std::net
        // "ToSocketAddrs" => doable but does it make sense?

        // std::ops
        "Add" => |f| add_shaped(f, &pq!(::core::ops::Add), &pq!(add)),
        "AddAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::AddAssign), &pq!(add_assign)),
        "BitAnd" => |f| add_shaped(f, &pq!(::core::ops::BitAnd), &pq!(bitand)),
        "BitAndAssign" =>
            |f| add_assign_shaped(f, &pq!(::core::ops::BitAndAssign), &pq!(bitand_assign)),
        "BitOr" => |f| add_shaped(f, &pq!(::core::ops::BitOr), &pq!(bitor)),
        "BitOrAssign" =>
            |f| add_assign_shaped(f, &pq!(::core::ops::BitOrAssign), &pq!(bitor_assign)),
        "BitXor" => |f| add_shaped(f, &pq!(::core::ops::BitXor), &pq!(bitxor)),
        "BitXorAssign" =>
            |f| add_assign_shaped(f, &pq!(::core::ops::BitXorAssign), &pq!(bitxor_assign)),
        // "CoerceUnsigned" => marker trait
        "Deref" => deref,
        "DerefMut" => deref_mut,
        // "DispatchFromDyn" => marker trait
        "Div" => |f| add_shaped(f, &pq!(::core::ops::Div), &pq!(div)),
        "DivAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::DivAssign), &pq!(div_assign)),
        "Drop" => drop,
        // "Fn" / "FnMut" / "FnOnce": doable, but are nightly-unstable.
        //"Fn" => r#fn,
        // "FnOnce" if has_nightly()? => fn_once,
        // "FromResidual": doable, but is nightly-unstable.
        // "Generator": possibly doable, but is nightly-unstable.
        "Index" => index,
        "IndexMut" => index_mut,
        "Mul" => |f| add_shaped(f, &pq!(::core::ops::Mul), &pq!(mul)),
        "MulAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::MulAssign), &pq!(mul_assign)),
        "Neg" => |f| neg_shaped(f, &pq!(::core::ops::Neg), &pq!(neg)),
        "Not" => |f| neg_shaped(f, &pq!(::core::ops::Not), &pq!(not)),
        // "RangeBounds": requires two functions.
        "Rem" => |f| add_shaped(f, &pq!(::core::ops::Rem), &pq!(rem)),
        "RemAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::RemAssign), &pq!(rem_assign)),
        "Shl" => |f| add_shaped(f, &pq!(::core::ops::Shl), &pq!(shl)),
        "ShlAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::ShlAssign), &pq!(shl_assign)),
        "Shr" => |f| add_shaped(f, &pq!(::core::ops::Shr), &pq!(shr)),
        "ShrAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::ShrAssign), &pq!(shr_assign)),
        "Sub" => |f| add_shaped(f, &pq!(::core::ops::Sub), &pq!(sub)),
        "SubAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::SubAssign), &pq!(sub_assign)),
        // "Try" => requires two functions

        // std::str
        "FromStr" => from_str,

        // std::string
        "ToString" if has_alloc()? => to_string,

        _ =>
            return Err(diagnostic_error!(
                ident,
                "this trait name is not recognised"
            )),
    };

    Ok(func)
}

/// `fn(&A) -> &T` ⇛ `impl Trait<T> for A { fn op(&self) -> &T }`
fn borrow_shaped(func: GenIn, trait_name: &Path, method_name: &Ident) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unwrap_ref_param(0)?.unary()?.unwrap_ref_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics #trait_name <#output> for #input #where_clause {
            #[inline]
            fn #method_name (&self) -> &#output {
                #to_call(self)
            }
        }
    })
}
/// `fn(&mut A) -> &T` ⇛ `impl Trait<T> for A { fn op(&mut self) -> &mut T }`
fn borrow_mut_shaped(func: GenIn, trait_name: &Path, method_name: &Ident) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unwrap_mut_param(0)?.unary()?.unwrap_mut_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics #trait_name <#output> for #input #where_clause {
            #[inline]
            fn #method_name (&mut self) -> &mut #output {
                #to_call(self)
            }
        }
    })
}
/// `fn(&A) -> T` ⇛ `impl Trait for A { type Assoc = T; fn op(&self) -> T }`
fn to_owned(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unwrap_ref_param(0)?.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::zoet::__alloc::borrow::ToOwned for #input #where_clause {
            type Owned = #output;
            #[inline]
            fn to_owned(&self) -> Self::Owned {
                #to_call(self)
            }
        }
    })
}
/// `fn(&A) -> T` ⇛ `impl Trait for A { fn op(&self) -> #out }` (`A` = `T`)
fn clone(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unwrap_ref_param(0)?.unary()?.has_return()?; // TODO: create and use a .self_return() instead?
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::clone::Clone for #input #where_clause {
            #[inline]
            fn clone(&self) -> #output {
                #to_call(self)
            }
        }
    })
}

// The comparison operators give us a certain amount of grief. It is probably not unreasonable to
// want to write `#[zoet(PartialEq, Ord, PartialOrd)]` and have the macro generate the right sort of
// signatures.
//
// Ord wants `Fn(T, T) -> Ordering`. None of the signatures below will do, so it's at least easy
// enough.
//
// PartialOrd wants `Fn(T, U) -> Option<Ordering>`. We can also trivially generate that shape from
// Ord. Thus it needs to look for `Option` and switch implementation.
//
// PartialEq wants `Fn(T, U) -> bool`. Again, this can be generated from the other two.

enum CompareShape {
    // -> Option<_>
    PartialOrd,

    // -> bool
    PartialEq,

    // -> _
    Ord,
}
#[allow(clippy::type_complexity)] // Yeah, whatever.
fn compare_shape(func: GenIn) -> Result<(CompareShape, FunctionArgs<(Type, Type), Type>)> {
    // We'll first make sure it looks like `Fn(T, U) -> V`.
    let filtered = func.unwrap_ref_param(0)?.unwrap_ref_param(1)?.binary()?;

    // Does it return Option<_> ?
    let ok = if let Ok(args) = filtered.clone().unwrap_return("Option") {
        (CompareShape::PartialOrd, args)
    } else {
        let args = filtered.has_return()?;
        match args.output {
            Type::Path(ref type_path) if type_path.path.is_ident("bool") =>
                (CompareShape::PartialEq, args),
            _ => (CompareShape::Ord, args),
        }
    };

    Ok(ok)
}

fn ord(func: GenIn) -> Result<TokenStream> {
    let (
        shape,
        FunctionArgs {
            input: (lhs, rhs),
            output,
            meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
        },
    ) = compare_shape(func)?;

    let fn_body = match shape {
        CompareShape::Ord => quote_spanned! {
            derive_span => #to_call(self, other)
        },
        _ =>
            return Err(diagnostic_error! {
                output, "`Ord` requires a function returning `Ordering`"
            }),
    };

    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::cmp::Ord for #lhs #where_clause {
            fn cmp(&self, other: &#rhs) -> ::core::cmp::Ordering {
                #fn_body
            }
        }
    })
}

fn partial_eq(func: GenIn) -> Result<TokenStream> {
    let (
        shape,
        FunctionArgs {
            input: (lhs, rhs),
            meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
            ..
        },
    ) = compare_shape(func)?;

    let fn_body = match shape {
        CompareShape::PartialOrd => quote_spanned! {
            derive_span =>
                #to_call(self, other) == ::core::option::Option::Some(::core::cmp::Ordering::Equals)
        },
        CompareShape::PartialEq => quote_spanned! {
            derive_span => #to_call(self, other)
        },
        CompareShape::Ord => quote_spanned! {
            derive_span => #to_call(self, other) == ::core::cmp::Ordering::Equal
        },
    };

    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::cmp::PartialEq<#rhs> for #lhs #where_clause {
            #[inline]
            fn eq(&self, other: &#rhs) -> ::core::primitive::bool {
                #fn_body
            }
        }
    })
}

fn partial_ord(func: GenIn) -> Result<TokenStream> {
    let (
        shape,
        FunctionArgs {
            input: (lhs, rhs),
            output,
            meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
        },
    ) = compare_shape(func)?;

    let fn_body = match shape {
        CompareShape::PartialOrd => quote_spanned! {
            derive_span => #to_call(self, other)
        },
        CompareShape::Ord => quote_spanned! {
            derive_span => ::core::option::Option::Some(#to_call(self, other))
        },
        CompareShape::PartialEq =>
            return Err(diagnostic_error! {
                output, "`Partial` requires a function returning `Ordering` or `Option<Ordering>`"
            }),
    };

    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::cmp::PartialOrd<#rhs> for #lhs #where_clause {
            #[inline]
            fn partial_cmp(&self, other: &#rhs) -> ::core::option::Option<::core::cmp::Ordering> {
                #fn_body
            }
        }
    })
}

/// `fn(A) -> T` ⇛ `impl Trait<A> for T { fn op(A) -> T }`
fn from(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unary()?.has_return()?; // TODO: .self_return()?
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::convert::From<#input> for #output #where_clause {
            #[inline]
            fn from(value: #input) -> Self {
                #to_call(value)
            }
        }
    })
}
/// `fn(A) -> T` ⇛ `impl Trait<T> for A { fn op(self) -> T }`
fn into(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::convert::Into<#output> for #input #where_clause {
            #[inline]
            fn into(self) -> #output {
                #to_call(self)
            }
        }
    })
}
/// `fn(A) -> Result<T, U>` ⇛
/// `impl Trait<A> for T { type Assoc = U; fn op(A) -> Result<T, U> }`
fn try_from(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output: (output, err),
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unary()?.unwrap_result_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::convert::TryFrom<#input> for #output #where_clause {
            type Error = #err;
            #[inline]
            fn try_from(value: #input) -> ::core::result::Result<Self, Self::Error> {
                #to_call(value)
            }
        }
    })
}
/// `fn(A) -> Result<T, U>` ⇛ `impl Trait<T> for A { type Assoc = U; fn op(self) -> Result<T, U> }`
fn try_into(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output: (output, err),
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unary()?.unwrap_result_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::convert::TryInto<#output> for #input #where_clause {
            type Error = #err;
            #[inline]
            fn try_into(self) -> ::core::result::Result<#output, Self::Error> {
                #to_call(self)
            }
        }
    })
}
/// `fn() => T` ⇛ `impl Trait for T { fn op() -> T }`
fn default(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input: (),
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.nullary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::default::Default for #output #where_clause {
            #[inline]
            fn default() -> Self {
                #to_call()
            }
        }
    })
}
/// `fn(&A, &mut B) -> T` ⇛ `impl Trait for A { fn op(&self, &mut B) -> T }`
fn debug_shaped(func: GenIn, trait_name: &Path) -> Result<TokenStream> {
    let FunctionArgs {
        input: (obj, formatter),
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func
        .unwrap_ref_param(0)?
        .unwrap_mut_param(1)?
        .binary()?
        .has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics #trait_name for #obj #where_clause {
            #[inline]
            fn fmt(&self, f: &mut #formatter) -> #output {
                #to_call(self, f)
            }
        }
    })
}
/// `fn(&mut A, &B) -> T` ⇛ `impl Trait for A { fn op(&mut self, &B) -> T }`
fn write(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input: (obj, str),
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func
        .unwrap_mut_param(0)?
        .unwrap_ref_param(1)?
        .binary()?
        .has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::fmt::Write for #obj #where_clause {
            #[inline]
            fn write_str(&mut self, s: &#str) -> #output {
                #to_call(self, s)
            }
        }
    })
}

/// `fn(Poll<&mut A>, &mut Context) -> T`
fn future(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input: (obj, _),
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func
        .unwrap_param(0, "Pin")?
        .unwrap_mut_param(0)?
        .binary()?
        .unwrap_return("Poll")?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::future::Future for #obj #where_clause {
            type Output = #output;

            #[inline]
            fn poll(
                self: ::core::pin::Pin<&mut Self>,
                cx: &mut ::core::task::Context
            ) -> ::core::task::Poll<Self::Output> {
                #to_call(self, cx)
            }
        }
    })
}

/// `fn(self) -> impl Future<Output=T>`
fn into_future(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::future::IntoFuture for #input #where_clause {
            type IntoFuture = #output;
            type Output = <Self::IntoFuture as ::core::future::Future>::Output;

            #[inline]
             fn into_future(self) -> Self::IntoFuture {
                 #to_call(self)
             }
        }
    })
}

/// (Signature is particularly trait-specific.)
fn hash(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input: (obj, _hasher),
        output: (),
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func
        .unwrap_ref_param(0)?
        .unwrap_mut_param(1)?
        .binary()?
        .default_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::hash::Hash for #obj #where_clause {
            #[inline]
            fn hash<H: ::core::hash::Hasher>(&self, state: &mut H) {
                #to_call(self, state);
            }
        }
    })
}

/// `fn(A) -> T` ⇛
/// `impl Trait for A { type Assoc1 = Out; type Assoc2 = <Out as Trait>::Assoc2; fn op(self) -> Out }`
fn into_iterator(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::iter::IntoIterator for #input #where_clause {
            type IntoIter = #output;
            type Item = <Self::IntoIter as ::core::iter::Iterator>::Item;
            #[inline]
            fn into_iter(self) -> Self::IntoIter {
                #to_call(self)
            }
        }
    })
}
/// `fn(&mut A) -> Option<T>` ⇛
/// `impl Trait for A { type Assoc1 = Out; fn op(&mut In) -> Option<Out> }`
fn iterator(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unwrap_mut_param(0)?.unary()?.unwrap_return("Option")?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::iter::Iterator for #input #where_clause {
            type Item = #output;
            #[inline]
            fn next(&mut self) -> Option<Self::Item> {
                #to_call(self)
            }
        }
    })
}
/// Switches implementation based on function signature:
/// * `fn(&mut A, B)` ⇛ `impl Trait<B> for A { fn op(&mut self, B) }`
/// * `fn(A, B) -> T` ⇛ implementation which doesn't just forward the call
// fn add_shaped(func: GenIn, trait_name: &Path, method_name: &Ident) -> Result<TokenStream> {
fn add_shaped(func: GenIn, trait_name: &Path, method_name: &Ident) -> Result<TokenStream> {
    if let Ok(ts) = add_from_add_assign_shaped(func.clone(), trait_name, method_name) {
        return Ok(ts);
    }

    let FunctionArgs {
        input: (lhs, rhs),
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.binary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics #trait_name<#rhs> for #lhs #where_clause {
            type Output = #output;
            #[inline]
            fn #method_name(self, rhs: #rhs) -> Self::Output {
                #to_call(self, rhs)
            }
        }
    })
}

fn add_from_add_assign_shaped(
    func: GenIn, trait_name: &Path, method_name: &Ident,
) -> Result<TokenStream> {
    let FunctionArgs {
        input: (lhs, rhs),
        output: (),
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unwrap_mut_param(0)?.binary()?.default_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics #trait_name<#rhs> for #lhs #where_clause {
            type Output = #lhs;
            #[inline]
            fn #method_name(mut self, rhs: #rhs) -> #lhs {
                #to_call(&mut self, rhs) ; self
            }
        }
    })
}

/// `fn(&mut A, B)` ⇛ `impl Trait<B> for A { fn op(&mut self, B) }`
fn add_assign_shaped(func: GenIn, trait_name: &Path, method_name: &Ident) -> Result<TokenStream> {
    let FunctionArgs {
        input: (lhs, rhs),
        output: (),
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unwrap_mut_param(0)?.binary()?.default_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics #trait_name<#rhs> for #lhs #where_clause {
            #[inline]
            fn #method_name(&mut self, rhs: #rhs) {
                #to_call(self, rhs);
            }
        }
    })
}
/// `fn(&mut A)` ⇛ `impl Trait for A { fn op(&mut self) }`
fn drop(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output: (),
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unwrap_mut_param(0)?.unary()?.default_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::ops::Drop for #input #where_clause {
            #[inline]
            fn drop(&mut self) {
                #to_call(self);
            }
        }
    })
}

/*
// TODO: works, but needs more work.
fn fn_once(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input: (input, args),
        output,
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.binary()?.any_return();
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::ops::FnOnce<#args>
            for #input #where_clause {
                type Output = #output;
                extern "rust-call" fn call_once(self, args: #args) -> Self::Output {
                    #to_call(self, args)
                }
            }
    })
}
 */

/// `fn(&A, B) -> &T` ⇛ `impl Trait<B> for A { fn op(&self, B) -> &T }`
fn index(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input: (coll, idx),
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unwrap_ref_param(0)?.binary()?.unwrap_ref_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::ops::Index<#idx> for #coll #where_clause {
            type Output = #output;
            #[inline]
             fn index(&self, index: #idx) -> &Self::Output {
                #to_call(self, index)
            }
        }
    })
}
/// `fn(&mut A, B) -> &mut T` ⇛ `impl Trait<B> for A { fn op(&mut self, B) -> &mut T }`
fn index_mut(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input: (coll, idx),
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
        ..
    } = func.unwrap_mut_param(0)?.binary()?.unwrap_mut_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::ops::IndexMut<#idx> for #coll #where_clause {
            #[inline]
             fn index_mut(&mut self, index: #idx) -> &mut Self::Output {
                #to_call(self, index)
            }
        }
    })
}
/// `fn(A) -> T` ⇛ `impl Trait for A { type Assoc = T; fn op(self) -> T }`
fn neg_shaped(func: GenIn, trait_name: &Path, method_name: &Ident) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics #trait_name for #input #where_clause {
            type Output = #output;
            #[inline]
             fn #method_name(self) -> Self::Output {
                #to_call(self)
            }
        }
    })
}
/// `fn(&A) -> &T` ⇛ `impl Trait for A { fn op(&self) -> &T }`
fn deref(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unwrap_ref_param(0)?.unary()?.unwrap_ref_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::ops::Deref for #input #where_clause {
            type Target = #output;
            #[inline]
             fn deref(&self) -> &Self::Target {
                #to_call(self)
            }
        }
    })
}
/// `fn(&mut A) -> &mut T` ⇛ `impl Trait for A { fn op(&mut self) -> &mut T }`
fn deref_mut(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unwrap_mut_param(0)?.unary()?.unwrap_mut_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::ops::DerefMut for #input #where_clause {
            #[inline]
             fn deref_mut(&mut self) -> &mut #output {
                #to_call(self)
            }
        }
    })
}
/// `fn(&A) -> Result<T, U>` ⇛ `impl Trait for T { type Assoc = U; fn op(&self) -> Result<T, U> }`
// This is a special case of TryInto
fn from_str(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output: (output, err),
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.check_ref_param(0)?.unary()?.unwrap_result_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::str::FromStr for #output #where_clause {
            type Err = #err;
            #[inline]
             fn from_str(s: #input) -> ::core::result::Result<Self, Self::Err> {
                #to_call(s)
            }
        }
    })
}
/// `fn(&A) -> T` ⇛ `impl Trait for A { fn op(&self) -> T }`
// This is a special-case of ToOwned
fn to_string(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta: FunctionMeta { derive_span, generics, to_call, extra_attrs, .. },
    } = func.unwrap_ref_param(0)?.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::zoet::__alloc::string::ToString for #input #where_clause {
            #[inline]
             fn to_string(&self) -> #output {
                #to_call(self)
            }
        }
    })
}
