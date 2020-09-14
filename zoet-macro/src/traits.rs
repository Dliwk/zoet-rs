use crate::{function_args::*, prelude::*, with_tokens::*};
use proc_macro2::TokenStream;
use quote::quote_spanned;
use syn::{parse_quote as pq, Ident, Path, ReturnType, Type};

pub(crate) type GenIn<'a> =
    FunctionArgs<'a, Box<[WithTokens<'a, Type>]>, WithTokens<'a, ReturnType>>;
pub(crate) type GenOut = Result<TokenStream>;
pub(crate) type GenFn = fn(GenIn) -> GenOut;

pub(crate) fn get_trait_fn(key: &str) -> Option<GenFn> {
    let func: GenFn = match key {
        // std::borrow
        "Borrow" => |f| borrow_shaped(f, &pq!(::core::borrow::Borrow), &pq!(borrow)),
        "BorrowMut" => |f| borrow_mut_shaped(f, &pq!(::core::borrow::BorrowMut), &pq!(borrow_mut)),
        "ToOwned" => to_owned,

        // std::clone
        "Clone" => clone,

        // std::cmp
        // "Eq"
        "Ord" => |f| ord_shaped(f, &pq!(::core::cmp::Ord), &pq!(cmp)),
        "PartialEq" => |f| ord_shaped(f, &pq!(::core::cmp::PartialEq), &pq!(eq)),
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
        // "Error" => generate Error::source?

        // std::fmt
        // Probably silly to do Binary/LowerExp/LowerHex etc.
        "Debug" => |f| debug_shaped(f, &pq!(::core::fmt::Debug)),
        "Display" => |f| debug_shaped(f, &pq!(::core::fmt::Display)),
        "Write" => write,

        // std::future
        // "Future" => generate Future::poll?

        // std::hash
        // "BuildHasher"?
        "Hash" => hash,
        // "Hasher" => non-starter; requires two functions.

        // std::io
        // Read/Seek are single-function traits, but BufRead/Write require two. Probably best to
        // avoid std::io completely.

        // std::iterator
        // DoubleEndedIterator::next_back
        // ExactSizeIterator::len
        // Extend::extend // fn has a generic type
        // "FromIterator" => from_iterator, // fn is rather complex
        "IntoIterator" => into_iterator,
        "Iterator" => iterator,
        // Product::product // fn has a generic type
        // Sum::sum // fn has a generic type

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
        "Deref" => deref,
        "DerefMut" => deref_mut,
        "Div" => |f| add_shaped(f, &pq!(::core::ops::Div), &pq!(div)),
        "DivAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::DivAssign), &pq!(div_assign)),
        "Drop" => drop,
        "Index" => index,
        "IndexMut" => index_mut,
        "Mul" => |f| add_shaped(f, &pq!(::core::ops::Mul), &pq!(mul)),
        "MulAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::MulAssign), &pq!(mul_assign)),
        "Neg" => |f| neg_shaped(f, &pq!(::core::ops::Neg), &pq!(neg)),
        "Not" => |f| neg_shaped(f, &pq!(::core::ops::Not), &pq!(not)),
        "Rem" => |f| add_shaped(f, &pq!(::core::ops::Rem), &pq!(rem)),
        "RemAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::RemAssign), &pq!(rem_assign)),
        "Shl" => |f| add_shaped(f, &pq!(::core::ops::Shl), &pq!(shl)),
        "ShlAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::ShlAssign), &pq!(shl_assign)),
        "Shr" => |f| add_shaped(f, &pq!(::core::ops::Shr), &pq!(shr)),
        "ShrAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::ShrAssign), &pq!(shr_assign)),
        "Sub" => |f| add_shaped(f, &pq!(::core::ops::Sub), &pq!(sub)),
        "SubAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::SubAssign), &pq!(sub_assign)),

        // std::str
        "FromStr" => from_str,

        // std::string
        "ToString" => to_string,

        _ => return None,
    };

    Some(func)
}

/// `fn(&A) -> &T` ⇛ `impl Trait<T> for A { fn op(&self) -> &T }`
fn borrow_shaped(func: GenIn, trait_name: &Path, method_name: &Ident) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unwrap_ref_param(0)?.unary()?.unwrap_ref_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics #trait_name <#output> for #input #where_clause {
            #[inline] fn #method_name (&self) -> &#output {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unwrap_mut_param(0)?.unary()?.unwrap_mut_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics #trait_name <#output> for #input #where_clause {
            #[inline] fn #method_name (&mut self) -> &mut #output {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unwrap_ref_param(0)?.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::zoet::traits::ToOwned for #input #where_clause {
            type Owned = #output;
            #[inline] fn to_owned(&self) -> Self::Owned {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unwrap_ref_param(0)?.unary()?.has_return()?; // TODO: create and use a .self_return() instead?
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::clone::Clone for #input #where_clause {
            #[inline] fn clone(&self) -> #output {
                #to_call(self)
            }
        }
    })
}
/// `fn(&A, &A) -> Struct` ⇛ `impl Trait for A { fn op(&self, other: &Self) -> Struct }` (`Struct` = `Ordering`)
fn ord_shaped(func: GenIn, trait_name: &Path, method_name: &Ident) -> Result<TokenStream> {
    let FunctionArgs {
        input: (lhs, rhs),
        output,
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func
        .unwrap_ref_param(0)?
        .unwrap_ref_param(1)?
        .binary()?
        .has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics #trait_name for #lhs #where_clause {
            #[inline] fn #method_name(&self, other: &#rhs) -> #output {
                #to_call(self, other)
            }
        }
    })
}
/// `fn(&A, &B) -> Struct` ⇛ `impl Trait<B> for A { fn op(&self, other: &B) -> Option<Struct> }` (`Struct` = `Ordering`)
fn partial_ord(func: GenIn) -> Result<TokenStream> {
    let filtered = func.unwrap_ref_param(0)?.unwrap_ref_param(1)?.binary()?;
    if let Ok(FunctionArgs {
        input: (lhs, rhs),
        output,
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    }) = filtered.clone().unwrap_option_return()
    {
        // The function returns Option<_>, so is assumed to be compatible with
        // PartialOrd::partial_ord.
        let where_clause = &generics.where_clause;
        Ok(quote_spanned! {
            derive_span =>
                #extra_attrs
            impl #generics ::core::cmp::PartialOrd for #lhs #where_clause {
                #[inline] fn partial_cmp(&self, other: &#rhs) -> ::core::option::Option<#output> {
                    #to_call(self, other)
                }
            }
        })
    } else {
        let FunctionArgs {
            input: (lhs, rhs),
            output,
            meta:
                FunctionMeta {
                    derive_span,
                    generics,
                    to_call,
                    extra_attrs,
                    ..
                },
        } = filtered.has_return()?;
        // The function does *not* return Option<_>, so we assume that its return value needs to be
        // wrapped.
        let where_clause = &generics.where_clause;
        Ok(quote_spanned! {
            derive_span =>
                #extra_attrs
            impl #generics ::core::cmp::PartialOrd for #lhs #where_clause {
                #[inline] fn partial_cmp(&self, other: &#rhs) -> ::core::option::Option<#output> {
                    ::core::option::Option::Some(#to_call(self, other))
                }
            }
        })
    }
}
/// `fn(A) -> T` ⇛ `impl Trait<A> for T { fn op(A) -> T }`
fn from(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unary()?.has_return()?; // TODO: .self_return()?
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::convert::From<#input> for #output #where_clause {
            #[inline] fn from(value: #input) -> Self {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::convert::Into<#output> for #input #where_clause {
            #[inline] fn into(self) -> #output {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unary()?.unwrap_result_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::convert::TryFrom<#input> for #output #where_clause {
            type Error = #err;
            #[inline] fn try_from(value: #input) -> ::core::result::Result<Self, Self::Error> {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unary()?.unwrap_result_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::convert::TryInto<#output> for #input #where_clause {
            type Error = #err;
            #[inline] fn try_into(self) -> ::core::result::Result<#output, Self::Error> {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.nullary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::default::Default for #output #where_clause {
            #[inline] fn default() -> Self {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
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
            #[inline] fn fmt(&self, f: &mut #formatter) -> #output {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
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
            #[inline] fn write_str(&mut self, s: &#str) -> #output {
                #to_call(self, s)
            }
        }
    })
}
/// (Signature is particularly trait-specific.)
fn hash(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input: (obj, _hasher),
        output: (),
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
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
            #[inline] fn hash<H: ::core::hash::Hasher>(&self, state: &mut H) {
                #to_call(self, state)
            }
        }
    })
}

/*
/// (Signature is frightful)
// TODO: this generates bad code. We need to add support for stripping the "impl" from the function
// signature and emitting extra generics magic.
fn from_iterator(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unary()?.has_return()?;

    let where_clause = &generics.where_clause;

    Ok(quote_spanned! {
        derive_span =>
            compile_error!("TODO: how did you even get to this code?");
            #extra_attrs
        impl <T /*from caller */, __ZoetImpl: IntoIterator<Item = T> /* from us */>
        // #generics
            ::core::iter::FromIterator<__ZoetImpl::Item> for #output
        {
            #[inline] fn from_iter<__ZoetT>(
                iter: __ZoetT
            ) -> Self {
                #to_call(iter)
            }
        }
    })
}
*/

/// `fn(A) -> T` ⇛
/// `impl Trait for A { type Assoc1 = Out; type Assoc2 = <Out as Trait>::Assoc2; fn op(self) -> Out }`
fn into_iterator(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output,
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::iter::IntoIterator for #input #where_clause {
            type IntoIter = #output;
            type Item = <Self::IntoIter as ::core::iter::Iterator>::Item;
            #[inline] fn into_iter(self) -> Self::IntoIter {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unwrap_mut_param(0)?.unary()?.unwrap_option_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::iter::Iterator for #input #where_clause {
            type Item = #output;
            #[inline] fn next(&mut self) -> Option<Self::Item> {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.binary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics #trait_name<#rhs> for #lhs #where_clause {
            type Output = #output;
            #[inline] fn #method_name(self, rhs: #rhs) -> Self::Output {
                #to_call(self, rhs)
            }
        }
    })
}

fn add_from_add_assign_shaped(
    func: GenIn,
    trait_name: &Path,
    method_name: &Ident,
) -> Result<TokenStream> {
    let FunctionArgs {
        input: (lhs, rhs),
        output: (),
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unwrap_mut_param(0)?.binary()?.default_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics #trait_name<#rhs> for #lhs #where_clause {
            type Output = #lhs;
            #[inline] fn #method_name(mut self, rhs: #rhs) -> #lhs {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unwrap_mut_param(0)?.binary()?.default_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics #trait_name<#rhs> for #lhs #where_clause {
            #[inline] fn #method_name(&mut self, rhs: #rhs) {
                #to_call(self, rhs)
            }
        }
    })
}
/// `fn(&mut A)` ⇛ `impl Trait for A { fn op(&mut self) }`
fn drop(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input,
        output: (),
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unwrap_mut_param(0)?.unary()?.default_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::ops::Drop for #input #where_clause {
            #[inline] fn drop(&mut self) {
                #to_call(self)
            }
        }
    })
}
/// `fn(&A, B) -> &T` ⇛ `impl Trait<B> for A { fn op(&self, B) -> &T }`
fn index(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input: (coll, idx),
        output,
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unwrap_ref_param(0)?.binary()?.unwrap_ref_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::ops::Index<#idx> for #coll #where_clause {
            type Output = #output;
            #[inline] fn index(&self, index: #idx) -> &Self::Output {
                #to_call(self, index)
            }
        }
    })
}
/// `fn(&mut A, B) -> &mut T` ⇛ `impl Trait<B> for A { fn op(&mut self, B) -> &mut T }`
fn index_mut(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input: (coll, idx),
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
        ..
    } = func.unwrap_mut_param(0)?.binary()?.unwrap_mut_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::ops::IndexMut<#idx> for #coll #where_clause {
            #[inline] fn index_mut(&mut self, index: #idx) -> &mut Self::Output {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics #trait_name for #input #where_clause {
            type Output = #output;
            #[inline] fn #method_name(self) -> Self::Output {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unwrap_ref_param(0)?.unary()?.unwrap_ref_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::ops::Deref for #input #where_clause {
            type Target = #output;
            #[inline] fn deref(&self) -> &Self::Target {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unwrap_mut_param(0)?.unary()?.unwrap_mut_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::ops::DerefMut for #input #where_clause {
            #[inline] fn deref_mut(&mut self) -> &mut #output {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.check_ref_param(0)?.unary()?.unwrap_result_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::core::str::FromStr for #output #where_clause {
            type Err = #err;
            #[inline] fn from_str(s: #input) -> ::core::result::Result<Self, Self::Err> {
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
        meta:
            FunctionMeta {
                derive_span,
                generics,
                to_call,
                extra_attrs,
                ..
            },
    } = func.unwrap_ref_param(0)?.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        derive_span =>
            #extra_attrs
        impl #generics ::zoet::traits::ToString for #input #where_clause {
            #[inline] fn to_string(&self) -> #output {
                #to_call(self)
            }
        }
    })
}
