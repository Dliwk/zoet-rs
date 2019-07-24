use crate::preamble::*;
use proc_macro2::TokenStream;
use quote::quote_spanned;
use std::borrow::Cow;
use syn::{parse_quote as pq, Ident, Path, ReturnType, Type};

pub type GenIn<'a> = FunctionArgs<'a, Cow<'a, [WithTokens<'a, Type>]>, WithTokens<'a, ReturnType>>;
pub type GenOut = Result<TokenStream>;
pub type GenFn = fn(GenIn) -> GenOut;

pub static TRAIT_FNS: phf::Map<&str, GenFn> = phf::phf_map! {
    // std::borrow
    "Borrow" => |f| borrow_shaped(f, &pq!(::core::borrow::Borrow), &pq!(borrow)),
    "BorrowMut" => |f| borrow_mut_shaped(f, &pq!(::core::borrow::BorrowMut), &pq!(borrow_mut)),
    "ToOwned" => to_owned,

    // std::clone
    "Clone" => clone,

    // std::cmp
    "Ord" => |f| ord_shaped(f, &pq!(::core::cmp::Ord), &pq!(cmp)),
    "PartialEq" => |f| ord_shaped(f, &pq!(::core::cmp::PartialEq), &pq!(eq)),
    "PartialOrd" =>|f| ord_shaped(f, &pq!(::core::cmp::PartialOrd), &pq!(partial_cmp)),
    
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
    // "Hash" => |_| todo!(), // fn has a generic type
    // "Hasher" => non-starter; requires two functions.

    // std::io
    // Read/Seek are single-function traits, but BufRead/Write require two. Probably best to avoid
    // std::io completely.

    // std::iterator
    // DoubleEndedIterator::next_back
    // ExactSizeIterator::len
    // Extend::extend // fn has a generic type
    //"FromIterator" => from_iterator, // fn also has a generic type
    "IntoIterator" => into_iterator,
    "Iterator" => iterator,
    // Product::product
    // Sum::sum

    // std::ops
    "Add" => |f| add_shaped(f, &pq!(::core::ops::Add), &pq!(add)),
    "AddAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::AddAssign), &pq!(add_assign)),
    "BitAnd" => |f| add_shaped(f, &pq!(::core::ops::BitAnd), &pq!(bitand)),
    "BitAndAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::BitAndAssign), &pq!(bitand_assign)),
    "BitOr" => |f| add_shaped(f, &pq!(::core::ops::BitOr), &pq!(bitor)),
    "BitOrAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::BitOrAssign), &pq!(bitor_assign)),
    "BitXor" => |f| add_shaped(f, &pq!(::core::ops::BitXor), &pq!(bitxor)),
    "BitXorAssign" => |f| add_assign_shaped(f, &pq!(::core::ops::BitXorAssign), &pq!(bitxor_assign)),
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

};

/// `fn(&A) -> &T` ⇛ `impl Trait<T> for A { fn op(&self) -> &T }`

fn borrow_shaped(func: GenIn, trait_name: &Path, method_name: &Ident) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.ref_param(0)?.unary()?.ref_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics #trait_name <#output> for #input #where_clause {
            fn #method_name (&self) -> &#output {
                #to_call(self)
            }
        }

    })
}

/// `fn(&mut A) -> &T` ⇛ `impl Trait<T> for A { fn op(&mut self) -> &mut T }`

fn borrow_mut_shaped(func: GenIn, trait_name: &Path, method_name: &Ident) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.mut_param(0)?.unary()?.mut_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics #trait_name <#output> for #input #where_clause {
            fn #method_name (&mut self) -> &mut #output {
                #to_call(self)
            }
        }

    })
}

/// `fn(&A) -> T` ⇛ `impl Trait for A { type Assoc = T; fn op(&self) -> T }`

fn to_owned(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.ref_param(0)?.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::std::borrow::ToOwned for #input #where_clause {
            type Owned = #output;
            fn to_owned(&self) -> #output {
                #to_call(self)
            }
        }

    })
}

/// `fn(&A) -> T` ⇛ `impl Trait for A { fn op(&self) -> #out }` (`A` = `T`)

fn clone(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.ref_param(0)?.unary()?.has_return()?; // TODO: create and use a .self_return() instead?
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::std::clone::Clone for #input #where_clause {
            fn clone(&self) -> #output {
                #to_call(self)
            }
        }

    })
}

fn ord_shaped(func: GenIn, trait_name: &Path, method_name: &Ident) -> Result<TokenStream> {
    let FunctionArgs {
        input: (ref lhs, ref rhs),
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.ref_param(0)?.ref_param(1)?.binary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics #trait_name for #lhs #where_clause {
            fn #method_name(&self, other: &#rhs) -> #output {
                #to_call(self, other)
            }
        }

    })
}

/// `fn(A) -> T` ⇛ `impl Trait<A> for T { fn op(A) -> T }`

fn from(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.unary()?.has_return()?; // TODO: .self_return()?
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::core::convert::From<#input> for #output #where_clause {
            fn from(value: #input) -> #output {
                #to_call(value)
            }
        }

    })
}

/// `fn(A) -> T` ⇛ `impl Trait<T> for A { fn op(self) -> T }`

fn into(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::core::convert::Into<#output> for #input #where_clause {
            fn into(self) -> #output {
                #to_call(value)
            }
        }

    })
}

/// `fn(A) -> Result<T, U>` ⇛
/// `impl Trait<A> for T { type Assoc = U; fn op(A) -> Result<T, U> }`

fn try_from(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        output: (ref output, ref err),
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.unary()?.result_return()?;
    let where_clause = &generics.where_clause;
    let error = pq!(Error);
    let err = err.as_ref().unwrap_or_else(|| &error);
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::core::convert::TryFrom<#input> for #output #where_clause {
            type Error = #err;
            fn try_from(value: #input) -> ::core::result::Result<#output, #err> {
                #to_call(value)
            }
        }

    })
}

/// `fn(A) -> Result<T, U>` ⇛ `impl Trait<T> for A { type Assoc = U; fn op(self) -> Result<T, U> }`

fn try_into(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        output: (ref output, ref err),
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.unary()?.result_return()?;
    let where_clause = &generics.where_clause;
    let error = pq!(Error);
    let err = err.as_ref().unwrap_or_else(|| &error);
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::core::convert::TryInto<#output> for #input #where_clause {
            type Error = #err;
            fn try_into(self) -> ::core::result::Result<#output, #err> {
                #to_call(self)
            }
        }

    })
}

/// `fn() => T` ⇛ `impl Trait for T { fn op() -> T }`

fn default(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input: (),
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.nullary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::core::default::Default for #output #where_clause {
            fn default() -> #output {
                #to_call()
            }
        }

    })
}

/// `fn(&A, &mut B) -> T` ⇛ `impl Trait for A { fn op(&self, &mut B) -> T }`

fn debug_shaped(func: GenIn, trait_name: &Path) -> Result<TokenStream> {
    let FunctionArgs {
        input: (ref obj, ref formatter),
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.ref_param(0)?.mut_param(1)?.binary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics #trait_name for #obj #where_clause {
            fn fmt(&self, f: &mut #formatter) -> #output {
                #to_call(self, f)
            }
        }

    })
}

/// `fn(&mut A, &B) -> T` ⇛ `impl Trait for A { fn op(&mut self, &B) -> T }`

fn write(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input: (ref obj, ref str),
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.mut_param(0)?.ref_param(1)?.binary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::core::fmt::Write for #obj #where_clause {
            fn write_str(&mut self, s: &#str) -> #output {
                #to_call(self, s)
            }
        }

    })
}

/// `fn(A) -> T` ⇛
/// `impl Trait for A { type Assoc1 = Out; type Assoc2 = <Out as Trait>::Assoc2; fn op(self) -> Out }`

fn into_iterator(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::core::iter::IntoIterator for #input #where_clause {
            type IntoIter = #output;
            type Item = <#output as ::core::iter::Iterator>::Item;
            fn into_iter(self) -> #output {
                #to_call(self)
            }
        }

    })
}

/// `fn(&mut A) -> Option<T>` ⇛
/// `impl Trait for A { type Assoc1 = Out; fn op(&mut In) -> Option<Out> }`

fn iterator(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.mut_param(0)?.unary()?.option_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::core::iter::Iterator for #input #where_clause {
            type Item = #output;
            fn next(&mut self) -> Option<#output> {
                 #to_call(self)
             }
        }

    })
}

/// Switches implementation based on function signature:
/// * `fn(&mut A, B)` ⇛ `impl Trait<B> for A { fn op(&mut self, B) }`
/// * `fn(A, B) -> T` ⇛ implementation which doesn't just forward the call

fn add_shaped(func: GenIn, trait_name: &Path, method_name: &Ident) -> Result<TokenStream> {
    let filter_add_assign = || func.clone().mut_param(0)?.binary()?.default_return();

    if let Ok(function_args) = filter_add_assign() {
        // This actually fits add_assign, so we generate a mutate-and-return implementation instead.
        let FunctionArgs {
            input: (ref lhs, ref rhs),
            output: (),
            meta: FunctionMeta { item_span, generics, to_call, .. },
        } = function_args;
        let where_clause = &generics.where_clause;
        return Ok(quote_spanned! {
            *item_span =>

            impl #generics #trait_name<#rhs> for #lhs #where_clause {
                type Output = #lhs;
                fn #method_name(mut self, rhs: #rhs) -> #lhs {
                    #to_call(&mut self, rhs) ; self
                }
            }

        });
    }

    let FunctionArgs {
        input: (ref lhs, ref rhs),
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.binary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics #trait_name<#rhs> for #lhs #where_clause {
            type Output = #output;
            fn #method_name(self, rhs: #rhs) -> #output {
                #to_call(self, rhs)
            }
        }

    })
}

/// `fn(&mut A, B)` ⇛ `impl Trait<B> for A { fn op(&mut self, B) }`

fn add_assign_shaped(func: GenIn, trait_name: &Path, method_name: &Ident) -> Result<TokenStream> {
    let FunctionArgs {
        input: (ref lhs, ref rhs),
        output: (),
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.mut_param(0)?.binary()?.default_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics #trait_name<#rhs> for #lhs #where_clause {
            fn #method_name(&mut self, rhs: #rhs) {
                #to_call(self, rhs)
            }
        }

    })
}

/// `fn(&mut A)` ⇛ `impl Trait for A { fn op(&mut self) }`

fn drop(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        output: (),
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.mut_param(0)?.unary()?.default_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::core::ops::Drop for #input #where_clause {
            fn drop(&mut self) {
                #to_call(self)
            }
        }

    })
}

/// `fn(&A, B) -> &T` ⇛ `impl Trait<B> for A { fn op(&self, B) -> &T }`

fn index(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input: (ref coll, ref idx),
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.ref_param(0)?.binary()?.ref_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::core::ops::Index<#idx> for #coll #where_clause {
            type Output = #output;
            fn index(&self, index: #idx) -> &#output {
                #to_call(self, index)
            }
        }

    })
}

/// `fn(&mut A, B) -> &mut T` ⇛ `impl Trait<B> for A { fn op(&mut self, B) -> &mut T }`

fn index_mut(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        input: (ref coll, ref idx),
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.mut_param(0)?.binary()?.mut_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::core::ops::IndexMut<#idx> for #coll #where_clause {
            fn index_mut(&mut self, index: #idx) -> &mut #output {
                #to_call(self, index)
            }
        }

    })
}

/// `fn(A) -> T` ⇛ `impl Trait for A { type Assoc = T; fn op(self) -> T }`

fn neg_shaped(func: GenIn, trait_name: &Path, method_name: &Ident) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics #trait_name for #input #where_clause {
            type Output = #output;
            fn #method_name(self) -> #output {
                #to_call(self)
            }
        }

    })
}

/// `fn(&A) -> &T` ⇛ `impl Trait for A { fn op(&self) -> &T }`

fn deref(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.ref_param(0)?.unary()?.ref_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::core::ops::Deref for #input #where_clause {
            type Target = #output;
            fn deref(&self) -> &#output {
                #to_call(self)
            }
        }

    })
}

/// `fn(&mut A) -> &mut T` ⇛ `impl Trait for A { fn op(&mut self) -> &mut T }`

fn deref_mut(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.mut_param(0)?.unary()?.mut_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::core::ops::DerefMut for #input #where_clause {
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
        ref input,
        output: (ref output, ref err),
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.ref_param(0)?.unary()?.result_return()?;
    let error = pq!(Error);
    let err = err.as_ref().unwrap_or_else(|| &error);
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::core::str::FromStr for #output #where_clause {
            type Err = #err;
            fn from_str(s: &#input) -> ::core::result::Result<#output, #err> {
                #to_call(s)
            }
        }

    })
}

/// `fn(&A) -> T` ⇛ `impl Trait for A { fn op(&self) -> T }`
// This is a special-case of ToOwned

fn to_string(func: GenIn) -> Result<TokenStream> {
    let FunctionArgs {
        ref input,
        ref output,
        meta: FunctionMeta { item_span, generics, to_call, .. },
    } = func.ref_param(0)?.unary()?.has_return()?;
    let where_clause = &generics.where_clause;
    Ok(quote_spanned! {
        *item_span =>

        impl #generics ::std::string::ToString for #input #where_clause {
            fn to_string(&self) -> #output {
                #to_call(self)
            }
        }

    })
}
