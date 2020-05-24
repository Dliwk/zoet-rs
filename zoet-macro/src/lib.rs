//! [`zoet`]'s proc-macro implementation. See that crate for documentation, and do not use this
//! crate directly.

#![cfg_attr(feature = "clippy-insane", warn(
    //// Turn the "allow" lints listed by `rustc -W help` (as of 2020-04-16) into warn lints:
    absolute_paths_not_starting_with_crate, anonymous_parameters, box_pointers,
    deprecated_in_future, elided_lifetimes_in_paths, explicit_outlives_requirements,
    indirect_structural_match, keyword_idents, macro_use_extern_crate, meta_variable_misuse,
    missing_copy_implementations, missing_crate_level_docs, missing_debug_implementations,
    missing_doc_code_examples, missing_docs, non_ascii_idents, private_doc_tests,
    single_use_lifetimes, trivial_casts, trivial_numeric_casts, unreachable_pub, unsafe_code,
    unstable_features, unused_extern_crates, unused_import_braces, unused_lifetimes,
    unused_qualifications, unused_results, variant_size_differences,
    //// Ditto for clippy lint categories (see https://github.com/rust-lang/rust-clippy):
    clippy::all, clippy::cargo, clippy::nursery, clippy::pedantic, clippy::restriction,
), allow(
    //// turn off individual noisy/buggy lints enabled by broader categories above:
    box_pointers,               // yeah, we allocate: get over it
    clippy::implicit_return,    // not idiomatic Rust
    elided_lifetimes_in_paths,  // it may be "deprecated", but adding <'_> everywhere is ugly
    unreachable_pub,            // putting pub(crate) everywhere isn't helpful
    clippy::missing_inline_in_public_items,
    clippy::wildcard_enum_match_arm,
    clippy::missing_docs_in_private_items,
    clippy::wildcard_imports,
    clippy::missing_const_for_fn,
    clippy::integer_arithmetic,
))]
#![forbid(unsafe_code)]

#[allow(unused_extern_crates)] extern crate proc_macro;

mod error;
mod function_args;
mod self_replacer;
mod traits;
mod with_tokens;
mod zoet;

use quote::ToTokens;

/// The `#[zoet]` macro.
#[proc_macro_attribute]
pub fn zoet(
    attr: proc_macro::TokenStream, item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    match crate::zoet::zoet(&attr.into(), item.into()) {
        Ok(ts) => ts.into(),
        Err(err) => err.into_token_stream().into(),
    }
}
