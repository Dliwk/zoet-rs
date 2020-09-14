//! [`module@zoet`]'s proc-macro implementation. See that crate for documentation, and do not use
//! this crate directly.

#![cfg_attr(all(feature = "clippy-insane", debug_assertions), warn(
    //// Turn the "allow" lints listed by `rustc -W help` ["rustc 1.46.0 (04488afe3 2020-08-24)"]
    //// into warn lints:
    absolute_paths_not_starting_with_crate, anonymous_parameters, box_pointers,
    clashing_extern_declarations, deprecated_in_future, elided_lifetimes_in_paths,
    explicit_outlives_requirements, indirect_structural_match, keyword_idents,
    macro_use_extern_crate, meta_variable_misuse, missing_copy_implementations,
    missing_crate_level_docs, missing_debug_implementations, missing_docs,
    missing_doc_code_examples, non_ascii_idents, private_doc_tests, single_use_lifetimes,
    trivial_casts, trivial_numeric_casts, unaligned_references, unreachable_pub, unsafe_code,
     unstable_features, unused_crate_dependencies, unused_extern_crates,
    unused_import_braces, unused_lifetimes, unused_qualifications, unused_results,
    variant_size_differences,
    //// Ditto for clippy lint categories (see https://github.com/rust-lang/rust-clippy):
    clippy::all, clippy::cargo, clippy::nursery, clippy::pedantic, clippy::restriction,
), cfg_attr(feature = "unstable", feature(
    unsafe_block_in_unsafe_fn   //// "unsafe_op_in_unsafe_fn" lint requires this feature
), warn(
    //// more "allow" lints from unstable rust ["rustc 1.48.0-nightly (a1947b3f9 2020-09-10)"]:
    unsafe_op_in_unsafe_fn,
)))]
#![forbid(unsafe_code)]
#![cfg_attr(feature = "unsafe", allow(unsafe_code))]
#![cfg_attr(feature = "unstable", allow(unstable_features), feature())]
#![cfg_attr(feature = "unstable-doc-cfg", feature(doc_cfg))]
#![cfg_attr(feature = "very-unstable", feature())]
#![cfg_attr(debug_assertions, allow(
    //// turn off individual noisy/buggy lints enabled by broader categories above:
    box_pointers,                             // don't care
    clippy::blanket_clippy_restriction_lints, // allow clippy::restriction (note: nightly)
    clippy::implicit_return,                  // not idiomatic Rust
    clippy::integer_arithmetic,               // what's a computer for?
    clippy::missing_const_for_fn,             // not relevant
    clippy::missing_docs_in_private_items,    // don't care
    clippy::redundant_pub_crate,              // a bit broken
    clippy::wildcard_enum_match_arm,          // don't care
    clippy::wildcard_imports,                 // don't care
    elided_lifetimes_in_paths,                // adding <'_> everywhere is ugly
    missing_doc_code_examples,                // don't care
))]
#![forbid(unsafe_code)]

#[allow(unused_extern_crates)] extern crate proc_macro;

macro_rules! diagnostic_error {
    ($SPAN:expr, $($REST:tt)+) => {
        diagnostic!($SPAN, ::proc_macro_error::Level::Error, $($REST)+)
    }
}

mod prelude {
    pub(crate) use proc_macro_error::{
        abort, diagnostic, emit_error, emit_warning, Diagnostic, OptionExt, ResultExt,
    };
    pub(crate) type Result<T, E = Diagnostic> = core::result::Result<T, E>;
}
mod function_args;
mod self_replacer;
mod traits;
mod with_tokens;
mod zoet;

/// The `#[zoet]` macro.
#[proc_macro_error::proc_macro_error]
#[proc_macro_attribute]
pub fn zoet(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream
{
    crate::zoet::zoet(&attr.into(), &item.into()).into()
}
