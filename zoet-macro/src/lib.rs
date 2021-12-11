//! [`module@zoet`]'s proc-macro implementation. See that crate for documentation, and do not use
//! this crate directly.

#![cfg_attr(all(feature = "clippy-insane", debug_assertions), warn(
    //// Turn the "allow" lints listed by `rustc -W help` ["rustc 1.56.0 (09c42c458 2021-10-18)"]
    //// into warn lints:
    absolute_paths_not_starting_with_crate, box_pointers, deprecated_in_future,
    elided_lifetimes_in_paths, explicit_outlives_requirements, keyword_idents,
    macro_use_extern_crate, meta_variable_misuse, missing_abi, missing_copy_implementations,
    missing_debug_implementations, missing_docs, non_ascii_idents, noop_method_call,
    pointer_structural_match, rust_2021_incompatible_closure_captures,
    rust_2021_incompatible_or_patterns, rust_2021_prefixes_incompatible_syntax,
    rust_2021_prelude_collisions, single_use_lifetimes, trivial_casts, trivial_numeric_casts,
    unreachable_pub, unsafe_code, unsafe_op_in_unsafe_fn, unstable_features,
    unused_crate_dependencies, unused_extern_crates, unused_import_braces, unused_lifetimes,
    unused_qualifications, unused_results, variant_size_differences,

    //// Ditto for clippy lint categories (see https://github.com/rust-lang/rust-clippy):
    clippy::all, clippy::cargo, clippy::nursery, clippy::pedantic, clippy::restriction,
))]
#![cfg_attr(debug_assertions, allow(
    clippy::blanket_clippy_restriction_lints,
    //// turn off individual noisy/buggy lints enabled by broader categories above:
    box_pointers,                          // obsolete/don't care
    clippy::implicit_return,               // not idiomatic Rust
    clippy::integer_arithmetic,            // what's a computer for?
    clippy::missing_const_for_fn,          // not relevant
    clippy::missing_docs_in_private_items, // don't care
    clippy::redundant_pub_crate,           // a bit broken
    clippy::shadow_reuse,
    clippy::shadow_same,
    clippy::shadow_unrelated,
    clippy::str_to_string,                 // triggered by `proc_macro_error` macros
    clippy::wildcard_enum_match_arm,       // don't care
    clippy::wildcard_imports,              // don't care
    elided_lifetimes_in_paths,             // adding <'_> everywhere is ugly
))]
#![forbid(unsafe_code)]

#[allow(unused_extern_crates)] extern crate proc_macro;

macro_rules! diagnostic_error {
    ($SPAN:expr, $($REST:tt)+) => {
        diagnostic!($SPAN, ::proc_macro_error::Level::Error, $($REST)+)
    }
}

pub(crate) mod prelude {
    pub(crate) use proc_macro_error::{
        abort, diagnostic, emit_error, emit_warning, Diagnostic, OptionExt as _, ResultExt as _,
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
    attr: proc_macro::TokenStream, item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    crate::zoet::zoet(&attr.into(), &item.into()).into()
}
