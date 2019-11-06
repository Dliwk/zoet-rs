// -- start of boilerplate that's generally pasted into the top of new projects -- //
#![cfg_attr(feature="clippy-insane", warn(
    // Turn on the "allow" lints currently listed by `rustc -W help` (as of 2019-11-06) into warn
    // lints, unless they're not useful:
    absolute_paths_not_starting_with_crate, anonymous_parameters,
    // box_pointers, // obsolete
    deprecated_in_future,
    // elided_lifetimes_in_paths, // suggests adding dubious <'_> noise everywhere
    explicit_outlives_requirements, indirect_structural_match, keyword_idents,
    macro_use_extern_crate, meta_variable_misuse,
    // missing_copy_implementations, // too noisy; enable and inspect before release
    // missing_debug_implementations, // too noisy; enable and inspect before release
    // missing_docs, // too noisy; enable and inspect before release
    // missing_doc_code_examples, // too noisy; enable and inspect before release
    non_ascii_idents,
    // private_doc_tests, // broken; still complains if "private" item is pub-used
    // single_use_lifetimes, // gets confused too easily by macros
    trivial_casts, trivial_numeric_casts,
    // unreachable_pub, // too noisy; enable and inspect before release
    // unsafe_code,
    // unstable_features, // silly; explicit use of #![feature] already indicates opt-in
    unused_extern_crates, unused_import_braces, unused_labels, unused_lifetimes,
    unused_qualifications, unused_results, variant_size_differences,
    // Ditto for clippy lint categories (see https://github.com/rust-lang/rust-clippy):
    clippy::all, clippy::pedantic, clippy::nursery,
    // clippy::cargo,
    clippy::restriction
))]
#![allow(
    // turn off individual noisy/buggy clippy lints:
    // // clippy::doc_markdown,
    // clippy::use_self,             // gets easily confused by macros
    // // clippy::cast_possible_truncation,
    // clippy::missing_const_for_fn,
    // // clippy::similar_names,
    // // clippy::pub_enum_variant_names,
    // // from clippy::restriction:
    clippy::implicit_return,    // bad style
    clippy::integer_arithmetic, clippy::integer_division, // uh-huh
    clippy::missing_docs_in_private_items,  // too noisy; enable and inspect before release
    clippy::missing_inline_in_public_items, // just moans about all public items
    clippy::multiple_inherent_impl,         // breaks with e.g. derive macros
    // clippy::shadow_reuse,                   // e.g. `let foo = bar(foo)`
    // clippy::shadow_same,                    // e.g. `let foo = &foo`
    // clippy::mem_forget,                     // triggered by no_panic macro
)]
// -- end of boilerplate that's generally pasted into the top of new projects -- //

extern crate proc_macro;

pub(crate) mod error;
pub(crate) mod function_args;
pub(crate) mod self_replacer;
pub(crate) mod traits;
pub(crate) mod with_tokens;
pub(crate) mod zoet;

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
