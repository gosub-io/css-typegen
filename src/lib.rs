pub mod component;
pub mod multiplier;

use proc_macro2::Span;
use syn::{ItemEnum, ItemStruct};
use syn::punctuated::Punctuated;


fn new_enum(name: &str) -> ItemEnum {
    ItemEnum {
        attrs: vec![],
        vis: syn::Visibility::Inherited,
        enum_token: Default::default(),
        ident: syn::Ident::new(name, Span::call_site()),
        generics: Default::default(),
        brace_token: Default::default(),
        variants: Punctuated::new(),
    }
}

fn new_struct(name: &str) -> ItemStruct {
    ItemStruct {
        attrs: vec![],
        vis: syn::Visibility::Inherited,
        struct_token: Default::default(),
        ident: syn::Ident::new(name, Span::call_site()),
        generics: Default::default(),
        fields: syn::Fields::Named(syn::FieldsNamed {
            brace_token: Default::default(),
            named: Punctuated::new(),
        }),
        semi_token: Some(Default::default()),
    }
}