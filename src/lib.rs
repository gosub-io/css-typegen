pub mod component;
pub mod multiplier;
mod value;

use convert_case::{Case, Casing};
use proc_macro2::Span;
use syn::punctuated::Punctuated;
use syn::{ItemEnum, ItemStruct};

fn new_enum(name: &str) -> ItemEnum {
    ItemEnum {
        attrs: vec![],
        vis: syn::Visibility::Inherited,
        enum_token: Default::default(),
        ident: ident(name),
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
        ident: ident(name),
        generics: Default::default(),
        fields: syn::Fields::Named(syn::FieldsNamed {
            brace_token: Default::default(),
            named: Punctuated::new(),
        }),
        semi_token: Some(Default::default()),
    }
}

fn new_struct_unit(name: &str) -> ItemStruct {
    ItemStruct {
        attrs: vec![],
        vis: syn::Visibility::Inherited,
        struct_token: Default::default(),
        ident: ident(name),
        generics: Default::default(),
        fields: syn::Fields::Unit,
        semi_token: Some(Default::default()),
    }
}

fn transform_id(name: &str) -> String {
    let mut name = name.replace(['+', '(', ')'], "");

    if name.starts_with(char::is_numeric) {
        return format!("_{}", name);
    }

    if name.to_lowercase() == "self" {
        name.remove(1);
    }

    name
}

fn ident(name: &str) -> syn::Ident {
    let mut name = transform_id(name).to_case(Case::Pascal);

    if name.starts_with(char::is_numeric) {
        name = format!("_{}", name);
    }

    syn::Ident::new(&name, Span::call_site())
}

fn ident_snake(name: &str) -> syn::Ident {
    let name = transform_id(name);

    syn::Ident::new(&name.to_case(Case::Snake), Span::call_site())
}
