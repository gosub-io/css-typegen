mod component;
mod multiplier;

use gosub_css3::matcher::property_definitions::get_css_definitions;
use proc_macro2::Span;
use syn::{ItemEnum, ItemStruct};
use syn::punctuated::Punctuated;
use crate::component::generate_component_root;

fn main() {
    let defs = get_css_definitions();

    let width = defs.properties.get("width").unwrap();

    generate_component_root(&width.syntax.components[0], "Width");
}

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
