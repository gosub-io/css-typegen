#![allow(unused)]
pub mod component;
pub mod multiplier;
mod value;
pub mod builtins;
pub mod renamer;
pub mod exporter;
mod repr;

#[cfg(test)]
mod tests;

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
        fields: syn::Fields::Unnamed(syn::FieldsUnnamed {
            paren_token: Default::default(),
            unnamed: Default::default(),
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

pub fn transform_id(name: &str) -> String {
    if name == "-infinity" {
        return "NegInfinity".to_owned();
    }

    let mut name = name.replace(['+', '(', ')', '<', '>'], "");

    if name.starts_with(char::is_numeric) {
        return format!("_{}", name);
    }

    if name.to_lowercase() == "self" {
        name.remove(1);
    }

    name
}

pub fn ident(name: &str) -> syn::Ident {
    let name = ident_str(name);

    syn::Ident::new(&name, Span::call_site())
}


pub fn ident_str(name: &str) -> String {
    let mut name = transform_id(name).to_case(Case::Pascal);

    if name.starts_with(char::is_numeric) {
        name = format!("_{}", name);
    }

    name
}

pub fn ident_snake(name: &str) -> syn::Ident {
    let name = transform_id(name);

    syn::Ident::new(&name.to_case(Case::Snake), Span::call_site())
}


#[derive(Debug, Clone, Copy)]
pub struct Name<'a> {
    pub name: &'a str,
    pub find_better_name: bool,
}

impl<'a> Name<'a> {
    pub fn better(&self) -> Self {
        Self {
            name: self.name,
            find_better_name: true,
        }
    }
    
    pub fn new(name: &'a str) -> Self {
        Self {
            name,
            find_better_name: true,
        }
    }
}

impl<'a> From<&'a str> for Name<'a> {
    fn from(name: &'a str) -> Self {
        Self {
            name,
            find_better_name: false,
        }
    }
}
