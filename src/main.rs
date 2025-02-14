mod component;
mod multiplier;

use gosub_css3::matcher::property_definitions::get_css_definitions;
use proc_macro2::Span;
use syn::ItemEnum;
use syn::punctuated::Punctuated;
use crate::component::generate_component;

fn main() {

    let defs = get_css_definitions();

    let cubic = defs.syntax.get("display-outside").unwrap();


    let mut ty = new_enum("DisplayOutside");
    
    generate_component(&mut ty, &cubic.syntax.components[0])
}

fn new_enum(name: &str) -> ItemEnum {
    ItemEnum {
        attrs: vec![],
        vis: syn::Visibility::Inherited,
        enum_token: Default::default(),
        ident: syn::Ident::new(name, Span::call_site()),
        generics: Default::default(),
        brace_token: Default::default(),
        variants: Punctuated::new()
    }
}



