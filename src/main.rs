use css_typegen::component::generate_component_root;
use gosub_css3::matcher::property_definitions::get_css_definitions;
use indexmap::IndexSet;
use proc_macro2::TokenStream;
use syn::__private::ToTokens;

fn main() {
    let defs = get_css_definitions();
    
    let mut items = IndexSet::new();

    for def in &defs.properties {
        if def.1.syntax.components.is_empty() {
            continue;
        }

        let Some(res) = generate_component_root(&def.1.syntax.components[0], &def.0) else {
            continue;
        };
        
        for item in res.1 {
            items.insert(item);
        }

        items.insert(res.0);
    }

    for def in &defs.syntax {
        if def.1.syntax.components.is_empty() {
            continue;
        }

        let Some(res) = generate_component_root(&def.1.syntax.components[0], &def.0) else {
            continue;
        };

        for item in res.1 {
            items.insert(item);
        }

        items.insert(res.0);
    }

    let mut output = TokenStream::new();
    
    for item in items {
        item.to_tokens(&mut output);
    }

    std::fs::write("out_raw.rs", output.to_string()).unwrap();

    let file: syn::File = syn::parse2(output).unwrap();

    let out = prettyplease::unparse(&file);

    std::fs::write("out.rs", out).unwrap();
}
