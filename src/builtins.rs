use proc_macro2::TokenStream;
use syn::__private::quote::quote;
use crate::ident;

const BUILTIN_DATA_TYPES: &[&str] = &[
    "absolute-size",
    "age",
    "angle",
    "basic-shape",
    "calc-size()",
    "counter-name",
    "counter-style-name",
    "custom-ident",
    "dashed-ident",
    "decibel",
    "feature-tag-value",
    "flex",
    "frequency",
    "gender",
    "hex-color",
    "id",
    "ident",
    "image-1D",
    "integer",
    "length",
    "number",
    "named-color",
    "semitones",
    "system-color",
    "outline-line-style",
    "palette-identifier",
    "percentage",
    "relative-size",
    "string",
    "target-name",
    "time",
    "timeline-range-name",
    "transform-function",
    "uri",
    "url-set",
    "url-token",
    "dimension",
    "ident-token",
    "url-modifier",
    "media-query-list",
    "declaration",
    "string-token",
    "attr()",
    "zero",
    "declaration-value",
    "custom-property-name",
    "x",
    "y",
    "element()",
    "any-value",
    "n-dimension",
    "ndash-dimension",
    "ndashdigit-dimension",
    "ndashdigit-ident",
    "signed-integer",
    "signless-integer",
    "hash-token",
    "function-token",
    "dashndashdigit-ident",



    "bool-in-parens",
    "function-parameter",
    "media-in-parens",
    "mix-blend-mode",
    "supports-in-parens",
    "top",
    "right",
    "bottom",
    "left",
    "type()",
    "mf-plain",
    "supports-decl",
    "calc-value"
    // "color()",
];


pub fn get() -> TokenStream {
    let mut items = quote! {
        pub struct ZeroOrMore<T>(pub Vec<T>);
        pub struct OneOrMore<T>(pub Vec<T>);
        pub struct Between<const L: usize, const U: usize, T>(pub Vec<T>);
        pub struct CommaSeparatedRepeat<const L: usize, const U: usize, T>(pub Vec<T>);
    };
    
    for dt in BUILTIN_DATA_TYPES {
        let id = if dt.contains("()") {
            ident(&dt.replace("()", "Fn"))
        } else {
            ident(&format!("{}Def", dt))
        };
        
        
        items.extend(quote! {
            type #id = ();
            
        })
        
    }
    
    
    items
    

}