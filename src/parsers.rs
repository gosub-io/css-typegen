use convert_case::{Case, Casing};
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use crate::repr::{CssRepr, CssTree, CssType, CssTypeRepr};

pub fn gen_parser(ty: &CssType) -> TokenStream {
    let id = Ident::new(&ty.id, Span::call_site());
    
    let parse = gen_parse_repr(&ty);
    
    quote! {
        impl Parser for #id {
            fn parse<'a, T: TokenStream<'a>>(input: T) -> IResult<Self, T, ParseError> {
                #parse
            }
        }
    }
}


fn gen_parse_repr(ty: &CssType) -> TokenStream {
    match &ty.repr {
        CssTypeRepr::Enum(variants) => gen_parse_enum(variants, ty.is_aloao, &ty.id),
        CssTypeRepr::Struct(fields) => gen_parse_struct(fields, ty.is_aloao),
    }
}

fn gen_parse_enum(variants: &[(String, CssTree)], is_aloao: bool, name: &str) -> TokenStream {
    let mut parsers = TokenStream::new();
    
    let mut parse = TokenStream::new();
    let name = Ident::new(name, Span::call_site());
    
    for (name, fields) in variants {
        let name_id = Ident::new(name, Span::call_site());
        
        let fn_name = Ident::new(&format!("parse_{}", name.to_case(Case::Snake)), Span::call_site());
        
        let (parse_tree, build_tree) = gen_parse_tree(fields, &mut 0, is_aloao | fields.is_aloao);
        
        parsers.extend(quote! {
            fn #fn_name<'b, T: TokenStream<'b>>(input: T) -> IResult<#name_id, T, ParseError> {
                #parse_tree
                
                Ok((Self::#name_id(#build_tree), input))
            }
        });
        
        parse.extend(quote! {
            #fn_name,
        })
    }


    quote! {
        #parsers
        
        nom::branch::alt([
            #parse
        ]).parse(input).map_err(|e| ParseError { message: e.to_string() })
    }
}

fn gen_parse_struct(fields: &CssTree, is_aloao: bool) -> TokenStream {
    let (parse, build) = gen_parse_tree(fields, &mut 0, is_aloao | fields.is_aloao);
    
    quote! {
        #parse
        
        Ok(Self(#build))
    }
    
}

fn gen_parse_tree(fields: &CssTree, n: &mut usize, is_aloao: bool) -> (TokenStream, TokenStream) {
    let mut parse = TokenStream::new();
    let mut build = TokenStream::new();
    
    for item in &fields.items {
        let name = Ident::new(&format!("item_{}", n), Span::call_site());
        
        match &item.repr {
            CssRepr::Lit(lit) => {
                parse.extend(quote! {
                    let input = parse_lit(input, #lit)?;
                });
                
                continue;
            }
            
            CssRepr::Keyword(kw, _) if is_aloao => {
                parse.extend(quote! {
                    let input = parse_keyword(input, #kw)?;
                });
                
                continue;
            }
            
            _ => {}
        }
        
        parse.extend(quote! {
            let (#name, input) = Parser::parse(input)?;
        });
        
        build.extend(quote! {
            #name,
        });
        
        *n += 1;
    }
    
    
    (parse, build)
    
}