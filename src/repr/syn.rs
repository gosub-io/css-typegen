use proc_macro2::Ident;
use crate::component::group::StructOrEnum;
use crate::repr::{Combinator, CssItem, CssRepr, CssTree, CssType};
use syn::punctuated::Punctuated;
use syn::{Field, Fields, FieldsUnnamed, Type, TypeTuple};
use syn::__private::ToTokens;
use syn::parse::Parse;

impl CssType {
    pub fn to_type(&self) -> StructOrEnum {
        match self {
            CssType::Enum(variants) => {
                todo!()
            }
            CssType::Struct(fields) => {
                todo!()
            }
        }
    }
}

impl CssTree {
    pub fn to_fields(&self, is_aloao: bool) -> Fields {
        if self.items.is_empty() {
            return Fields::Unit;
        }

        let mut elems = Punctuated::new();

        for item in &self.items {
            if let Some(item) = item.to_type(is_aloao) {
                elems.push(Field {
                    attrs: Vec::new(),
                    vis: syn::Visibility::Inherited,
                    mutability: syn::FieldMutability::None,
                    ident: None,
                    colon_token: None,
                    ty: item,
                });
            }
        }
        
        Fields::Unnamed(FieldsUnnamed {
            paren_token: Default::default(),
            unnamed: elems,
        })
    }
}

impl CssItem {
    pub fn to_type(&self, is_aloao: bool) -> Option<Type> {
        let mut ty = self.repr.to_type(is_aloao)?;

        match ty {
            Type::Paren(paren) => {
                ty = *paren.elem;
            },

            Type::Tuple(tuple) if tuple.elems.len() == 1 => {
                ty = tuple.elems.into_iter().next().unwrap();
            }

            _ => {}
        }
        
        
        Some(match self.combinator {
            Combinator::None => ty,
            Combinator::Optional => {
                apply_multiplier_internal("Option", ty, None, false)
            },
            Combinator::ZeroOrMore => {
                apply_multiplier_internal("ZeroOrMore", ty, None, false)
            },

            Combinator::OneOrMore => {
                apply_multiplier_internal("OneOrMore", ty, None, false)
            },

            Combinator::Between(l, u) => {
                apply_multiplier_internal("Between", ty, Some((l, u)), false)
            },

            Combinator::CommaSeparatedRepeat(l, u) => {
                apply_multiplier_internal("CommaSeparatedRepeat", ty, Some((l, u)), false)
            },
        })
    }
}

impl CssRepr {
    pub fn to_type(&self, is_aloao: bool) -> Option<Type> {
        Some(match self {
            Self::Function(f) => {
                todo!()
                
            },
            Self::Sub(sub) => {
                todo!()
                
            },
            Self::Definition(def) => {
                todo!()
                
            },
            Self::Lit(lit) => {
                todo!()
                
            },
            Self::Group(g) => {
                todo!()
                
            },
            
        })
    }
}

fn id(name: &str) -> syn::Ident {
    syn::Ident::new(&name, proc_macro2::Span::call_site())
}

fn apply_multiplier_internal<T: Parse>(multi: &str, fields: impl ToTokens, size: Option<(usize, usize)>, paren: bool) -> T {
    let multi = Ident::new(multi, proc_macro2::Span::call_site());

    if paren {
        if let Some((lower, upper)) = size {
            syn::parse_quote! {
                (#multi<#lower, #upper, #fields>)
            }
        } else {
            syn::parse_quote! {
                (#multi<#fields>)
            }
        }

    } else {
        if let Some((lower, upper)) = size {
            syn::parse_quote! {
                #multi<#lower, #upper, #fields>
            }
        } else {
            syn::parse_quote! {
                #multi<#fields>
            }
        }
    }
}
