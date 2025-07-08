use crate::component::group::StructOrEnum;
use crate::repr::{Multiplier, CssItem, CssRepr, CssTree, CssType, CssTypeRepr};
use proc_macro2::Ident;
use syn::__private::ToTokens;
use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::{Field, Fields, FieldsUnnamed, Type};

impl CssType {
    pub fn to_type(&self) -> StructOrEnum {
        match &self.repr {
            CssTypeRepr::Enum(variants) => {
                let mut enum_variants = Punctuated::new();

                for (name, tree) in variants {
                    let fields = tree.to_fields(self.is_aloao);
                    enum_variants.push(syn::Variant {
                        attrs: Vec::new(),
                        ident: Ident::new(name, proc_macro2::Span::call_site()),
                        fields,
                        discriminant: None,
                    });
                }

                StructOrEnum::Enum(syn::ItemEnum {
                    attrs: Vec::new(),
                    vis: syn::Visibility::Inherited,
                    enum_token: Default::default(),
                    ident: Ident::new(&self.id, proc_macro2::Span::call_site()),
                    generics: Default::default(),
                    brace_token: Default::default(),
                    variants: enum_variants,
                })
            }
            CssTypeRepr::Struct(fields) => {
                let fields = fields.to_fields(self.is_aloao);
                StructOrEnum::Struct(syn::ItemStruct {
                    attrs: Vec::new(),
                    vis: syn::Visibility::Inherited,
                    struct_token: Default::default(),
                    ident: Ident::new(&self.id, proc_macro2::Span::call_site()),
                    generics: Default::default(),
                    fields,
                    semi_token: Some(Default::default()),
                })
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
    
    pub fn to_type(&self, is_aloao: bool) -> Option<Type> {
        if self.items.is_empty() {
            return None;
        }

        let mut elems = Punctuated::new();

        for item in &self.items {
            if let Some(ty) = item.to_type(is_aloao) {
                elems.push(ty);
            }
        }

        if elems.len() == 1 {
            Some(elems.into_iter().next().unwrap())
        } else {
            Some(Type::Tuple(syn::TypeTuple {
                paren_token: Default::default(),
                elems,
            }))
        }
    }
}

impl CssItem {
    pub fn to_type(&self, is_aloao: bool) -> Option<Type> {
        let mut ty = self.repr.to_type(is_aloao)?;
        
        match ty {
            Type::Paren(paren) => {
                ty = *paren.elem;
            }

            Type::Tuple(tuple) if tuple.elems.len() == 1 => {
                ty = tuple.elems.into_iter().next().unwrap();
            }

            _ => {}
        }

        Some(match self.multiplier {
            Multiplier::None => ty,
            Multiplier::Optional => apply_multiplier_internal("Option", ty, None, false),
            Multiplier::ZeroOrMore => apply_multiplier_internal("ZeroOrMore", ty, None, false),

            Multiplier::OneOrMore => apply_multiplier_internal("OneOrMore", ty, None, false),

            Multiplier::Between(l, u) => {
                apply_multiplier_internal("Between", ty, Some((l, u)), false)
            }

            Multiplier::CommaSeparatedRepeat(l, u) => {
                apply_multiplier_internal("CommaSeparatedRepeat", ty, Some((l, u)), false)
            }
        })
    }
}

impl CssRepr {
    pub fn to_type(&self, is_aloao: bool) -> Option<Type> {
        Some(match self {
            Self::Function(_, f) => {
                f.to_type(is_aloao)?
            }
            Self::Sub(sub) => Type::Path(syn::TypePath {
                qself: None,
                path: Ident::new(sub, proc_macro2::Span::call_site()).into(),
            }),
            Self::Definition(def) => Type::Path(syn::TypePath {
                qself: None,
                path: Ident::new(&format!("{def}Def"), proc_macro2::Span::call_site()).into(),
            }),
            Self::Lit(_) => return None,
            Self::Group(is_aloao, g) => {
                g.to_type(*is_aloao)?
            }
            Self::Keyword(_, kw_id) => {
                if is_aloao {
                    Type::Path(syn::TypePath {
                        qself: None,
                        path: Ident::new(kw_id, proc_macro2::Span::call_site()).into(),
                    })
                } else {
                    return None
                }
            }
            Self::Tuple(tree) => {
                Type::Tuple(syn::TypeTuple {
                    paren_token: Default::default(),
                    elems: Punctuated::from_iter(tree.items.iter().filter_map(|f| f.to_type(is_aloao))),
                })
            }
        })
    }
}

fn id(name: &str) -> syn::Ident {
    syn::Ident::new(&name, proc_macro2::Span::call_site())
}

fn apply_multiplier_internal<T: Parse>(
    multi: &str,
    fields: impl ToTokens,
    size: Option<(usize, usize)>,
    paren: bool,
) -> T {
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
