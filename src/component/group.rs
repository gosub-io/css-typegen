use crate::{new_enum, new_struct};
use convert_case::{Case, Casing};
use gosub_css3::matcher::syntax::{GroupCombinators, SyntaxComponent};
use proc_macro2::{Ident, Span};
use std::fmt::Display;
use std::slice;
use syn::__private::ToTokens;
use syn::punctuated::Punctuated;
use syn::{Field, FieldMutability, ItemEnum, ItemStruct, Path, Type, TypePath};

pub enum StructOrEnum {
    Struct(ItemStruct),
    Enum(ItemEnum),
}

impl Display for StructOrEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StructOrEnum::Struct(s) => write!(f, "{}", s.to_token_stream()),
            StructOrEnum::Enum(e) => write!(f, "{}", e.to_token_stream()),
        }
    }
}

pub fn generate_group(
    components: &[SyntaxComponent],
    combinator: GroupCombinators,
    name: &str,
) -> StructOrEnum {
    match combinator {
        GroupCombinators::Juxtaposition | GroupCombinators::AllAnyOrder => {
            StructOrEnum::Struct(generate_group_struct(components, name))
        }

        GroupCombinators::AtLeastOneAnyOrder | GroupCombinators::ExactlyOne => {
            StructOrEnum::Enum(generate_group_enum(components, name))
        }
    }
}

pub fn generate_group_struct(components: &[SyntaxComponent], name: &str) -> ItemStruct {
    let mut ty = new_struct(name);
    
    let fields = match &mut ty.fields {
        syn::Fields::Named(fields) => fields,
        _ => panic!("Expected named fields"),
    };
    

    for component in components {
        match component {
            SyntaxComponent::GenericKeyword { .. } | SyntaxComponent::Inherit { .. } | SyntaxComponent::Initial { .. } | SyntaxComponent::Unset { .. } => {}
            
            SyntaxComponent::Definition { datatype, .. } => {
                let ty = datatype.to_case(Case::Pascal);

                let ty = ty.trim_end_matches("()");

                let ty = Ident::new(ty, Span::call_site());
                
                let id = datatype.to_case(Case::Snake);
                let id = id.trim_end_matches("()");
                let id = Ident::new(id, Span::call_site());
                
                
                fields.named.push(Field {
                    attrs: Vec::new(),
                    vis: syn::Visibility::Inherited,
                    mutability: FieldMutability::None,
                    ident: Some(id),
                    colon_token: None,
                    ty: Type::Path(TypePath {
                        qself: None,
                        path: ty.into()
                    }),
                })
            }
            
            _ => {}
        }
    }
    
    
    
    ty
}

pub fn generate_group_enum(components: &[SyntaxComponent], name: &str) -> ItemEnum {
    let mut ty = new_enum(name);

    for component in components {
        match component {
            SyntaxComponent::GenericKeyword {
                keyword,
                multipliers,
            } => {
                //TODO: handle multipliers
                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: Ident::new(&keyword.to_case(Case::Pascal), Span::call_site()),
                    fields: syn::Fields::Unit,
                    discriminant: None,
                });
            }
            SyntaxComponent::Inherit { .. } => {
                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: Ident::new("Inherit", Span::call_site()),
                    fields: syn::Fields::Unit,
                    discriminant: None,
                });
            }

            SyntaxComponent::Initial { .. } => {
                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: Ident::new("Initial", Span::call_site()),
                    fields: syn::Fields::Unit,
                    discriminant: None,
                });
            }

            SyntaxComponent::Definition { datatype, .. } => {
                let id = datatype.to_case(Case::Pascal);

                let id = id.trim_end_matches("()");

                let id = Ident::new(id, Span::call_site());

                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: id.clone(),
                    fields: syn::Fields::Unnamed(syn::FieldsUnnamed {
                        paren_token: Default::default(),
                        unnamed: Punctuated::from_iter(vec![syn::Field {
                            attrs: vec![],
                            vis: syn::Visibility::Inherited,
                            mutability: FieldMutability::None,
                            ident: None,
                            colon_token: None,
                            ty: Type::Path(TypePath {
                                qself: None,
                                path: Path::from(id),
                            }),
                        }]),
                    }),
                    discriminant: None,
                });
            }
            SyntaxComponent::Function { name, multipliers, arguments } => {
                let fields = if let Some(arguments) = arguments {
                    generate_group_struct(slice::from_ref(arguments), &name.to_case(Case::Pascal)).fields
                } else {
                    syn::Fields::Unit
                };
                
                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: Ident::new(&name.to_case(Case::Pascal), Span::call_site()),
                    fields,
                    discriminant: None,
                });
            }
            _ => {}
        }
    }

    ty
}
