use crate::component::generate_component_root;
use crate::value::value_to_ident;
use crate::{ident, ident_snake, new_enum, new_struct};
use convert_case::{Case, Casing};
use gosub_css3::matcher::syntax::{GroupCombinators, SyntaxComponent};
use proc_macro2::{Ident, Span};
use std::fmt::Display;
use std::slice;
use syn::__private::ToTokens;
use syn::punctuated::Punctuated;
use syn::{
    Field, FieldMutability, Fields, FieldsUnnamed, ItemEnum, ItemStruct, Path, Type, TypePath,
    Visibility,
};

pub enum StructOrEnum {
    Struct(ItemStruct),
    Enum(ItemEnum),
}

impl ToTokens for StructOrEnum {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            StructOrEnum::Struct(s) => s.to_tokens(tokens),
            StructOrEnum::Enum(e) => e.to_tokens(tokens),
        }
    }
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
) -> (StructOrEnum, Vec<StructOrEnum>) {
    match combinator {
        GroupCombinators::Juxtaposition | GroupCombinators::AllAnyOrder => {
            let res = generate_group_struct(components, name);
            (StructOrEnum::Struct(res.0), res.1)
        }

        GroupCombinators::AtLeastOneAnyOrder | GroupCombinators::ExactlyOne => {
            let res = generate_group_enum(components, name);
            (StructOrEnum::Enum(res.0), res.1)
        }
    }
}

pub fn generate_group_struct(
    components: &[SyntaxComponent],
    name: &str,
) -> (ItemStruct, Vec<StructOrEnum>) {
    let mut ty = new_struct(name);
    let mut additional = Vec::new();

    let fields = match &mut ty.fields {
        syn::Fields::Named(fields) => fields,
        _ => panic!("Expected named fields"),
    };

    for component in components {
        match component {
            SyntaxComponent::GenericKeyword { .. }
            | SyntaxComponent::Inherit { .. }
            | SyntaxComponent::Initial { .. }
            | SyntaxComponent::Unset { .. } => {}

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
                        path: ty.into(),
                    }),
                })
            }

            SyntaxComponent::Function {
                name,
                multipliers: _,
                arguments,
            } => {
                if let Some(arguments) = arguments {
                    let Some(res) = generate_component_root(arguments, name) else {
                        continue;
                    };
                    additional.extend(res.1);

                    match res.0 {
                        StructOrEnum::Struct(s) => match s.fields {
                            Fields::Unit => {}
                            Fields::Named(fn_fields) => {
                                fields.named.extend(fn_fields.named);
                            }
                            Fields::Unnamed(fn_fields) => fields.named.push(Field {
                                attrs: vec![],
                                vis: Visibility::Inherited,
                                mutability: FieldMutability::None,
                                ident: Some(ident_snake(name)),
                                colon_token: None,
                                ty: Type::Tuple(syn::TypeTuple {
                                    paren_token: Default::default(),
                                    elems: fn_fields.unnamed.into_iter().map(|f| f.ty).collect(),
                                }),
                            }),
                        },
                        StructOrEnum::Enum(mut e) => {
                            e.ident = ident(&format!("{}{}", name, "Args"));

                            fields.named.push(Field {
                                attrs: vec![],
                                vis: Visibility::Inherited,
                                mutability: FieldMutability::None,
                                ident: Some(ident_snake(&e.ident.to_string())),
                                colon_token: None,
                                ty: Type::Path(TypePath {
                                    qself: None,
                                    path: Path::from(e.ident.clone()),
                                }),
                            });

                            additional.push(StructOrEnum::Enum(e));
                        }
                    }
                }
            }

            SyntaxComponent::Group {
                components,
                combinator,
                ..
            } => {
                let res = generate_group(components, combinator.clone(), name);
                additional.extend(res.1);

                match res.0 {
                    StructOrEnum::Struct(s) => match s.fields {
                        Fields::Unit => {}
                        Fields::Named(fn_fields) => {
                            fields.named.extend(fn_fields.named);
                        }
                        Fields::Unnamed(fn_fields) => fields.named.push(Field {
                            attrs: vec![],
                            vis: Visibility::Inherited,
                            mutability: FieldMutability::None,
                            ident: None,
                            colon_token: None,
                            ty: Type::Tuple(syn::TypeTuple {
                                paren_token: Default::default(),
                                elems: fn_fields.unnamed.into_iter().map(|f| f.ty).collect(),
                            }),
                        }),
                    },
                    StructOrEnum::Enum(mut e) => {
                        let name = name.trim_end_matches("()");
                        e.ident = ident(&format!("{}{}", name, "Group"));

                        fields.named.push(Field {
                            attrs: vec![],
                            vis: Visibility::Inherited,
                            mutability: FieldMutability::None,
                            ident: Some(ident_snake(&e.ident.to_string())),
                            colon_token: Some(Default::default()),
                            ty: Type::Path(TypePath {
                                qself: None,
                                path: Path::from(e.ident.clone()),
                            }),
                        });

                        additional.push(StructOrEnum::Enum(e));
                    }
                }
            }

            SyntaxComponent::Literal { .. } => {}

            SyntaxComponent::Builtin { datatype, .. } => {
                fields.named.push(Field {
                    attrs: vec![],
                    vis: syn::Visibility::Inherited,
                    mutability: FieldMutability::None,
                    ident: Some(ident_snake(datatype)),
                    colon_token: None,
                    ty: Type::Path(TypePath {
                        qself: None,
                        path: Path::from(ident(datatype)),
                    }),
                })
            }

        _ => {
            todo!("Component: {:#?}", component);
            }
        }
    }

    (ty, additional)
}

pub fn generate_group_enum(
    components: &[SyntaxComponent],
    name: &str,
) -> (ItemEnum, Vec<StructOrEnum>) {
    let mut ty = new_enum(name);
    let mut additional = Vec::new();

    for component in components {
        match component {
            SyntaxComponent::GenericKeyword {
                keyword,
                multipliers: _,
            } => {
                //TODO: handle multipliers
                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: ident(keyword),
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
            SyntaxComponent::Function {
                name,
                multipliers: _,
                arguments,
            } => {
                let fields = if let Some(arguments) = arguments {
                    let res = generate_group_struct(
                        slice::from_ref(arguments),
                        &name.to_case(Case::Pascal),
                    );

                    additional.extend(res.1);

                    res.0.fields
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

            SyntaxComponent::Group {
                components,
                combinator,
                ..
            } => {
                let res = generate_group(components, combinator.clone(), name);
                additional.extend(res.1);
                match res.0 {
                    //TODO there needs to be another name for the group
                    StructOrEnum::Enum(e) => {
                        ty.variants.extend(e.variants);
                    }

                    StructOrEnum::Struct(s) => {
                        ty.variants.push(syn::Variant {
                            attrs: vec![],
                            ident: s.ident.clone(),
                            fields: Fields::Unnamed(FieldsUnnamed {
                                paren_token: Default::default(),
                                unnamed: Punctuated::from_iter(vec![Field {
                                    attrs: vec![],
                                    vis: syn::Visibility::Inherited,
                                    mutability: FieldMutability::None,
                                    ident: None,
                                    colon_token: None,
                                    ty: Type::Path(TypePath {
                                        qself: None,
                                        path: Path::from(s.ident.clone()),
                                    }),
                                }]),
                            }),
                            discriminant: None,
                        });
                    }
                }
            }

            SyntaxComponent::Literal { .. } => {}

            SyntaxComponent::Unset { .. } => {
                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: Ident::new("Unset", Span::call_site()),
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

            SyntaxComponent::Inherit { .. } => {
                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: Ident::new("Inherit", Span::call_site()),
                    fields: syn::Fields::Unit,
                    discriminant: None,
                });
            }

            SyntaxComponent::Value { value, .. } => {
                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: Ident::new(&value_to_ident(value), Span::call_site()),
                    fields: syn::Fields::Unit,
                    discriminant: None,
                });
            }

            SyntaxComponent::Builtin { datatype, .. } => {
                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: ident(datatype),
                    fields: Fields::Unnamed(FieldsUnnamed {
                        paren_token: Default::default(),
                        unnamed: Punctuated::from_iter(vec![Field {
                            attrs: vec![],
                            vis: syn::Visibility::Inherited,
                            mutability: FieldMutability::None,
                            ident: None,
                            colon_token: None,
                            ty: Type::Path(TypePath {
                                qself: None,
                                path: Path::from(ident(datatype)),
                            }),
                        }]),
                    }),
                    discriminant: None,
                });
            }

            SyntaxComponent::Property { .. } => {
                // what is this?
            }

            _ => {
                todo!("Component: {:#?}", component);
            }
        }
    }

    (ty, additional)
}
