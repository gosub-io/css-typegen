use crate::component::generate_component_root;
use crate::value::value_to_ident;
use crate::{ident, new_enum, new_struct, Name};
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

const NAME_RANGE: std::ops::Range<usize> = 5..50;

#[derive(Debug, PartialEq, Eq, Hash)]
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
    name: Name,
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
    name: Name,
) -> (ItemStruct, Vec<StructOrEnum>) {
    let mut ty = new_struct(name.name);
    let mut additional = Vec::new();
    let mut better_name = String::new();

    let fields = match &mut ty.fields {
        syn::Fields::Unnamed(fields) => fields,
        _ => panic!("Expected unnamed fields"),
    };

    for component in components {
        match component {
            SyntaxComponent::GenericKeyword { keyword, .. } => {
                better_name.push_str(&keyword.to_case(Case::Pascal));
            }
            SyntaxComponent::Inherit { .. } => {
                better_name.push_str("Inherit");
            }
            SyntaxComponent::Initial { .. } => {
                better_name.push_str("Initial");
            }
            SyntaxComponent::Unset { .. } => {
                better_name.push_str("Unset");
            }

            SyntaxComponent::Definition { datatype, .. } => {
                let ty = datatype.to_case(Case::Pascal);

                let ty = ty.trim_end_matches("()");
                better_name.push_str(ty);

                let ty = Ident::new(ty, Span::call_site());

                fields.unnamed.push(Field {
                    attrs: Vec::new(),
                    vis: syn::Visibility::Inherited,
                    mutability: FieldMutability::None,
                    ident: None,
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
                better_name.push_str(&name.to_case(Case::Pascal));
                
                if let Some(arguments) = arguments {
                    let Some(res) = generate_component_root(arguments, name) else {
                        continue;
                    };
                    additional.extend(res.1);

                    match res.0 {
                        StructOrEnum::Struct(s) => match s.fields {
                            Fields::Unit => {}
                            Fields::Named(fn_fields) => {
                                
                                
                                
                                // fields.named.extend(fn_fields.named);
                            }
                            Fields::Unnamed(fn_fields) => {
                                let ty = if fn_fields.unnamed.len() == 1 {
                                    fn_fields.unnamed.first().unwrap().ty.clone()
                                } else {
                                    Type::Tuple(syn::TypeTuple {
                                        paren_token: Default::default(),
                                        elems: fn_fields.unnamed.into_iter().map(|f| f.ty).collect(),
                                    })
                                };

                                fields.unnamed.push(Field {
                                    attrs: vec![],
                                    vis: Visibility::Inherited,
                                    mutability: FieldMutability::None,
                                    ident: None,
                                    colon_token: None,
                                    ty,
                                })
                            },
                        },
                        StructOrEnum::Enum(mut e) => {
                            e.ident = ident(&format!("{}{}", name, "Args"));

                            fields.unnamed.push(Field {
                                attrs: vec![],
                                vis: Visibility::Inherited,
                                mutability: FieldMutability::None,
                                ident: None,
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
                let name_init = name.name.trim_end_matches("()");
                let name_init = format!("{}Group", name_init);
                let res = generate_group(components, combinator.clone(), Name::new(&name_init));
                additional.extend(res.1);

                match res.0 {
                    StructOrEnum::Struct(s) => match s.fields {
                        Fields::Unit => {}
                        Fields::Named(fn_fields) => {
                            // fields.unnamed.extend(fn_fields.named);
                        }
                        Fields::Unnamed(fn_fields) => {
                            if !fn_fields.unnamed.is_empty() {
                                let ty = if fn_fields.unnamed.len() == 1 {
                                    fn_fields.unnamed.first().unwrap().ty.clone()
                                } else {
                                    Type::Tuple(syn::TypeTuple {
                                        paren_token: Default::default(),
                                        elems: fn_fields.unnamed.into_iter().map(|f| f.ty).collect(),
                                    })
                                };
                                
                                
                                fields.unnamed.push(Field {
                                    attrs: vec![],
                                    vis: Visibility::Inherited,
                                    mutability: FieldMutability::None,
                                    ident: None,
                                    colon_token: None,
                                    ty,
                                })
                            }
                            
                        },
                    },
                    StructOrEnum::Enum(e) => {
                        fields.unnamed.push(Field {
                            attrs: vec![],
                            vis: Visibility::Inherited,
                            mutability: FieldMutability::None,
                            ident: None,
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

            SyntaxComponent::Literal { .. } => {}

            SyntaxComponent::Builtin { datatype, .. } => {
                better_name.push_str(&datatype.to_case(Case::Pascal));
                
                fields.unnamed.push(Field {
                    attrs: vec![],
                    vis: syn::Visibility::Inherited,
                    mutability: FieldMutability::None,
                    ident: None,
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

    if name.find_better_name && NAME_RANGE.contains(&better_name.len()) {
        ty.ident = ident(&better_name);
    }

    (ty, additional)
}

pub fn generate_group_enum(
    components: &[SyntaxComponent],
    name: Name,
) -> (ItemEnum, Vec<StructOrEnum>) {
    let mut ty = new_enum(name.name);
    let mut additional = Vec::new();
    let mut better_name = String::new();

    for component in components {
        match component {
            SyntaxComponent::GenericKeyword {
                keyword,
                multipliers: _,
            } => {
                better_name.push_str(&keyword.to_case(Case::Pascal));

                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: ident(keyword),
                    fields: syn::Fields::Unit,
                    discriminant: None,
                });
            }
            SyntaxComponent::Inherit { .. } => {
                better_name.push_str("Inherit");

                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: Ident::new("Inherit", Span::call_site()),
                    fields: syn::Fields::Unit,
                    discriminant: None,
                });
            }

            SyntaxComponent::Initial { .. } => {
                better_name.push_str("Initial");

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

                better_name.push_str(&id);


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
                let name = name.to_case(Case::Pascal);
                better_name.push_str(&name);

                let fields = if let Some(arguments) = arguments {
                    let res = generate_group_struct(
                        slice::from_ref(arguments),
                        name.as_str().into()
                    );

                    additional.extend(res.1);

                    fix_fields(res.0.fields)
                } else {
                    syn::Fields::Unit
                };

                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: Ident::new(&name, Span::call_site()),
                    fields,
                    discriminant: None,
                });
            }

            SyntaxComponent::Group {
                components,
                combinator,
                ..
            } => {
                let res = generate_group(components, combinator.clone(), Name::new(&format!("{}Group", name.name)));
                additional.extend(res.1);
                match res.0 {
                    //TODO there needs to be another name for the group
                    StructOrEnum::Enum(e) => {
                        ty.variants.extend(e.variants);
                    }

                    StructOrEnum::Struct(s) => {
                        let fields = fix_fields(s.fields);
                        
                        ty.variants.push(syn::Variant {
                            attrs: vec![],
                            ident: s.ident.clone(),
                            fields,
                            discriminant: None,
                        });
                    }
                }
            }

            SyntaxComponent::Literal { literal, .. } => {
                let lit = get_literal(literal);
                
                better_name.push_str(&lit);
                
                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: Ident::new(&lit, Span::call_site()),
                    fields: syn::Fields::Unit,
                    discriminant: None,
                });
            }

            SyntaxComponent::Unset { .. } => {
                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: Ident::new("Unset", Span::call_site()),
                    fields: syn::Fields::Unit,
                    discriminant: None,
                });
            }

            SyntaxComponent::Value { value, .. } => {
                let name = value_to_ident(value);

                better_name.push_str(&name.to_string());

                ty.variants.push(syn::Variant {
                    attrs: vec![],
                    ident: Ident::new(&name, Span::call_site()),
                    fields: syn::Fields::Unit,
                    discriminant: None,
                });
            }

            SyntaxComponent::Builtin { datatype, .. } => {
                better_name.push_str(&datatype.to_case(Case::Pascal));

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
    
    if name.find_better_name && NAME_RANGE.contains(&better_name.len()) {
        ty.ident = ident(&better_name);
    }

    // println!("Better name: {}", better_name);

    (ty, additional)
}


fn fix_fields(fields: Fields) -> Fields {
    match fields {
        Fields::Unnamed(mut fields) => {
            if fields.unnamed.is_empty() {
                return Fields::Unit;
            }
            
            if fields.unnamed.len() == 1 {
                let field = fields.unnamed.pop().unwrap().into_value();

                if let Type::Tuple(tuple) = field.ty {
                    for ty in tuple.elems {
                        fields.unnamed.push(Field {
                            attrs: vec![],
                            vis: syn::Visibility::Inherited,
                            mutability: FieldMutability::None,
                            ident: None,
                            colon_token: None,
                            ty,
                        });

                    }
                } else {
                    fields.unnamed.push(field);
                }
            }
            
            for field in fields.unnamed.iter_mut() {
                if let Type::Tuple(tuple) = &field.ty {
                    if tuple.elems.len() == 1 {
                        field.ty = tuple.elems.first().unwrap().clone();
                    }
                    
                }
            }

            Fields::Unnamed(fields)
        },
        
        Fields::Named(fields) => {
            if fields.named.is_empty() {
                return Fields::Unit;
            }

            Fields::Named(fields)
        },

        Fields::Unit => Fields::Unit,
    }
}

fn get_literal(literal: &str) -> String {
    let literal = match literal {
        "+" => "Plus",
        "-" => "Minus",
        "*" => "Asterisk",
        "/" => "Slash",
        "%" => "Percent",
        "^" => "Caret",
        "!" => "Exclamation",
        "=" => "Equal",
        "<" => "LessThan",
        ">" => "GreaterThan",
        "$" => "Dollar",
        "#" => "Hash",
        "~" => "Tilde",
        "|" => "Pipe",
        _ => literal,
    };
    
    
    
    let mut lit = literal.to_case(Case::Pascal);

    if lit.starts_with(char::is_numeric) {
        lit = format!("_{}", lit);
    }

    if lit.to_lowercase() == "self" {
        lit.remove(1);
    }

    lit
}
