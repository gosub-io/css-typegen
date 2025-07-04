pub mod group;
// mod group_ty;

use convert_case::{Case, Casing};
use crate::component::group::StructOrEnum;
use crate::{ident, ident_str, new_struct_unit};
use gosub_css3::matcher::syntax::SyntaxComponent;
use syn::punctuated::Punctuated;
use syn::{FieldMutability, Fields, Path, Type, TypePath};
use crate::multiplier::multiply_fields;
use crate::repr::{CssItem, CssRepr, CssTree, CssType};

pub fn generate_component_root(
    component: &SyntaxComponent,
    name: &str,
) -> Option<(StructOrEnum, Vec<StructOrEnum>)> {
    let (mut own, mut other) = match component {
        SyntaxComponent::Function {
            name,
            arguments,
            ..
        } => {
            let name = format!("{}Fn", name.to_case(Case::Pascal));
            if let Some(arg) = arguments {
                let res = generate_component_root(arg, &name)?;

                match res.0 {
                    StructOrEnum::Struct(mut s) => {
                        s.ident = ident(&name);

                        (StructOrEnum::Struct(s), res.1)
                    }
                    StructOrEnum::Enum(mut e) => {
                        e.ident = ident(&name);

                        (StructOrEnum::Enum(e), res.1)
                    }
                }
            } else {
                (StructOrEnum::Struct(new_struct_unit(&name)), Vec::new())
            }
        }

        SyntaxComponent::Group {
            components,
            combinator,
            multipliers: _,
        } => group::generate_group(components, combinator.clone(), name.into()),

        SyntaxComponent::GenericKeyword { .. } => {
            (StructOrEnum::Struct(new_struct_unit(name)), Vec::new())
        }

        SyntaxComponent::Definition { datatype, quoted, multipliers, .. } => {
            let suffix = if *quoted { "" } else if datatype.contains("()") { "Fn" } else { "Def" };
            
            let datatype = &format!("{}{suffix}", datatype.to_case(Case::Pascal));
            let mut s = new_struct_unit(name);

            let fields = syn::FieldsUnnamed {
                paren_token: Default::default(),
                unnamed: Punctuated::from_iter(vec![syn::Field {
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
            };

            let fields = multiply_fields(fields, multipliers);

            s.fields = Fields::Unnamed(fields);

            (StructOrEnum::Struct(s), Vec::new())
        }

        SyntaxComponent::Literal { .. } => {
            (StructOrEnum::Struct(new_struct_unit(name)), Vec::new())

        },

        SyntaxComponent::Builtin { datatype, multipliers } => {
            let mut s = new_struct_unit(name);

            let fields = syn::FieldsUnnamed {
                paren_token: Default::default(),
                unnamed: Punctuated::from_iter(vec![syn::Field {
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
            };

            let fields = multiply_fields(fields, multipliers);

            s.fields = Fields::Unnamed(fields);

            (StructOrEnum::Struct(s), Vec::new())
        }

        _ => {
            todo!("Component: {:#?} {:?}", component, name);
        }
    };

    fix_unit(&mut own);

    other.iter_mut().for_each(fix_unit);

    Some((own, other))
}


pub fn generate_component_root_ty(
    component: &SyntaxComponent,
    name: &str,
) -> Option<(CssType, Vec<CssType>)> {
    let (mut own, mut other) = match component {
        SyntaxComponent::Function {
            name: name_raw,
            arguments,
            ..
        } => {
            let name = format!("{}Fn", name_raw.to_case(Case::Pascal));
            if let Some(arg) = arguments {
                let mut res = generate_component_root_ty(arg, &name)?;
                
                
                res.0.id = ident_str(&name);

                res
            } else {
                (CssType::unit(name_raw.to_owned(), &name), Vec::new())
            }
        }

        SyntaxComponent::Group {
            components,
            combinator,
            multipliers: _,
        } => {
            // group_ty::generate_group(components, combinator.clone(), name.into())
            todo!()
        },

        SyntaxComponent::GenericKeyword { .. } => {
            (CssType::unit(name.to_owned(), name), Vec::new())
        }

        SyntaxComponent::Definition { datatype, quoted, multipliers, .. } => {
            let suffix = if *quoted { "" } else if datatype.contains("()") { "Fn" } else { "Def" };

            let datatype = &format!("{}{suffix}", datatype.to_case(Case::Pascal));
            
            let rep = CssRepr::Sub(ident_str(datatype)).into();
            let multipliers = multipliers.as_slice().into();

            let item: CssTree = CssItem::with_multiplier(rep, multipliers).into();

            (
                CssType::new(name.to_owned(), ident_str(name), item.into()),
                Vec::new()
            )
        }

        SyntaxComponent::Literal { .. } => {
            (CssType::unit(name.to_owned(), name), Vec::new())

        },

        SyntaxComponent::Builtin { datatype, multipliers } => {
            let rep = CssRepr::Sub(ident_str(datatype)).into();
            let multipliers = multipliers.as_slice().into();
            
            let item: CssTree = CssItem::with_multiplier(rep, multipliers).into();

            (
                CssType::new(name.to_owned(), ident_str(name), item.into()),
                Vec::new()
            )
        }

        _ => {
            todo!("Component: {:#?} {:?}", component, name);
        }
    };

    Some((own, other))
}

fn fix_unit(item: &mut StructOrEnum) {
    match item {
        StructOrEnum::Struct(s) => match s.fields {
            Fields::Unnamed(ref mut fields) if fields.unnamed.is_empty() => {
                s.fields = Fields::Unit;
            }

            Fields::Named(ref mut fields) if fields.named.is_empty() => {
                s.fields = Fields::Unit;
            }

            _ => {}
        },
        StructOrEnum::Enum(e) => {
            e.variants.iter_mut().for_each(|v| match v.fields {
                Fields::Unnamed(ref mut fields) if fields.unnamed.is_empty() => {
                    v.fields = Fields::Unit;
                }

                Fields::Named(ref mut fields) if fields.named.is_empty() => {
                    v.fields = Fields::Unit;
                }

                _ => {}
            });
        }
    }
}

// enum Display {
//     InsideOutside(Option<DisplayOutside>, Option<DisplayInside>)
// }
