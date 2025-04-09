pub mod group;

use convert_case::{Case, Casing};
use crate::component::group::StructOrEnum;
use crate::{ident, new_struct_unit};
use gosub_css3::matcher::syntax::SyntaxComponent;
use syn::punctuated::Punctuated;
use syn::{FieldMutability, Fields, Path, Type, TypePath};

pub fn generate_component_root(
    component: &SyntaxComponent,
    name: &str,
) -> Option<(StructOrEnum, Vec<StructOrEnum>)> {

    let (mut own, mut other) = match component {
        SyntaxComponent::Function {
            name,
            multipliers: _,
            arguments,
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

        SyntaxComponent::Definition { datatype, quoted, .. } => {
            let suffix = if *quoted { "" } else if datatype.contains("()") { "Fn" } else { "Def" };
            
            let datatype = &format!("{}{suffix}", datatype.to_case(Case::Pascal));
            let mut s = new_struct_unit(name);

            s.fields = Fields::Unnamed(syn::FieldsUnnamed {
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
            });

            (StructOrEnum::Struct(s), Vec::new())
        }

        SyntaxComponent::Literal { .. } => {
            (StructOrEnum::Struct(new_struct_unit(name)), Vec::new())

        },

        SyntaxComponent::Builtin { datatype, .. } => {
            let mut s = new_struct_unit(name);

            s.fields = Fields::Unnamed(syn::FieldsUnnamed {
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
            });

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
