mod group;

use gosub_css3::matcher::syntax::SyntaxComponent;
use syn::{FieldMutability, Fields, Path, Type, TypePath};
use syn::punctuated::Punctuated;
use crate::component::group::StructOrEnum;
use crate::{ident, new_struct, new_struct_unit};

pub fn generate_component_root(component: &SyntaxComponent, name: &str) -> Option<(StructOrEnum, Vec<StructOrEnum>)> {
    Some(match component {
        SyntaxComponent::Function { name, multipliers: _, arguments } => {
            if let Some(arg) = arguments {
                let res = generate_component_root(arg, name)?;

                match res.0 {
                    StructOrEnum::Struct(mut s) => {
                        s.ident = ident(name);

                        (StructOrEnum::Struct(s), res.1)
                    }
                    StructOrEnum::Enum(mut e) => {
                        e.ident = ident(name);

                        (StructOrEnum::Enum(e), res.1)
                    }
                }
            } else {
                (StructOrEnum::Struct(new_struct_unit(name)), Vec::new())
            }
        }

        SyntaxComponent::Group {components, combinator, multipliers: _ } => {
            group::generate_group(components, combinator.clone(), name)
        }

        SyntaxComponent::GenericKeyword { .. } => {
            (StructOrEnum::Struct(new_struct_unit(name)), Vec::new())
        }

        SyntaxComponent::Definition { datatype, .. } => {
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

        SyntaxComponent::Literal { .. } => return None,

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
                    })
                }])
            });

            (StructOrEnum::Struct(s), Vec::new())
        },


        _ => {
            todo!("Component: {:#?} {:?}", component, name);
            println!("Component: {:#?}", component);
        }
    })
    
}


// enum Display {
//     InsideOutside(Option<DisplayOutside>, Option<DisplayInside>)
// }