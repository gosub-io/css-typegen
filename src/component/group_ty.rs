use crate::component::{generate_component_root, generate_component_root_ty};
use crate::value::value_to_ident;
use crate::{ident, ident_str, new_enum, new_struct, Name};
use convert_case::{Case, Casing};
use gosub_css3::matcher::syntax::{GroupCombinators, SyntaxComponent, SyntaxComponentMultiplier};
use proc_macro2::{Ident, Span};
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::slice;
use std::sync::atomic::{AtomicU8, Ordering};
use syn::__private::ToTokens;
use syn::punctuated::Punctuated;
use syn::{
    Field, FieldMutability, Fields, FieldsUnnamed, ItemEnum, ItemStruct, Path, Type, TypePath,
    Visibility, parse_quote,
};
use crate::component::group::{get_literal, StructOrEnum};
use crate::multiplier::{multiply, multiply_fields};
use crate::repr::{CssItem, CssRepr, CssTree, CssType, CssTypeRepr, Multiplier};

const NAME_RANGE: std::ops::Range<usize> = 5..50;

const BOXES: &[&[&str]] = &[
    &["ColorDef", "ColorBase"],
];



#[derive(Clone)]
struct VariantInfo {
    base: String,
    kind: String,
    args: Option<Vec<String>>,
    variant_name: String,
    variant_id: String,
    ty: CssTree,
}

pub fn generate_group(
    components: &[SyntaxComponent],
    combinator: GroupCombinators,
    name: Name,
) -> (CssType, Vec<CssType>) {
    static CENTER: AtomicU8 = AtomicU8::new(0);
    const CENTER_NAMES: &[&str] = &["CenterLR", "CenterTB"];
    
    match combinator {
        GroupCombinators::Juxtaposition | GroupCombinators::AllAnyOrder | GroupCombinators::AtLeastOneAnyOrder => {
            let mut res = generate_group_struct(components, name, combinator == GroupCombinators::AtLeastOneAnyOrder);
            if res.0.id == "Center" {
                res.0.id = ident_str(CENTER_NAMES[CENTER.fetch_add(1, Ordering::SeqCst) as usize]);
            }
            
            res
        }

        GroupCombinators::ExactlyOne => {
            let mut res = generate_group_enum(components, name);
            if res.0.id.to_string() == "Center" {
                res.0.id = ident_str(CENTER_NAMES[CENTER.fetch_add(1, Ordering::SeqCst) as usize]);
            }
            
            res
        }
    }
}



pub fn generate_group_struct(
    components: &[SyntaxComponent],
    name: Name,
    is_aloao: bool, //is_at_least_one_any_order
) -> (CssType, Vec<CssType>) {
    let mut additional = Vec::new();
    let mut better_name = String::new();
    
    let mut items = Vec::with_capacity(components.len());

    
    let mut group_name = 0;

    for component in components {
        match component {
            SyntaxComponent::GenericKeyword { keyword, .. } => {
                if is_aloao {
                    let id = ident_str(keyword);

                    additional.push(CssType::unit(keyword.clone(), &id));
                    
                    items.push(kw(keyword.clone(), id));
                }
                
                better_name.push_str(&keyword.to_case(Case::Pascal));
            }
            SyntaxComponent::Inherit { .. } => {
                if is_aloao {
                    items.push(kwd("Inherit"));
                }
                better_name.push_str("Inherit");
            }
            SyntaxComponent::Initial { .. } => {
                if is_aloao {
                    items.push(kwd("Initial"));
                }
                better_name.push_str("Initial");
            }
            SyntaxComponent::Unset { .. } => {
                if is_aloao {
                    items.push(kwd("Unset"));
                }
                better_name.push_str("Unset");
            }

            SyntaxComponent::Definition { datatype, quoted, multipliers, .. } => { 
                let suffix = if *quoted { "" } else if datatype.contains("()") { "Fn" } else { "Def" };

                let ty = datatype.trim_end_matches("()");

                better_name.push_str(ty);
                let ty = format!("{}{suffix}", ty.to_case(Case::Pascal));

                
                let mut multipliers = multipliers.clone();
                
                if is_aloao && !multipliers.contains(&SyntaxComponentMultiplier::Optional) {
                    multipliers.push(SyntaxComponentMultiplier::Optional);
                    
                }
                
                items.push(CssItem::with_multiplier(
                    CssRepr::Sub(ty),
                    multipliers.as_slice().into(),
                ));
            }

            SyntaxComponent::Function {
                name,
                multipliers,
                arguments,
            } => {
                let name = name.to_case(Case::Pascal);
                better_name.push_str(&name);
                let name = &format!("{}Fn", name);
                
                if let Some(arguments) = arguments {
                    let Some(mut res) = generate_component_root_ty(arguments, name) else {
                        continue;
                    };
                    additional.extend(res.1);
                    
                    
                    match res.0.repr {
                        CssTypeRepr::Struct(mut s) => {
                            match s.items.len() {
                                0 => {}
                                1 => {
                                    let mut item = s.items.pop().expect("unreachable");
                                    
                                    item.add_multiplier(multipliers.as_slice().into());
                                    
                                    items.push(item);
                                }
                                
                                _ => {
                                    items.push(CssItem::with_multiplier(
                                        CssRepr::Tuple(s),
                                        multipliers.as_slice().into(),
                                    ));
                                }
                            }
                            
                        }
                        CssTypeRepr::Enum(_) => {
                            res.0.id = ident_str(&format!("{}{}", res.0.id, "Args"));
                            
                            items.push(CssItem::with_multiplier(
                                CssRepr::Sub(res.0.id.clone()),
                                multipliers.as_slice().into(),
                            ));
                            
                            additional.push(res.0);
                        }
                    }
                }
            }

            SyntaxComponent::Group {
                components,
                combinator,
                multipliers,
            } => {
                let name_init = name.name.trim_end_matches("()");
                let name_init = format!("{name_init}Group{group_name}");
                group_name += 1;
                let res = generate_group(components, *combinator, Name::new(&name_init));
                additional.extend(res.1);

                match res.0.repr {
                    CssTypeRepr::Struct(mut s) => {
                        match s.items.len() {
                            0 => {}
                            1 => {
                                let mut item = s.items.pop().expect("unreachable");

                                item.add_multiplier(multipliers.as_slice().into());

                                items.push(item);
                            }

                            _ => {
                                items.push(CssItem::with_multiplier(
                                    CssRepr::Tuple(s),
                                    multipliers.as_slice().into(),
                                ));
                            }
                        }
                    }
                    CssTypeRepr::Enum(_) => {
                        items.push(CssItem::with_multiplier(
                            CssRepr::Sub(res.0.id.clone()),
                            multipliers.as_slice().into(),
                        ));

                        additional.push(res.0);
                    }
                }
            }

            SyntaxComponent::Literal { .. } => {}

            SyntaxComponent::Builtin { datatype, multipliers } => {
                better_name.push_str(&datatype.to_case(Case::Pascal));
                
                items.push(CssItem::with_multiplier(
                    CssRepr::Sub(datatype.to_case(Case::Pascal)),
                    multipliers.as_slice().into(),
                ));
            }

        _ => {
            todo!("Component: {:#?}", component);
            }
        }
    }
    

    let name = if name.find_better_name && NAME_RANGE.contains(&better_name.len()) {
        ident_str(&better_name)
    } else {
        name.name.to_owned()
    };
    
    
    let ty = CssType::new(name.clone(), name, CssTree::with_items(items).into());

    (ty, additional)
}

pub fn generate_group_enum(
    components: &[SyntaxComponent],
    name: Name,
) -> (CssType, Vec<CssType>) {
    let mut additional = Vec::new();
    let mut better_name = String::new();

    let mut group_name = 0;

    let mut candidates: Vec<VariantInfo> = Vec::new();

    for component in components.iter() {
        match component {
            SyntaxComponent::GenericKeyword { keyword, .. } => {
                better_name.push_str(&keyword.to_case(Case::Pascal));
                let id = ident(keyword);
                candidates.push(VariantInfo {
                    base: id.to_string(),
                    kind: "GenericKeyword".to_string(),
                    args: None,
                    variant_name: keyword.clone(),
                    variant_id: ident_str(keyword),
                    ty: CssTree::new(),
                });
            }
            SyntaxComponent::Inherit { .. } => {
                better_name.push_str("Inherit");
                candidates.push(VariantInfo {
                    base: "Inherit".to_string(),
                    kind: "Inherit".to_string(),
                    args: None,
                    variant_name: "Inherit".to_string(),
                    variant_id: "Inherit".to_string(),
                    ty: CssTree::new()
                });
            }
            SyntaxComponent::Initial { .. } => {
                better_name.push_str("Initial");
                candidates.push(VariantInfo {
                    base: "Initial".to_string(),
                    kind: "Initial".to_string(),
                    args: None,
                    variant_name: "Initial".to_string(),
                    variant_id: "Initial".to_string(),
                    ty: CssTree::new()
                });
            }
            SyntaxComponent::Definition { datatype, quoted, multipliers, .. } => {
                let suffix = if *quoted { "" } else if datatype.contains("()") { "Fn" } else { "Def" };
                let id = datatype.to_case(Case::Pascal).replace("()", "");
                better_name.push_str(&id);
                
                let id_def = &format!("{}{suffix}", id);
                
                let ty = CssItem::with_multiplier(
                    CssRepr::Sub(id_def.to_string()),
                    multipliers.as_slice().into(),
                ).into();
                
                
                candidates.push(VariantInfo {
                    base: id.clone(),
                    kind: suffix.to_string(),
                    args: None,
                    variant_name: datatype.clone(),
                    variant_id: id,
                    ty,
                });
            }
            SyntaxComponent::Function { name: fn_name, multipliers, arguments } => {
                let name = fn_name.to_case(Case::Pascal);
                better_name.push_str(&name);
                let name_fn = &format!("{}Fn", name);

                let (fields, args_info) = if let Some(arguments) = arguments {
                    let res = generate_group_struct(
                        slice::from_ref(arguments),
                        name_fn.as_str().into(),
                        false
                    );
                    additional.extend(res.1);

                    let mut arg_names = Vec::new();
                    match &res.0.fields {
                        Fields::Unnamed(fn_fields) => {
                            for f in &fn_fields.unnamed {
                                arg_names.push(format!("{}", type_to_string(&f.ty)));
                            }
                        }
                        _ => {}
                    }
                    (fix_fields(res.0.fields, multipliers), Some(arg_names))
                } else {
                    (syn::Fields::Unit, None)
                };

                candidates.push(VariantInfo {
                    base: name.clone(),
                    kind: "Fn".to_string(),
                    args: args_info,
                    variant: syn::Variant {
                        attrs: vec![],
                        ident: Ident::new(&name, Span::call_site()),
                        fields,
                        discriminant: None,
                    },
                });
            }
            SyntaxComponent::Group { components, combinator, multipliers } => {
                let res = generate_group(components, *combinator, Name::new(&format!("{}Group{}", name.name, group_name)));
                group_name += 1;
                additional.extend(res.1);
                match res.0 {
                    StructOrEnum::Enum(e) => {
                        for v in e.variants {
                            candidates.push(VariantInfo {
                                base: v.ident.to_string(),
                                kind: "GroupEnum".to_string(),
                                args: None,
                                variant: {
                                    let mut v = v;
                                    v.fields = fix_fields(v.fields, multipliers);
                                    v
                                },
                            });
                        }
                    }
                    StructOrEnum::Struct(s) => {
                        let fields = fix_fields(s.fields, multipliers);
                        candidates.push(VariantInfo {
                            base: s.ident.to_string(),
                            kind: "GroupStruct".to_string(),
                            args: None,
                            variant: syn::Variant {
                                attrs: vec![],
                                ident: s.ident.clone(),
                                fields,
                                discriminant: None,
                            },
                        });
                    }
                }
            }
            SyntaxComponent::Literal { literal, .. } => {
                let lit = get_literal(literal);
                better_name.push_str(&lit);
                
                candidates.push(VariantInfo {
                    base: lit.clone(),
                    kind: "Literal".to_string(),
                    args: None,
                    variant: syn::Variant {
                        attrs: vec![],
                        ident: Ident::new(&lit, Span::call_site()),
                        fields: syn::Fields::Unit,
                        discriminant: None,
                    },
                });
            }
            SyntaxComponent::Unset { .. } => {
                candidates.push(VariantInfo {
                    base: "Unset".to_string(),
                    kind: "Unset".to_string(),
                    args: None,
                    variant: syn::Variant {
                        attrs: vec![],
                        ident: Ident::new("Unset", Span::call_site()),
                        fields: syn::Fields::Unit,
                        discriminant: None,
                    },
                });
            }
            SyntaxComponent::Value { value, .. } => {
                let name = value_to_ident(value);
                better_name.push_str(&name.to_string());
                candidates.push(VariantInfo {
                    base: name.to_string(),
                    kind: "Value".to_string(),
                    args: None,
                    variant: syn::Variant {
                        attrs: vec![],
                        ident: Ident::new(&name, Span::call_site()),
                        fields: syn::Fields::Unit,
                        discriminant: None,
                    },
                });
            }
            SyntaxComponent::Builtin { datatype, multipliers } => {
                better_name.push_str(&datatype.to_case(Case::Pascal));
                candidates.push(VariantInfo {
                    base: datatype.to_case(Case::Pascal),
                    kind: "Builtin".to_string(),
                    args: None,
                    variant: syn::Variant {
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
                                ty: multiply(Type::Path(TypePath {
                                    qself: None,
                                    path: Path::from(ident(datatype)),
                                }), multipliers),
                            }]),
                        }),
                        discriminant: None,
                    },
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

    let mut name_map: HashMap<String, Vec<usize>> = HashMap::new();
    for (i, c) in candidates.iter().enumerate() {
        name_map.entry(c.base.clone()).or_default().push(i);
    }

    let mut used_names = HashSet::new();
    for (_, idxs) in &name_map {
        if idxs.len() == 1 {
            let c = &mut candidates[idxs[0]];
            let mut ident_str = c.base.clone();
            let mut n = 1;
            
            while used_names.contains(&ident_str) {
                ident_str = format!("{}{}", c.base, n);
                n += 1;
            }
            c.variant.ident = ident(&ident_str);
            used_names.insert(ident_str);
        } else {
            let all_fn = idxs.iter().all(|&i| candidates[i].kind == "Fn");
            if all_fn {
                let args_list: Vec<_> = idxs.iter().map(|&i| candidates[i].args.clone().unwrap_or_default()).collect();
                let mut min_diff = vec![];
                let max_args = args_list.iter().map(|a| a.len()).max().unwrap_or(0);
                
                for arg_idx in 0..max_args {
                    let mut vals = HashSet::new();

                    for args in &args_list {
                        if let Some(val) = args.get(arg_idx) {
                            vals.insert(val);
                        }
                    }
                    
                    if vals.len() > 1 {
                        min_diff.push(arg_idx);
                    }
                }
                for (_, &i) in idxs.iter().enumerate() {
                    let c = &mut candidates[i];
                    let mut ident_str = format!("{}Fn", c.base);

                    if let Some(args) = &c.args {
                        let mut parts = Vec::new();
                        
                        for &diff_idx in &min_diff {
                            if let Some(arg) = args.get(diff_idx) {
                                parts.push(arg.clone());
                            }
                        }
                        
                        if !parts.is_empty() {
                            ident_str = format!("{}Fn{}", c.base, parts.join(""));
                        }
                    }
                    
                    let mut n = 1;
                    let mut try_ident = ident_str.clone();
                    
                    while used_names.contains(&try_ident) {
                        try_ident = format!("{}{}", ident_str, n); //TODO: this is bad, there always is something different out out and we need to find it, for now this fixes some errors
                        n += 1;
                    }
                    
                    c.variant.ident = ident(&try_ident);
                    used_names.insert(try_ident);
                }
            } else {
                for &i in idxs {
                    let c = &mut candidates[i];
                    let ident_str = format!("{}{}", c.base, c.kind);
                    let mut n = 1;
                    let mut try_ident = ident_str.clone();
                    
                    while used_names.contains(&try_ident) {
                        try_ident = format!("{}{}", ident_str, n);
                        n += 1;
                    }
                    
                    c.variant.ident = ident(&try_ident);
                    used_names.insert(try_ident);
                }
            }
        }
    }

    for c in candidates {
        ty.variants.push(c.variant);
    }

    if name.find_better_name && NAME_RANGE.contains(&better_name.len()) {
        ty.ident = ident(&better_name);
    }

    for b in BOXES {
        if ty.ident.to_string() == b[0] {
            for variant in &mut ty.variants {
                if variant.ident.to_string() == b[1] {
                    if let syn::Fields::Unnamed(ref mut fields) = variant.fields {
                        if let Some(field) = fields.unnamed.first_mut() {
                            let orig_ty = field.ty.clone();
                            field.ty = parse_quote!(Box<#orig_ty>);
                        }
                    }
                }
            }
        }
    }

    (ty, additional)
}

fn fix_fields(fields: Fields, multipliers: &[SyntaxComponentMultiplier]) -> Fields {
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

            Fields::Unnamed(multiply_fields(fields, multipliers))
        },
        
        Fields::Named(fields) => {
            if fields.named.is_empty() {
                return Fields::Unit;
            }
            
            match multipliers.len() {
                0 => Fields::Named(fields),
                1 if multipliers[0] == SyntaxComponentMultiplier::Once => {
                    Fields::Named(fields)
                }
                _ => unimplemented!("Cannot handle multipliers for named fields"),
            }
        },

        Fields::Unit => Fields::Unit,
    }
}

pub fn kw(name: String, id: String) -> CssItem {
    CssItem {
        combinator: Multiplier::Optional,
        repr: CssRepr::Keyword(name, id),
    }
}

pub fn kwd(str: &str) -> CssItem {
    kw(str.to_owned(), ident_str(str))
}
