use crate::component::{generate_component_root};
use crate::value::value_to_ident;
use crate::{ident, new_enum, new_struct, Name};
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
use crate::multiplier::{multiply, multiply_fields};

const NAME_RANGE: std::ops::Range<usize> = 5..50;

const BOXES: &[&[&str]] = &[
    &["ColorDef", "ColorBase"],
];

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

#[derive(Clone)]
struct VariantInfo {
    base: String,
    kind: String,
    args: Option<Vec<String>>,
    variant: syn::Variant,
}

pub fn generate_group(
    components: &[SyntaxComponent],
    combinator: GroupCombinators,
    name: Name,
) -> (StructOrEnum, Vec<StructOrEnum>) {
    static CENTER: AtomicU8 = AtomicU8::new(0);
    const CENTER_NAMES: &[&str] = &["CenterLR", "CenterTB"];
    
    match combinator {
        GroupCombinators::Juxtaposition | GroupCombinators::AllAnyOrder | GroupCombinators::AtLeastOneAnyOrder => {
            let mut res = generate_group_struct(components, name, combinator == GroupCombinators::AtLeastOneAnyOrder);
            if res.0.ident.to_string() == "Center" {
                res.0.ident = ident(CENTER_NAMES[CENTER.fetch_add(1, Ordering::SeqCst) as usize]);
            }
            (StructOrEnum::Struct(res.0), res.1)
        }

        GroupCombinators::ExactlyOne => {
            let mut res = generate_group_enum(components, name);
            if res.0.ident.to_string() == "Center" {
                res.0.ident = ident(CENTER_NAMES[CENTER.fetch_add(1, Ordering::SeqCst) as usize]);
            }
            (StructOrEnum::Enum(res.0), res.1)
        }
    }
}

pub fn generate_group_struct(
    components: &[SyntaxComponent],
    name: Name,
    is_aloao: bool, //is_at_least_one_any_order
) -> (ItemStruct, Vec<StructOrEnum>) {
    let mut ty = new_struct(name.name);
    let mut additional = Vec::new();
    let mut better_name = String::new();

    let fields = match &mut ty.fields {
        syn::Fields::Unnamed(fields) => fields,
        _ => panic!("Expected unnamed fields"),
    };
    
    let mut group_name = 0;

    for component in components {
        match component {
            SyntaxComponent::GenericKeyword { keyword, .. } => {
                if is_aloao {
                    let keyword = ident(keyword);
                    fields.unnamed.push(parse_quote!(Option<#keyword>));
                    
                    additional.push(StructOrEnum::Struct(parse_quote!(struct #keyword;)));
                }
                
                better_name.push_str(&keyword.to_case(Case::Pascal));
            }
            SyntaxComponent::Inherit { .. } => {
                if is_aloao {
                    fields.unnamed.push(parse_quote!(Option<Inherit>))
                }
                better_name.push_str("Inherit");
            }
            SyntaxComponent::Initial { .. } => {
                if is_aloao {
                    fields.unnamed.push(parse_quote!(Option<Initial>))
                }
                better_name.push_str("Initial");
            }
            SyntaxComponent::Unset { .. } => {
                if is_aloao {
                    fields.unnamed.push(parse_quote!(Option<Unset>))
                }
                better_name.push_str("Unset");
            }

            SyntaxComponent::Definition { datatype, quoted, multipliers, .. } => { 
                let suffix = if *quoted { "" } else if datatype.contains("()") { "Fn" } else { "Def" };

                let ty = datatype.trim_end_matches("()");

                better_name.push_str(ty);
                let ty = format!("{}{suffix}", ty.to_case(Case::Pascal));

                let ty = Ident::new(&ty, Span::call_site());
                
                let ty = Type::Path(TypePath {
                    qself: None,
                    path: ty.into(),
                });
                
                let mut multipliers = multipliers.clone();
                
                if is_aloao && !multipliers.contains(&SyntaxComponentMultiplier::Optional) {
                    multipliers.push(SyntaxComponentMultiplier::Optional);
                    
                }

                fields.unnamed.push(Field {
                    attrs: Vec::new(),
                    vis: syn::Visibility::Inherited,
                    mutability: FieldMutability::None,
                    ident: None,
                    colon_token: None,
                    ty: multiply(ty, &multipliers),
                })
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
                    let Some(res) = generate_component_root(arguments, name) else {
                        continue;
                    };
                    additional.extend(res.1);

                    match res.0 {
                        StructOrEnum::Struct(s) => match s.fields {
                            Fields::Unit => {}
                            Fields::Named(_) => {
                                
                                
                                
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
                                    ty: multiply(ty, multipliers),
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
                                ty: multiply(Type::Path(TypePath {
                                    qself: None,
                                    path: Path::from(e.ident.clone()),
                                }), multipliers),
                            });

                            additional.push(StructOrEnum::Enum(e));
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

                match res.0 {
                    StructOrEnum::Struct(s) => match s.fields {
                        Fields::Unit => {}
                        Fields::Named(_) => {
                            todo!()
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
                                    ty: multiply(ty, multipliers),
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
                            ty: multiply(Type::Path(TypePath {
                                qself: None,
                                path: Path::from(e.ident.clone()),
                            }), multipliers),
                        });

                        additional.push(StructOrEnum::Enum(e));
                    }
                }
            }

            SyntaxComponent::Literal { .. } => {}

            SyntaxComponent::Builtin { datatype, multipliers } => {
                better_name.push_str(&datatype.to_case(Case::Pascal));
                
                fields.unnamed.push(Field {
                    attrs: vec![],
                    vis: syn::Visibility::Inherited,
                    mutability: FieldMutability::None,
                    ident: None,
                    colon_token: None,
                    ty: multiply(Type::Path(TypePath {
                        qself: None,
                        path: Path::from(ident(datatype)),
                    }), multipliers),
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
                    variant: syn::Variant {
                        attrs: vec![],
                        ident: id,
                        fields: syn::Fields::Unit,
                        discriminant: None,
                    },
                });
            }
            SyntaxComponent::Inherit { .. } => {
                better_name.push_str("Inherit");
                candidates.push(VariantInfo {
                    base: "Inherit".to_string(),
                    kind: "Inherit".to_string(),
                    args: None,
                    variant: syn::Variant {
                        attrs: vec![],
                        ident: Ident::new("Inherit", Span::call_site()),
                        fields: syn::Fields::Unit,
                        discriminant: None,
                    },
                });
            }
            SyntaxComponent::Initial { .. } => {
                better_name.push_str("Initial");
                candidates.push(VariantInfo {
                    base: "Initial".to_string(),
                    kind: "Initial".to_string(),
                    args: None,
                    variant: syn::Variant {
                        attrs: vec![],
                        ident: Ident::new("Initial", Span::call_site()),
                        fields: syn::Fields::Unit,
                        discriminant: None,
                    },
                });
            }
            SyntaxComponent::Definition { datatype, quoted, multipliers, .. } => {
                let suffix = if *quoted { "" } else if datatype.contains("()") { "Fn" } else { "Def" };
                let id = datatype.to_case(Case::Pascal).replace("()", "");
                better_name.push_str(&id);
                
                let id_def = &format!("{}{suffix}", id);
                let id = Ident::new(&id, Span::call_site());
                let id_def = Ident::new(id_def, Span::call_site());
                
                candidates.push(VariantInfo {
                    base: id.to_string(),
                    kind: suffix.to_string(),
                    args: None,
                    variant: syn::Variant {
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
                                ty: multiply(Type::Path(TypePath {
                                    qself: None,
                                    path: Path::from(id_def),
                                }), multipliers),
                            }]),
                        }),
                        discriminant: None,
                    },
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

pub(crate) fn get_literal(literal: &str) -> String {
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

fn type_to_string(ty: &Type) -> String {
    match ty {
        Type::Path(tp) => tp.path.segments.last().map(|s| s.ident.to_string()).unwrap_or_default(),
        Type::Tuple(tup) => tup.elems.iter().map(type_to_string).collect::<Vec<_>>().join(""),
        _ => format!("{:?}", ty),
    }
}
