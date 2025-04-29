use std::mem;
use gosub_css3::matcher::syntax::SyntaxComponentMultiplier;
use proc_macro2::{Ident, TokenStream};
use syn::__private::ToTokens;
use syn::{FieldsUnnamed, Type};
use syn::parse::Parse;

pub enum MultiplierError {
    NotEnoughItems,
    TooManyItems,
}


pub struct ZeroOrMore<T>(pub Vec<T>);
pub struct OneOrMore<T>(pub Vec<T>);
pub struct Between<const L: usize, const U: usize, T>(pub Vec<T>);
// pub struct AtLeastOne //TODO
pub struct CommaSeparatedRepeat<const L: usize, const U: usize, T>(pub Vec<T>);


impl<T> ZeroOrMore<T> {
    pub fn new(items: Vec<T>) -> Self {
        Self(items)
    }
}

impl<T> OneOrMore<T> {
    pub fn new(items: Vec<T>) -> Result<Self, MultiplierError> {
        if items.is_empty() {
            return Err(MultiplierError::NotEnoughItems)
        }
        
        Ok(Self(items))
    }
}


impl<const L: usize, const U: usize, T> Between<L, U, T> {
    pub fn new(items: Vec<T>) -> Result<Self, MultiplierError> {
        if items.len() < L {
            return Err(MultiplierError::NotEnoughItems)
        }
        
        if items.len() > U {
            return Err(MultiplierError::TooManyItems)
        }
        
        Ok(Self(items))
    }
}

impl<const L: usize, const U: usize, T> CommaSeparatedRepeat<L, U, T> {
    pub fn new(items: Vec<T>) -> Result<Self, MultiplierError> {
        if items.len() < L {
            return Err(MultiplierError::NotEnoughItems)
        }
        
        if items.len() > U {
            return Err(MultiplierError::TooManyItems)
        }
        
        Ok(Self(items))
    }
}

fn merge_multipliers(multipliers: &[SyntaxComponentMultiplier]) -> SyntaxComponentMultiplier {

    match multipliers.len() {
        0 => return SyntaxComponentMultiplier::Once,
        1 => return multipliers[0],
        2 => {
            if multipliers[0] == multipliers[1] {
                return multipliers[0];
            }

            if multipliers.contains(&SyntaxComponentMultiplier::Optional) {
                let non_optional = multipliers.iter().find(|&&m| m != SyntaxComponentMultiplier::Optional).unwrap();

                return match non_optional {
                    SyntaxComponentMultiplier::Once => SyntaxComponentMultiplier::Optional,
                    SyntaxComponentMultiplier::ZeroOrMore => SyntaxComponentMultiplier::ZeroOrMore,
                    SyntaxComponentMultiplier::OneOrMore => SyntaxComponentMultiplier::ZeroOrMore,
                    SyntaxComponentMultiplier::Between(l, u) => {
                        if *l == 0 {
                            return SyntaxComponentMultiplier::Between(0, *u);
                        }
                        if *l == 1 {
                            return SyntaxComponentMultiplier::Between(0, *u);
                        }
                        unimplemented!("Handle between with optional: {:?}", multipliers);
                        // return SyntaxComponentMultiplier::Between(0, *u); This is not correct!
                    },
                    SyntaxComponentMultiplier::CommaSeparatedRepeat(l, u) => {
                        if *l == 0 {
                            return SyntaxComponentMultiplier::CommaSeparatedRepeat(0, *u);
                        }
                        if *l == 1 {
                            return SyntaxComponentMultiplier::CommaSeparatedRepeat(0, *u);
                        }
                        unimplemented!("Handle comma separated repeat with optional {:?}", multipliers);
                        // return SyntaxComponentMultiplier::CommaSeparatedRepeat(0, *u); This is not correct!
                    },
                    SyntaxComponentMultiplier::AtLeastOneValue => SyntaxComponentMultiplier::Once,
                    SyntaxComponentMultiplier::Optional => SyntaxComponentMultiplier::Optional,

                };
            }
            
            if multipliers.contains(&SyntaxComponentMultiplier::OneOrMore) {
                if let Some(other) = multipliers.iter().find(|&&m| {
                    match m {
                        SyntaxComponentMultiplier::Between(f, _) if f == 1 => true,
                        SyntaxComponentMultiplier::CommaSeparatedRepeat(f, _) if f == 1 => true,
                        _ => false
                    }
                    
                }) {
                    return *other;
                }
                
            }

            unimplemented!("Cannot handle multipliers: {:?}", multipliers);
        },
        3 => {
            if multipliers[0] == multipliers[1] && multipliers[1] == multipliers[2] {
                return multipliers[0];
            }

            if multipliers.contains(&SyntaxComponentMultiplier::OneOrMore) {
                if let Some(m) = multipliers.iter().find(|m| matches!(m, SyntaxComponentMultiplier::CommaSeparatedRepeat(_, _))) {
                    if multipliers.contains(&SyntaxComponentMultiplier::Optional) {
                        return match m {
                            SyntaxComponentMultiplier::Once => SyntaxComponentMultiplier::Optional,
                            SyntaxComponentMultiplier::ZeroOrMore => SyntaxComponentMultiplier::ZeroOrMore,
                            SyntaxComponentMultiplier::OneOrMore => SyntaxComponentMultiplier::ZeroOrMore,
                            SyntaxComponentMultiplier::Between(l, u) => {
                                if *l == 0 {
                                    return SyntaxComponentMultiplier::Between(0, *u);
                                }
                                if *l == 1 {
                                    return SyntaxComponentMultiplier::Between(0, *u);
                                }
                                unimplemented!("Handle between with optional");
                                // return SyntaxComponentMultiplier::Between(0, *u); This is not correct!
                            },
                            SyntaxComponentMultiplier::CommaSeparatedRepeat(l, u) => {
                                if *l == 0 {
                                    return SyntaxComponentMultiplier::CommaSeparatedRepeat(0, *u);
                                }
                                if *l == 1 {
                                    return SyntaxComponentMultiplier::CommaSeparatedRepeat(0, *u);
                                }
                                unimplemented!("Handle comma separated repeat with optional");
                                // return SyntaxComponentMultiplier::CommaSeparatedRepeat(0, *u); This is not correct!
                            },
                            SyntaxComponentMultiplier::AtLeastOneValue => SyntaxComponentMultiplier::Once,
                            SyntaxComponentMultiplier::Optional => SyntaxComponentMultiplier::Optional,
                        };
                    }
                }
            }
        },
        _ => unimplemented!("Handle more than 3 multipliers")
    }

    unimplemented!()
}
pub fn multiply(mut ty: Type, multipliers: &[SyntaxComponentMultiplier]) -> Type {
    let m = merge_multipliers(multipliers);
    
    match ty {
        Type::Paren(paren) => {
            ty = *paren.elem;
        },
        
        Type::Tuple(tuple) if tuple.elems.len() == 1 => {
            ty = tuple.elems.into_iter().next().unwrap();
        }
        
        _ => {}
    }
    
    match m {
        SyntaxComponentMultiplier::Once => ty,
        SyntaxComponentMultiplier::Optional => {
            apply_multiplier_internal("Option", ty, None, false)
        },
        SyntaxComponentMultiplier::ZeroOrMore => {
            apply_multiplier_internal("ZeroOrMore", ty, None, false)
        },

        SyntaxComponentMultiplier::OneOrMore => {
            apply_multiplier_internal("OneOrMore", ty, None, false)
        },

        SyntaxComponentMultiplier::Between(l, u) => {
            apply_multiplier_internal("Between", ty, Some((l, u)), false)
        },

        SyntaxComponentMultiplier::CommaSeparatedRepeat(l, u) => {
            apply_multiplier_internal("CommaSeparatedRepeat", ty, Some((l, u)), false)
        },

        SyntaxComponentMultiplier::AtLeastOneValue => ty, //TODO: handle this case!
    }
}

pub fn multiply_fields(items: FieldsUnnamed, multipliers: &[SyntaxComponentMultiplier]) -> FieldsUnnamed {
    
    if items.unnamed.len() == 1 {
        let f = items.unnamed.into_iter().next().unwrap();
        
        let tok = multiply(f.ty, multipliers);
        
        return syn::parse_quote! {
            (#tok)
        }
    }
    
    
    let m = merge_multipliers(multipliers);

    match m {
        SyntaxComponentMultiplier::Once => syn::parse2(items.into_token_stream()).unwrap(),
        SyntaxComponentMultiplier::Optional => {
            apply_multiplier_internal("Option", items, None, true)
        },
        SyntaxComponentMultiplier::ZeroOrMore => {
            apply_multiplier_internal("ZeroOrMore", items, None, true)
        },

        SyntaxComponentMultiplier::OneOrMore => {
            apply_multiplier_internal("OneOrMore", items, None, true)
        },

        SyntaxComponentMultiplier::Between(l, u) => {
            apply_multiplier_internal("Between", items, Some((l, u)), true)
        },

        SyntaxComponentMultiplier::CommaSeparatedRepeat(l, u) => {
            apply_multiplier_internal("CommaSeparatedRepeat", items, Some((l, u)), true)
        },

        SyntaxComponentMultiplier::AtLeastOneValue => syn::parse2(items.into_token_stream()).unwrap() //TODO: handle this case!
    }
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