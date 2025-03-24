use convert_case::{Case, Casing};
use gosub_css3::stylesheet::CssValue;

pub fn value_to_ident(val: &CssValue) -> String {
    match val {
        CssValue::Unit(val, unit) => {
            format!("{}{}", unit.to_case(Case::Pascal), *val as i32)
        }
        CssValue::String(val) => {
            val.to_case(Case::Pascal)
        }
        CssValue::None => {
            "None".to_string()
        }
        CssValue::Inherit => {
            "Inherit".to_string()
        }
        CssValue::Initial => {
            "Initial".to_string()
        }
        CssValue::Zero => {
            "Zero".to_string()
        }

        _ => unimplemented!()


    }
}
