use convert_case::{Case, Casing};
use css_typegen::component::group::StructOrEnum;
use css_typegen::component::{generate_component_root};
use css_typegen::{builtins, ident};
use gosub_css3::matcher::property_definitions::{get_css_properties, get_css_values, SyntaxType};
use indexmap::IndexSet;
use syn::__private::ToTokens;
use syn::visit_mut::VisitMut;
use css_typegen::exporter::ExportAll;
use css_typegen::renamer::Renamer;

const RENAME_ROOT: &[(&str, &str)] = &[
    ("BgPositionDef", "BgPosition"),
    ("CursorGroup1", "CursorType"),
    ("XLowLowMediumHighXHigh", "VoicePitch")
];
const RENAME_VARIANT: &[((&str, &str), &str)] = &[
    (("BgPosition", "BgPositionDefGroup1"), "LeftRightTopBottonCenter"),
    (("ColorScheme", "ColorSchemeGroup0"), "LightDark"),
    (("StrokeDashJustify", "StrokeDashJustifyGroup0"), "StretchCompress"),
    (("VoiceRange", "VoiceRangeGroup1"), "PitchFrequency")
];

fn main() {
    let props = get_css_properties();
    let values = get_css_values();
    
    let mut items = IndexSet::new();

    for def in props {
        if def.1.syntax.components.is_empty() {
            continue;
        }

        let Some(res) = generate_component_root(&def.1.syntax.components[0], &def.0) else {
            continue;
        };
        
        for item in res.1 {
            items.insert(item);
        }

        items.insert(res.0);
    }

    for def in values {
        // break;
        if def.1.syntax.components.is_empty() {
            continue;
        }

        let name = if def.0.contains("()") {format!("{}Fn", def.0.to_case(Case::Pascal))} else {format!("{}Def", def.0.to_case(Case::Pascal))};

        let Some(mut res) = generate_component_root(&def.1.syntax.components[0], &name) else {
            continue;
        };

        for item in res.1 {
            items.insert(item);
        }

        if def.1.ty == SyntaxType::Definition {
            let name = &format!("{}Def", def.0.to_case(Case::Pascal));
            let name = ident(name);

            match &mut res.0 {
                StructOrEnum::Struct(s) => {
                    s.ident = name
                }
                StructOrEnum::Enum(e) => {
                    e.ident = name
                }
            }
        }

        items.insert(res.0);
    }
    
    let mut output = builtins::get();
    
    for item in items {
        item.to_tokens(&mut output);
    }

    std::fs::write("out_raw.rs", output.to_string()).unwrap();

    let mut file: syn::File = syn::parse2(output).unwrap();
    
    let mut renamer = Renamer::new(RENAME_ROOT, RENAME_VARIANT);
    renamer.visit_file_mut(&mut file);
    
    ExportAll.visit_file_mut(&mut file);

    let out = prettyplease::unparse(&file);

    std::fs::write("out.rs", out).unwrap();
}
