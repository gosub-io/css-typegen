use gosub_css3::matcher::property_definitions::get_css_definitions;
use css_typegen::component::generate_component_root;

fn main() {
    let defs = get_css_definitions();

    let width = defs.properties.get("width").unwrap();

    generate_component_root(&width.syntax.components[0], "Width");
}

