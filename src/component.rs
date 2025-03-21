mod group;

use gosub_css3::matcher::syntax::SyntaxComponent;

pub fn generate_component_root(component: &SyntaxComponent, name: &str) {
    match component {
        SyntaxComponent::Function { name, multipliers: _, arguments } => {
            println!("Function: {}", name);
            
            if let Some(_arg) = arguments {
                // generate_component(ty, arg);
            }
        }

        SyntaxComponent::Group {components, combinator, multipliers: _ } => {
            let group = group::generate_group(components, combinator.clone(), name);

            println!("{}", group);
        }
        
        _ => {
            println!("Component: {:#?}", component);
        }
    }
    
}


// enum Display {
//     InsideOutside(Option<DisplayOutside>, Option<DisplayInside>)
// }