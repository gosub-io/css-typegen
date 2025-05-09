use syn::{Field, Visibility};
use syn::visit_mut::{self, VisitMut};

pub struct ExportAll;

fn public() -> Visibility {
    Visibility::Public(Default::default())
}

impl VisitMut for ExportAll {
    fn visit_field_mut(&mut self, i: &mut Field) {
        i.vis = public();
        visit_mut::visit_field_mut(self, i);
    }

    fn visit_item_enum_mut(&mut self, node: &mut syn::ItemEnum) {
        node.vis = public();
        
        // visit_mut::visit_item_enum_mut(self, node);
    }


    fn visit_item_struct_mut(&mut self, node: &mut syn::ItemStruct) {
        node.vis = public();
        visit_mut::visit_item_struct_mut(self, node);
    }

}