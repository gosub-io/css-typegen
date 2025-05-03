use proc_macro2::Span;
use syn::{visit_mut, Ident, ItemEnum, ItemStruct, TypePath};
use syn::visit_mut::VisitMut;

pub struct Renamer {
    rename_root: Vec<(Ident, Ident)>,
    rename_variant: Vec<((Ident, Ident), Ident)>,
}

impl Renamer {
    pub fn new(
        root_map: &[(&str, &str)],
        variant_map: &[((&str, &str), &str)],
    ) -> Self {
        let rename_root = root_map
            .iter()
            .map(|(o, n)| {
                (Ident::new(o, Span::call_site()), Ident::new(n, Span::call_site()))
            })
            .collect();
        let rename_variant = variant_map
            .iter()
            .map(|((e, v), nv)| {
                (
                    (
                        Ident::new(e, Span::call_site()),
                        Ident::new(v, Span::call_site()),
                    ),
                    Ident::new(nv, Span::call_site()),
                )
            })
            .collect();
        Renamer {
            rename_root,
            rename_variant,
        }
    }
}

impl VisitMut for Renamer {
    fn visit_item_enum_mut(&mut self, node: &mut ItemEnum) {
        // rename enum name
        for (old, new) in &self.rename_root {
            if node.ident == *old {
                node.ident = new.clone();
            }
        }
        // rename specific variants
        for var in &mut node.variants {
            for ((enm, ov), nv) in &self.rename_variant {
                if &node.ident == enm && var.ident == *ov {
                    var.ident = nv.clone();
                }
            }
        }
        visit_mut::visit_item_enum_mut(self, node);
    }

    fn visit_item_struct_mut(&mut self, node: &mut ItemStruct) {
        // rename struct name
        for (old, new) in &self.rename_root {
            if node.ident == *old {
                node.ident = new.clone();
            }
        }
        visit_mut::visit_item_struct_mut(self, node);
    }

    fn visit_type_path_mut(&mut self, node: &mut TypePath) {
        // rename all TypePath segments
        for seg in &mut node.path.segments {
            for (old, new) in &self.rename_root {
                if seg.ident == *old {
                    seg.ident = new.clone();
                }
            }
        }
        visit_mut::visit_type_path_mut(self, node);
    }
}
