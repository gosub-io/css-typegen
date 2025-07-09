use gosub_css3::matcher::syntax::{SyntaxComponentMultiplier};
use crate::ident_str;
use crate::multiplier::merge_multipliers;

mod syn;

#[derive(Debug, Clone)]
pub enum CssRepr {
    Function(String, CssTree),
    Sub(String),
    Definition(String),
    Lit(String),
    Group(bool, CssTree),
    Keyword(String, String),
    Tuple(CssTree),
}

#[derive(Debug, Clone)]
pub struct CssItem {
    pub multiplier: Multiplier,
    pub repr: CssRepr,
}

impl CssItem {
    pub fn new(repr: CssRepr) -> Self {
        Self {
            multiplier: Multiplier::None,
            repr,
        }
    }

    pub fn with_multiplier(repr: CssRepr, combinator: Multiplier) -> Self {
        Self {
            multiplier: combinator,
            repr,
        }
    }

    pub fn add_multiplier(&mut self, multiplier: Multiplier) {
        self.multiplier = self.multiplier.merge(multiplier);
    }

    pub fn name(&self) -> String {
        match &self.repr {
            CssRepr::Function(name, _) => name.clone(),
            CssRepr::Sub(name) => name.clone(),
            CssRepr::Definition(name) => name.clone(),
            CssRepr::Lit(name) => name.clone(),
            CssRepr::Group(_, _) => "Group".to_string(),
            CssRepr::Keyword(_, name) => name.clone(),
            CssRepr::Tuple(_) => "Group".to_string(),
        }
    }

}

impl From<CssRepr> for CssItem {
    fn from(repr: CssRepr) -> Self {
        CssItem::new(repr)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum Multiplier {
    #[default]
    None,
    Optional,
    ZeroOrMore,
    OneOrMore,
    Between(usize, usize),
    CommaSeparatedRepeat(usize, usize),
}


impl Multiplier {
    pub fn merge(self, other: Multiplier) -> Self {
        
        
        match (self, other) {
            (Multiplier::None, _) => other,
            (_, Multiplier::None) => self,
            (Multiplier::Optional, _) => other.make_opt(),
            (_, Multiplier::Optional) => self.make_opt(),
            
            (Multiplier::ZeroOrMore, _) => other.make_zero_or_more(),
            (_, Multiplier::ZeroOrMore) => self.make_zero_or_more(),
            
            (Multiplier::OneOrMore, _) => other.make_one_or_more(),
            (_, Multiplier::OneOrMore) => self.make_one_or_more(),
            
            
            (Multiplier::Between(l1, u1), Multiplier::Between(l2, u2)) => {
                Multiplier::Between(l1.max(l2), u1.min(u2))
            }
            (Multiplier::CommaSeparatedRepeat(l1, u1), Multiplier::CommaSeparatedRepeat(l2, u2)) => {
                Multiplier::CommaSeparatedRepeat(l1.max(l2), u1.min(u2))
            }
            
            _ => todo!()
        }
    }

    pub fn make_opt(self) -> Multiplier {
        match  self {
            Multiplier::None => Multiplier::Optional,
            Multiplier::Optional => Multiplier::Optional,
            Multiplier::ZeroOrMore => Multiplier::ZeroOrMore,
            Multiplier::OneOrMore => Multiplier::OneOrMore,
            Multiplier::Between(l, u) => Multiplier::Between(l.saturating_sub(1), u),
            Multiplier::CommaSeparatedRepeat(l, u) => Multiplier::CommaSeparatedRepeat(l.saturating_sub(1), u),
        }
    }
    
    pub fn make_zero_or_more(self) -> Multiplier {
        match self {
            Multiplier::None => Multiplier::ZeroOrMore,
            Multiplier::Optional => Multiplier::ZeroOrMore,
            Multiplier::ZeroOrMore => Multiplier::ZeroOrMore,
            Multiplier::OneOrMore => Multiplier::ZeroOrMore,
            Multiplier::Between(l, u) => Multiplier::Between(0, u),
            Multiplier::CommaSeparatedRepeat(l, u) => Multiplier::CommaSeparatedRepeat(0, u),
        }
    }
    
    pub fn make_one_or_more(self) -> Multiplier {
        match self {
            Multiplier::None => Multiplier::OneOrMore,
            Multiplier::Optional => Multiplier::ZeroOrMore,
            Multiplier::ZeroOrMore => Multiplier::ZeroOrMore,
            Multiplier::OneOrMore => Multiplier::OneOrMore,
            Multiplier::Between(l, u) => Multiplier::Between(l.max(1), u),
            Multiplier::CommaSeparatedRepeat(l, u) => Multiplier::CommaSeparatedRepeat(l.max(1), u),
        }
    }

}

impl From<&[SyntaxComponentMultiplier]> for Multiplier {
    fn from(multipliers: &[SyntaxComponentMultiplier]) -> Self {
        merge_multipliers(multipliers).into()
    }
}

impl From<SyntaxComponentMultiplier> for Multiplier {
    fn from(multiplier: SyntaxComponentMultiplier) -> Self {
        match multiplier {
            SyntaxComponentMultiplier::Once => Multiplier::None,
            SyntaxComponentMultiplier::Optional => Multiplier::Optional,
            SyntaxComponentMultiplier::ZeroOrMore => Multiplier::ZeroOrMore,
            SyntaxComponentMultiplier::OneOrMore => Multiplier::OneOrMore,
            SyntaxComponentMultiplier::Between(l, u) => Multiplier::Between(l, u),
            SyntaxComponentMultiplier::CommaSeparatedRepeat(l, u) => {
                Multiplier::CommaSeparatedRepeat(l, u)
            }
            SyntaxComponentMultiplier::AtLeastOneValue => Multiplier::None,
        }
    }
}


#[derive(Debug, Clone)]
pub enum CssTypeRepr {
    Enum(Vec<(String, CssTree)>),
    Struct(CssTree),
}

impl From<CssTree> for CssTypeRepr {
    fn from(tree: CssTree) -> Self {
        CssTypeRepr::Struct(tree)
    }
}

#[derive(Debug, Clone)]
pub struct CssType {
    pub name: String,
    pub id: String,
    pub repr: CssTypeRepr,
    pub is_aloao: bool,
}

impl CssType {

    pub fn unit(name: String, id: &str) -> Self {
        let id = ident_str(&id);


        Self {
            name,
            id,
            repr: CssTypeRepr::Struct(CssTree { items: Vec::new() }),
            is_aloao: false,
        }

    }

    pub fn new(name: String, id: String, repr: CssTypeRepr) -> Self {
        Self { name, id, repr, is_aloao: false }
    }
    
    pub fn with_aloao(mut self, is_aloao: bool) -> Self {
        self.is_aloao = is_aloao;
        self
    }
}

#[derive(Debug, Clone, Default)]
pub struct CssTree {
    pub(crate) items: Vec<CssItem>,
}

impl CssTree {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn with_items(items: Vec<CssItem>) -> Self {
        Self { items }
    }

    pub fn add_item(&mut self, item: CssItem) {
        self.items.push(item);
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn add_multiplier(self, multiplier: Multiplier) -> Self {
        if multiplier == Multiplier::None {
            return self;
        }

        let repr = CssRepr::Tuple(self);

        let item = CssItem::with_multiplier(repr, multiplier);

        CssTree {
            items: vec![item],
        }
    }
}

impl From<CssItem> for CssTree {
    fn from(item: CssItem) -> Self {
        Self {
            items: vec![item],
        }
    }
}