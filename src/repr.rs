use gosub_css3::matcher::syntax::GroupCombinators;

mod syn;

pub enum CssRepr {
    Function(String, CssTree),
    Sub(String),
    Definition(String),
    Lit(String),
    Group(bool, CssTree),
    Keyword(String, String),
}

pub struct CssItem {
    pub combinator: Multiplier,
    pub repr: CssRepr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Multiplier {
    None,
    Optional,
    ZeroOrMore,
    OneOrMore,
    Between(usize, usize),
    CommaSeparatedRepeat(usize, usize),
}



pub enum CssTypeRepr {
    Enum(Vec<(String, CssTree)>),
    Struct(CssTree),
}

pub struct CssType {
    pub name: String,
    pub id: String,
    pub repr: CssTypeRepr,
}

pub struct CssTree {
    pub(crate) items: Vec<CssItem>,
}