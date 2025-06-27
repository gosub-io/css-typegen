mod syn;

pub enum CssRepr {
    Function(CssTree),
    Sub(String),
    Definition(String),
    Lit(String),
    Group(CssTree),
}

pub struct CssItem {
    pub combinator: Combinator,
    pub repr: CssRepr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Combinator {
    None,
    Optional,
    ZeroOrMore,
    OneOrMore,
    Between(usize, usize),
    CommaSeparatedRepeat(usize, usize),
}



pub enum CssType {
    Enum(Vec<(String, CssTree)>),
    Struct(CssTree),
}

pub struct CssTree {
    pub(crate) items: Vec<CssItem>,
}