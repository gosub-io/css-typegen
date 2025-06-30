mod syn;

pub enum CssRepr {
    Function(String, CssTree),
    Sub(String),
    Definition(String),
    Lit(String),
    Group(CssTree),
    Keyword(String, String),
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