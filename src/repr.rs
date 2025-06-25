


pub enum CssRepr {
    Keyword(String),
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

pub enum Combinator {
    None,
    Optional,
    ZeroOrMore,
    OneOrMore,
    Between(usize, usize),
    CommaSeparatedRepeat(usize, usize),
}



pub enum CssType {
    Enum(Vec<(String, CssItem)>),
    Struct(Vec<CssItem>),
}

pub struct CssTree {
    pub(crate) items: Vec<CssItem>,
}