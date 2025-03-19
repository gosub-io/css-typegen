


pub enum MultiplierError {
    NotEnoughItems,
    TooManyItems,
}


pub struct ZeroOrMore<T>(pub Vec<T>);
pub struct OneOrMore<T>(pub Vec<T>);
pub struct Between<const L: usize, const U: usize, T>(pub Vec<T>);
// pub struct AtLeastOne //TODO
pub struct CommaSeparatedRepeat<const L: usize, const U: usize, T>(pub Vec<T>);


impl<T> ZeroOrMore<T> {
    pub fn new(items: Vec<T>) -> Self {
        Self(items)
    }
}

impl<T> OneOrMore<T> {
    pub fn new(items: Vec<T>) -> Result<Self, MultiplierError> {
        if items.is_empty() {
            return Err(MultiplierError::NotEnoughItems)
        }
        
        Ok(Self(items))
    }
}


impl<const L: usize, const U: usize, T> Between<L, U, T> {
    pub fn new(items: Vec<T>) -> Result<Self, MultiplierError> {
        if items.len() < L {
            return Err(MultiplierError::NotEnoughItems)
        }
        
        if items.len() > U {
            return Err(MultiplierError::TooManyItems)
        }
        
        Ok(Self(items))
    }
}

impl<const L: usize, const U: usize, T> CommaSeparatedRepeat<L, U, T> {
    pub fn new(items: Vec<T>) -> Result<Self, MultiplierError> {
        if items.len() < L {
            return Err(MultiplierError::NotEnoughItems)
        }
        
        if items.len() > U {
            return Err(MultiplierError::TooManyItems)
        }
        
        Ok(Self(items))
    }
}
