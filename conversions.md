
# Conversions from a SyntaxComponent to a typed representation



- GenericKeyword: either empty enum variant or with multipliers
- Property: ???
- Function: name => arguments again in enum variant (with multipliers)
- Definition: ???
- Inherit: enum variant (with multipliers)
- Initial: enum variant (with multipliers)
- Unset: enum variant (with multipliers)
- Literal: drop (just for parsing)
- Value: It seems like it can only be CssValue::Zero or CssValue::Unit
- Group: Force new variant => depending on combinator all subcomponents in one variant or in multiple
- Unit: enum variant with value, unit enum (made from units strings) and multiplier
- Builtin: builtin data type