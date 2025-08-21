use heck::*;

/// The `Case` trait defines methods for checking the naming convention of identifiers.
///
/// NB: It's already implemented in ``String`` and ``&str`` by default, so you don't need to implement it yourself.
pub trait Case {
    /// Checks if an identifier follows the snake_case naming convention.
    ///
    /// # Returns
    ///
    /// `true` if the identifier is in snake_case, `false` otherwise.
    fn is_snake_case(&self) -> bool;
    /// Checks if an identifier follows the SHOUTY_SNAKE_CASE naming convention.
    ///
    /// # Returns
    ///
    /// `true` if the identifier is in SHOUTY_SNAKE_CASE, `false` otherwise.
    fn is_shouty_snake_case(&self) -> bool;
    /// Checks if an identifier follows the PascalCase naming convention.
    ///
    /// # Returns
    ///
    /// `true` if the identifier is in PascalCase, `false` otherwise.
    fn is_pascal_case(&self) -> bool;
    /// Checks if an identifier follows the camelCase naming convention.
    ///
    /// # Returns
    ///
    /// `true` if the identifier is in camelCase, `false` otherwise.
    fn is_camel_case(&self) -> bool;
}

impl Case for String {
    #[inline(always)]
    fn is_snake_case(&self) -> bool {
        self.to_snake_case() == *self
    }

    #[inline(always)]
    fn is_shouty_snake_case(&self) -> bool {
        self.to_shouty_snake_case() == *self
    }

    #[inline(always)]
    fn is_pascal_case(&self) -> bool {
        self.to_pascal_case() == *self
    }

    #[inline(always)]
    fn is_camel_case(&self) -> bool {
        self.to_lower_camel_case() == *self
    }
}

impl Case for &str {
    #[inline(always)]
    fn is_snake_case(&self) -> bool {
        self.to_snake_case() == *self
    }

    #[inline(always)]
    fn is_shouty_snake_case(&self) -> bool {
        self.to_shouty_snake_case() == *self
    }

    #[inline(always)]
    fn is_pascal_case(&self) -> bool {
        self.to_pascal_case() == *self
    }

    #[inline(always)]
    fn is_camel_case(&self) -> bool {
        self.to_lower_camel_case() == *self
    }
}
