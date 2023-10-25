#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceLocation<'filepath> {
    pub filepath: &'filepath str,
    pub line: usize,
    pub column: usize,
}

#[macro_export]
macro_rules! current_location {
    () => {
        $crate::location::SourceLocation {
            filepath: ::core::file!(),
            line: ::core::line!() as usize,
            column: ::core::column!() as usize,
        }
    };
}
pub use current_location;
