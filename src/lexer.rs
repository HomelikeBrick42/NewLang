use crate::interning::InternedStr;
use derive_more::{Debug, Display};
use std::{num::NonZeroUsize, str::Chars};

#[derive(Debug, Display, Clone, Copy)]
#[debug("{filepath}:{line}:{column} at byte {position}")]
#[display("{filepath}:{line}:{column}")]
pub struct SourceLocation {
    pub filepath: InternedStr,
    pub position: usize,
    pub line: NonZeroUsize,
    pub column: NonZeroUsize,
}

#[derive(Debug, Display)]
#[display("{kind}")]
pub struct Token {
    pub location: SourceLocation,
    pub kind: TokenKind,
}

#[derive(Debug, Display)]
pub enum TokenKind {
    #[display("{{newline}}")]
    Newline,

    #[display("'{_0}'")]
    Name(InternedStr),
    #[display("{_0}")]
    Integer(u64),

    #[display("_")]
    Placeholder,
    #[display("fn")]
    FnKeyword,
    #[display("type")]
    TypeKeyword,
    #[display("let")]
    LetKeyword,
    #[display("struct")]
    StructKeyword,
    #[display("enum")]
    EnumKeyword,
    #[display("builtin")]
    BuiltinKeyword,

    #[display("#")]
    Hash,
    #[display(":")]
    Colon,
    #[display(",")]
    Comma,
    #[display("=")]
    Equal,

    #[display("(")]
    OpenParenthesis,
    #[display(")")]
    CloseParenthesis,
    #[display("[")]
    OpenBracket,
    #[display("]")]
    CloseBracket,
    #[display("{{")]
    OpenBrace,
    #[display("}}")]
    CloseBrace,

    #[display("+")]
    Plus,
    #[display("-")]
    Minus,
    #[display("*")]
    Asterisk,
    #[display("/")]
    Slash,

    #[display("->")]
    RightArrow,
}

#[derive(Clone)]
pub struct Lexer<'source> {
    pub location: SourceLocation,
    pub chars: Chars<'source>,
}

impl<'source> Lexer<'source> {
    pub fn new(filepath: InternedStr, source: &'source str) -> Self {
        Self {
            location: SourceLocation {
                filepath,
                position: 0,
                line: NonZeroUsize::MIN,
                column: NonZeroUsize::MIN,
            },
            chars: source.chars(),
        }
    }

    pub fn location(&self) -> SourceLocation {
        self.location
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.location.position += c.len_utf8();
        self.location.column = self.location.column.checked_add(1).unwrap();
        if c == '\n' {
            self.location.line = self.location.line.checked_add(1).unwrap();
            self.location.column = NonZeroUsize::MIN;
        }
        Some(c)
    }

    pub fn is_at_eof(&self) -> bool {
        matches!(
            self.clone().next_token(),
            Err(LexerError {
                kind: LexerErrorKind::UnexpectedEOF,
                ..
            })
        )
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        loop {
            let start_location = self.location;
            break Ok(Token {
                location: start_location,
                kind: match self.next_char() {
                    Some(' ' | '\t' | '\r') => continue,

                    Some('\n') => TokenKind::Newline,

                    Some('#') => TokenKind::Hash,
                    Some(':') => TokenKind::Colon,
                    Some(',') => TokenKind::Comma,
                    Some('=') => TokenKind::Equal,

                    Some('(') => TokenKind::OpenParenthesis,
                    Some(')') => TokenKind::CloseParenthesis,
                    Some('[') => TokenKind::OpenBracket,
                    Some(']') => TokenKind::CloseBracket,
                    Some('{') => TokenKind::OpenBrace,
                    Some('}') => TokenKind::CloseBrace,

                    Some('+') => TokenKind::Plus,
                    Some('*') => TokenKind::Asterisk,

                    Some('-') => match self.peek_char() {
                        Some('>') => {
                            self.next_char();
                            TokenKind::RightArrow
                        }

                        _ => TokenKind::Minus,
                    },

                    Some('/') => match self.peek_char() {
                        Some('/') => {
                            while !matches!(self.next_char(), None | Some('\n')) {}
                            continue;
                        }

                        Some('*') => {
                            self.next_char();
                            let mut depth = 1usize;
                            while depth > 0 {
                                match self.next_char() {
                                    None => {
                                        return Err(LexerError {
                                            location: start_location,
                                            kind: LexerErrorKind::UnclosedBlockComment,
                                        });
                                    }

                                    Some('/') if let Some('*') = self.peek_char() => {
                                        self.next_char();
                                        depth += 1;
                                    }

                                    Some('*') if let Some('/') = self.peek_char() => {
                                        self.next_char();
                                        depth -= 1;
                                    }

                                    _ => {}
                                }
                            }
                            continue;
                        }

                        _ => TokenKind::Slash,
                    },

                    Some(c) if c.is_ascii_alphabetic() || c == '_' => {
                        // TODO: maybe improve this by not always heap allocating?
                        let mut name = String::new();
                        name.push(c);
                        while let Some(c) = self.peek_char()
                            && (c.is_ascii_alphanumeric() || c == '_')
                        {
                            self.next_char();
                            name.push(c);
                        }

                        match name.as_str() {
                            "_" => TokenKind::Placeholder,
                            "fn" => TokenKind::FnKeyword,
                            "type" => TokenKind::TypeKeyword,
                            "let" => TokenKind::LetKeyword,
                            "struct" => TokenKind::StructKeyword,
                            "enum" => TokenKind::EnumKeyword,
                            "builtin" => TokenKind::BuiltinKeyword,
                            name => TokenKind::Name(name.into()),
                        }
                    }

                    Some(c) if c.is_ascii_digit() => {
                        let (mut value, base) = match c {
                            '0' => (
                                0,
                                match self.peek_char() {
                                    Some('x') => {
                                        self.next_char();
                                        16
                                    }
                                    Some('d') => {
                                        self.next_char();
                                        10
                                    }
                                    Some('o') => {
                                        self.next_char();
                                        8
                                    }
                                    Some('b') => {
                                        self.next_char();
                                        2
                                    }
                                    _ => 10,
                                },
                            ),
                            '1'..='9' => ((c as u8 - b'0') as u64, 10),
                            _ => unreachable!(),
                        };

                        loop {
                            let digit_location = self.location;
                            let digit_value = match self.peek_char() {
                                Some('_') => {
                                    self.next_char();
                                    continue;
                                }

                                Some(c @ '0'..='9') => {
                                    self.next_char();
                                    c as u8 - b'0'
                                }

                                Some(c @ 'A'..='Z') => {
                                    self.next_char();
                                    (c as u8 - b'A') + 10
                                }

                                Some(c @ 'a'..='z') => {
                                    self.next_char();
                                    (c as u8 - b'a') + 10
                                }

                                _ => break,
                            };

                            if digit_value >= base {
                                while let Some(c) = self.peek_char()
                                    && (c.is_ascii_alphanumeric() || c == '_')
                                {
                                    self.next_char();
                                }
                                return Err(LexerError {
                                    location: digit_location,
                                    kind: LexerErrorKind::DigitTooLarge { digit_value, base },
                                });
                            }

                            if let Some(new_value) = value.checked_mul(base as _)
                                && let Some(new_value) = new_value.checked_add(digit_value as _)
                            {
                                value = new_value;
                            } else {
                                while let Some(c) = self.peek_char()
                                    && (c.is_ascii_alphanumeric() || c == '_')
                                {
                                    self.next_char();
                                }
                                return Err(LexerError {
                                    location: start_location,
                                    kind: LexerErrorKind::IntegerLiteralTooLarge,
                                });
                            };
                        }

                        TokenKind::Integer(value)
                    }

                    Some(c) => {
                        return Err(LexerError {
                            location: start_location,
                            kind: LexerErrorKind::UnexpectedChar(c),
                        });
                    }

                    None => {
                        return Err(LexerError {
                            location: start_location,
                            kind: LexerErrorKind::UnexpectedEOF,
                        });
                    }
                },
            });
        }
    }
}

#[derive(Debug, Display)]
#[display("{location}: {kind}")]
pub struct LexerError {
    pub location: SourceLocation,
    pub kind: LexerErrorKind,
}

#[derive(Debug, Display)]
pub enum LexerErrorKind {
    #[display("Unexpected EOF")]
    UnexpectedEOF,
    #[display("Unexpected character {_0:?}")]
    UnexpectedChar(char),
    #[display("Unclosed block comment")]
    UnclosedBlockComment,
    #[display("Digit (value {digit_value}) is too large for base {base}")]
    DigitTooLarge { digit_value: u8, base: u8 },
    #[display("Integer is too large")]
    IntegerLiteralTooLarge,
}
