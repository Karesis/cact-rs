use thiserror::Error;
use miette::{Diagnostic, SourceSpan};
use crate::lexer::Token;
use logos::Lexer;

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq, Default)]
#[diagnostic(
    // a code for all lexical errors
    code(lexer::error)
)]
pub enum LexicalError {
    #[default]
    #[error("Unknow Error")]
    #[diagnostic(help("Unknow error occured while lexing"))]
    LexingPanic,

    #[error("Invalid integer literal")]
    #[diagnostic(
        help("Integer literals can be decimal (123), hexadecimal (0xAF), or octal (0755).")
    )]
    InvalidIntegerLiteral {
        #[source]
        source: std::num::ParseIntError,
        #[label("This integer is malformed")]
        span: SourceSpan,
    },

    #[error("Invalid float literal")]
    #[diagnostic(
        help("Float literals must be valid floating-point numbers, like '3.14f' or '1.0e-5f'.")
    )]
    InvalidFloatLiteral {
        #[source]
        source: std::num::ParseFloatError,
        #[label("This float is malformed")]
        span: SourceSpan,
    },

    #[error("Invalid double literal")]
    #[diagnostic(
        help("Double literals must be valid floating-point numbers, like '3.14159' or '1.0e10'.")
    )]
    InvalidDoubleLiteral {
        #[source]
        source: std::num::ParseFloatError,
        #[label("This double is malformed")]
        span: SourceSpan,
    },

    #[error("Unexpected character")]
    #[diagnostic(help("The character '{bad_char}' is not recognized here."))]
    UnexpectedCharacter {
        bad_char: char,
        #[label("Unexpected character")]
        span: SourceSpan,
    },
}

/// for the most cases
impl LexicalError {
    pub fn lex_error_handler(lex: &mut Lexer<Token>) -> LexicalError {
        // Extract the character and the span
        let bad_char = lex.slice().chars().next().unwrap_or('?');
        let span = lex.span().into();

        // Skip the character to avoid an infinite loop
        lex.bump(1);

        LexicalError::UnexpectedCharacter { bad_char, span }
    }
}