mod codegen;
mod lexical;
mod parsing;
mod semantic;

use codegen::CodeGenError;
use lexical::LexicalError;
use parsing::ParsingError;
use semantic::SemanticError;
use thiserror::Error;
use miette::Diagnostic;

/// All errors
#[derive(Debug, Error, Diagnostic)]
pub enum CompilerError {
    /// lexer
    #[error(transparent)]
    #[diagnostic(transparent)]
    Lexical(#[from] LexicalError),

    /// parser
    #[error(transparent)]
    #[diagnostic(transparent)]
    Parsing(#[from] ParsingError),

    /// analyzer
    #[error(transparent)]
    #[diagnostic(transparent)]
    Semantic(#[from] SemanticError),

    /// codegen
    #[error(transparent)]
    #[diagnostic(transparent)]
    CodeGen(#[from] CodeGenError),
}
