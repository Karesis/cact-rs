use thiserror::Error;
use miette::Diagnostic;

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
pub enum SemanticError {

}