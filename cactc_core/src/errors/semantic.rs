use thiserror::Error;
use miette::Diagnostic;

#[derive(Debug, Error, Diagnostic)]
pub enum SemanticError {

}