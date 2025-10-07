use thiserror::Error;
use miette::Diagnostic;

/// 代码生成阶段可能产生的所有错误的集合。
#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
pub enum CodeGenError {

}