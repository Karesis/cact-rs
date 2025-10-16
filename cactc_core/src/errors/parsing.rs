use thiserror::Error;
use crate::span::Span;
use miette::Diagnostic;
use chumsky::error::Rich;
use crate::lexer::Token; // 你的 Token 类型
use std::fmt::Write;

#[derive(Debug, Error, Diagnostic, Clone)]
#[error("Parsing error: {message}")]
pub struct ParsingError {
    /// 核心错误信息
    message: String,

    /// 错误发生的位置，miette 会用它来高亮代码
    #[label]
    span: Span,

    /// (可选) 额外的帮助信息，比如期望的 token 是什么
    #[help]
    help_text: Option<String>,
}

impl<'a> From<Rich<'a, Token, Span>> for ParsingError {
    fn from(err: Rich<'a, Token, Span>) -> Self {
        let mut message = String::new();
        let mut help_text = None;

        // 根据错误的具体原因来构建消息
        match err.reason() {
            chumsky::error::RichReason::ExpectedFound { expected, found } => {
                // `found` token 是什么
                let found_str = found.clone()
                    .map(|t| format!("'{:#?}'", t))
                    .unwrap_or_else(|| "end of input".to_string());
                
                write!(&mut message, "Unexpected {}, ", found_str).unwrap();

                // `expected` 的 token 是什么
                if expected.is_empty() {
                    write!(&mut message, "but nothing was expected here.").unwrap();
                } else {
                    write!(&mut message, "expected one of...").unwrap();
                    let expected_str = expected
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    help_text = Some(format!("Try one of these instead: {}", expected_str));
                }
            }
            chumsky::error::RichReason::Custom(msg) => {
                message = msg.to_string();
            }
        }

        ParsingError {
            message,
            span: (*err.span()).into(), // 将你的 Span 转换成 SourceSpan
            help_text,
        }
    }
}