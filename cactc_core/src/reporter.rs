// 顶层的编译器错误枚举。
/// 所有编译阶段（词法、语法、类型检查等）的错误都会被包含在这里。
#[derive(Debug, Error, Diagnostic)]
pub enum CompilerError {
    /// 词法分析阶段的错误
    #[error(transparent)]
    #[diagnostic(transparent)]
    Lexical(#[from] LexerError),

    /// 语法分析阶段的错误
    #[error(transparent)]
    #[diagnostic(transparent)]
    Parsing(#[from] ParserError),

    /// 语义分析阶段的错误
    #[error(transparent)]
    #[diagnostic(transparent)]
    Semantic(#[from] SemanticError),

    /// 代码生成阶段的错误
    #[error(transparent)]
    #[diagnostic(transparent)]
    CodeGen(#[from] CodeGenError),
}
