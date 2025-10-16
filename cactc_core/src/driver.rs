use bumpalo::Bump;
use logos::Logos;
use crate::ast::AstArena;
use crate::errors::{CompilerError};
use crate::lexer::Lexer;
use crate::parser::parse;
use std::sync::mpsc;
use std::thread;
use std::rc::Rc;
use std::cell::RefCell;
use rayon::prelude::*;
use std::mem::ManuallyDrop;

#[derive(Logos, Debug, PartialEq)]
enum BoundaryToken {
    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token(";")]
    Semicolon,

    // 跳过所有明确的、我们不关心的模式（注释、字符串、空白）
    #[regex(r"//[^\n]*", logos::skip)]
    #[regex(r"/\*([^*]|\*[^/])*\*/", logos::skip)]
    #[regex(r#""([^"\\]|\\.)*""#, logos::skip)]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Ignored,

    #[logos(skip)]
    Error,
}

/// 阶段一：扫描源代码，返回顶层结构的源码切片 Vec<&str>
pub fn scan_for_top_level_chunks(source: &str) -> Vec<&str> {
    let mut chunks = Vec::new();
    let mut lexer = BoundaryToken::lexer(source).spanned();
    let mut brace_level = 0;
    let mut last_chunk_end = 0;

    while let Some((token, span)) = lexer.next() {
        match token {
            Ok(BoundaryToken::LBrace) => brace_level += 1,
            Ok(BoundaryToken::RBrace) => {
                if brace_level > 0 {
                    brace_level -= 1;
                    if brace_level == 0 {
                        // 函数定义结束
                        let chunk = &source[last_chunk_end..span.end];
                        chunks.push(chunk.trim());
                        last_chunk_end = span.end;
                    }
                }
            }
            Ok(BoundaryToken::Semicolon) => {
                if brace_level == 0 {
                    // 全局声明结束
                    let chunk = &source[last_chunk_end..span.end];
                    chunks.push(chunk.trim());
                    last_chunk_end = span.end;
                }
            }
            _ => {} // 忽略其他
        }
    }
    chunks
}

pub fn driver(source: &str) -> Vec<CompilerError> {
    let chunks = scan_for_top_level_chunks(source);
    let (tx, rx) = mpsc::channel::<CompilerError>();

    // 启动收集器线程
    let collector_handle = thread::spawn(move || {
        rx.iter().collect()
    });

    // 并行迭代器现在变得非常简单
    chunks.into_par_iter().for_each_with(tx, |error_sender, chunk| {
        // 调用我们的辅助函数，它保证了内部的借用不会泄露出来
        let (lexical_errors, parsing_errors) = parse_chunk(chunk);

        // 发送词法错误
        for err in lexical_errors {
            if error_sender.send(err.into()).is_err() {
                return;
            }
        }
        
        // 发送语法错误
        for err in parsing_errors {
            if error_sender.send(err).is_err() {
                return;
            }
        }
    });

    // 这里无需 drop(tx)，因为它在 for_each_with 结束后会自动被销毁，从而关闭通道
    let all_errors = collector_handle.join().expect("Collector thread panicked!");
    all_errors
}

fn parse_chunk(chunk: &str) -> (Vec<CompilerError>, Vec<CompilerError>) {
    let bump = Bump::new();
    let mut arena = ManuallyDrop::new(AstArena::new_in(&bump));

    // --- 词法分析 ---
    let lexical_errors_rc: Rc<RefCell<Vec<CompilerError>>> = Rc::new(RefCell::new(Vec::new()));
    let lexer = Lexer::new(chunk, Rc::clone(&lexical_errors_rc));
    let token_stream = lexer.into_stream();

    // --- 语法分析 ---

    // ======================== 关键改动在这里 ========================
    // 1. 将可变引用转换为原始指针，这会打破借用检查器的分析链。
    let arena_ptr: *mut _ = &mut *arena;

    // 2. 在一个 unsafe 块中，我们从原始指针创建一个新的、生命周期更短的可变引用。
    //    我们向编译器保证，在这一刻，这是对 arena 数据的唯一访问方式。
    let (ast_result, parsing_errors) = unsafe {
        parse(token_stream, chunk, &mut *arena_ptr)
    };
    // =============================================================

    // --- 获取词法错误 ---
    let lexical_errors = Rc::try_unwrap(lexical_errors_rc)
        .expect("RC should have only one owner now")
        .into_inner();
    
    // 销毁 AST，解除它对 arena 的所有借用
    drop(ast_result);
    
    // 手动销毁 arena，因为我们知道所有借用都已结束
    unsafe {
        ManuallyDrop::drop(&mut arena);
    }
    
    // bump 会在函数末尾被正常销毁，回收所有内存
    (lexical_errors, parsing_errors)
}