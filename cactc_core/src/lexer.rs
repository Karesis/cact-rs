mod tokens;

use logos::{Logos, SpannedIter};
use crate::errors::CompilerError;
pub use tokens::Token;
use crate::span::Span;
use std::rc::Rc;
use std::sync::Mutex;
use std::sync::Arc;
use std::cell::RefCell;

/// A streaming token iterator that filters out and collects lexical errors.
#[derive(Clone)]
pub struct TokenStream<'s> {
    lexer: SpannedIter<'s, Token>,
    errors: Rc<RefCell<Vec<CompilerError>>>,
}

impl<'s> TokenStream<'s> {
    /// Creates a new TokenStream from a source string.
    pub fn new(source: &'s str, errors: Rc<RefCell<Vec<CompilerError>>>) -> Self {
        Self {
            lexer: Token::lexer(source).spanned(),
            errors
        }
    }
}

impl<'s> Iterator for TokenStream<'s> {
    // The iterator yields valid tokens with their spans.
    type Item = (Token, Span);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // Get the next item from the raw logos lexer.
            let next_item = self.lexer.next();

            match next_item {
                Some((result, span)) => {
                    match result {
                        // This is a valid token. Return it.
                        Ok(token) => return Some((token, span.into())),
                        
                        // This is a lexical error.
                        Err(lex_error) => {
                            // Add it to our error list and continue the loop
                            // to find the next valid token.
                            self.errors.borrow_mut().push(lex_error.into());
                            continue;
                        }
                    }
                },
                // The lexer is exhausted.
                None => return None,
            }
        }
    }
}

pub struct Lexer<'s> {
    stream: TokenStream<'s>,
}

impl<'s> Lexer<'s> {
    /// Creates a new Lexer from a source string.
    pub fn new(source: &'s str, errors:Rc<RefCell<Vec<CompilerError>>>) -> Self {
        Self {
            stream: TokenStream::new(source, errors),
        }
    }

    // --- Streaming Method (for parser) ---

    /// Consumes the Lexer and returns the underlying TokenStream.
    /// The parser is responsible for making this stream peekable if needed.
    pub fn into_stream(self) -> TokenStream<'s> {
        self.stream
    }

    // --- Batch Methods (for convenience) ---

    // /// Consumes the entire lexer and returns all tokens and errors.
    // pub fn collect_all(mut self) -> (Vec<(Token, Span)>, Vec<CompilerError>) {
    //     // First, collect all the valid tokens by iterating through the stream.
    //     // `by_ref()` allows us to drain the iterator while keeping `self.stream`.
    //     let tokens: Vec<_> = self.stream.by_ref().collect();
        
    //     // The TokenStream has now been iterated, and all errors have been
    //     // gathered internally. We can now retrieve them.
    //     let errors = self.stream.into_errors().to_vec();
        
    //     (tokens, errors)
    // }

    // /// Consumes the lexer and returns just the valid tokens.
    // pub fn into_tokens(self) -> Vec<(Token, Span)> {
    //     self.collect_all().0
    // }
    
    // /// Consumes the lexer and returns just the errors found.
    // pub fn into_errors(self) -> Vec<CompilerError> {
    //     self.collect_all().1
    // }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Result;
    use std::{env, fs};

    // #[test]
    // fn test_lexer_all_through() -> Result<()>{
    //     match env::current_dir() {
    //         Ok(path) => {
    //             println!("[DEBUG]The current dir is: {path:?}");
    //         }
    //         Err(e) => {
    //             println!("[DEGUG]Error when getting the current dir: {e}");
    //         }
    //     }
    //     let source_file = fs::read_to_string("../tests/all_tests_in.cact")?;
    //     let source_codes = source_file.as_str();
    //     let lexer = Lexer::new(source_codes);
    //     let tokens_and_errors = lexer.collect_all();
    //     println!("{tokens_and_errors:?}");
    //     Ok(())
    // }
}
