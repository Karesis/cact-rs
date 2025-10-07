use logos::{Lexer, Logos};
use std::fmt::{self, Display, Formatter};

fn parse_int(lex: &mut Lexer<Token>) -> Result<i32, &'static str> {
    let slice = lex.slice();
    if let Some(s) = slice.strip_prefix("0x").or_else(|| slice.strip_prefix("0X")) {
        //  Hexadecimal
        i32::from_str_radix(s, 16).map_err(|_| "Failed to parse hex integer")
    } else if slice.starts_with('0') && slice.len() > 1 {
        // Octal
        i32::from_str_radix(&slice[1..], 8).map_err(|_| "Failed to parse octal integer")
    } else {
        // Decimal
        slice.parse().map_err(|_| "Failed to parse decimal integer")
    }
}

fn parse_float(lex: &mut Lexer<Token>) -> Result<f32, &'static str> {
    let slice = lex.slice();
    // delete f/F endien
    slice[..slice.len() - 1].parse().map_err(|_| "Failed to parse float")
}

fn parse_double(lex: &mut Lexer<Token>) -> Result<f64, &'static str> {
    lex.slice().parse().map_err(|_| "Failed to parse double")
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(error = &'static str)]
pub enum Token {
    // --- Keywords  ---
    #[token("const")]
    Const,
    #[token("int")]
    IntType,
    #[token("bool")]
    BoolType,
    #[token("float")]
    FloatType,
    #[token("double")]
    DoubleType,
    #[token("void")]
    Void,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("return")]
    Return,

    // --- Literals ---
    
    // boolean
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    BoolConst(bool),

    // float
    #[regex(r"([0-9]+\.[0-9]*|\.[0-9]+)([eE][+-]?[0-9]+)?[fF]|[0-9]+[eE][+-]?[0-9]+[fF]", parse_float)]
    FloatConst(f32),
    
    // double
    #[regex(r"([0-9]+\.[0-9]*|\.[0-9]+)([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+", parse_double)]
    DoubleConst(f64),

    // integer
    #[regex(r"0[xX][0-9a-fA-F]+|0[0-7]+|[0-9]+", parse_int)]
    IntConst(i32),

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    // --- Operators & Delimiters ---
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("=")]
    Assign,
    #[token("==")]
    Eq,
    #[token("!=")]
    Ne,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("!")]
    Not,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,

    // --- Comments & Whitespace ---
    #[regex(r"//[^\n]*", logos::skip)] // one-line comment
    #[regex(r"/\*([^*]|\*[^/])*\*/", logos::skip)] // multi-line comment
    #[regex(r"[ \t\n\f\r]+", logos::skip)] // 空白符 [cite: 31]
    Whitespace,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_full() {
        let input = r#"
            // CACT Program Example
            const int a = 10;
            float pi = 3.14f;
            double large_pi = 3.1415926;
            
            int main() {
                if (a > 0) {
                    return 0; /* exit */
                }
                return 1;
            }
        "#;

        let mut lexer = Token::lexer(input);

        // --- const int a = 10; ---
        assert_eq!(lexer.next(), Some(Ok(Token::Const)));
        assert_eq!(lexer.next(), Some(Ok(Token::IntType)));
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("a".to_string()))));
        assert_eq!(lexer.next(), Some(Ok(Token::Assign)));
        assert_eq!(lexer.next(), Some(Ok(Token::IntConst(10))));
        assert_eq!(lexer.next(), Some(Ok(Token::Semicolon)));

        // --- float pi = 3.14f; ---
        assert_eq!(lexer.next(), Some(Ok(Token::FloatType)));
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("pi".to_string()))));
        assert_eq!(lexer.next(), Some(Ok(Token::Assign)));
        assert_eq!(lexer.next(), Some(Ok(Token::FloatConst(3.14f32))));
        assert_eq!(lexer.next(), Some(Ok(Token::Semicolon)));
        
        // --- double large_pi = 3.1415926; ---
        assert_eq!(lexer.next(), Some(Ok(Token::DoubleType)));
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("large_pi".to_string()))));
        assert_eq!(lexer.next(), Some(Ok(Token::Assign)));
        assert_eq!(lexer.next(), Some(Ok(Token::DoubleConst(3.1415926f64))));
        assert_eq!(lexer.next(), Some(Ok(Token::Semicolon)));

        // --- int main() { ---
        assert_eq!(lexer.next(), Some(Ok(Token::IntType)));
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("main".to_string()))));
        assert_eq!(lexer.next(), Some(Ok(Token::LParen)));
        assert_eq!(lexer.next(), Some(Ok(Token::RParen)));
        assert_eq!(lexer.next(), Some(Ok(Token::LBrace)));

        // --- if (a > 0) { ---
        assert_eq!(lexer.next(), Some(Ok(Token::If)));
        assert_eq!(lexer.next(), Some(Ok(Token::LParen)));
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("a".to_string()))));
        assert_eq!(lexer.next(), Some(Ok(Token::Gt)));
        assert_eq!(lexer.next(), Some(Ok(Token::IntConst(0))));
        assert_eq!(lexer.next(), Some(Ok(Token::RParen)));
        assert_eq!(lexer.next(), Some(Ok(Token::LBrace)));
        
        // --- return 0; --- (block comment is skipped)
        assert_eq!(lexer.next(), Some(Ok(Token::Return)));
        assert_eq!(lexer.next(), Some(Ok(Token::IntConst(0))));
        assert_eq!(lexer.next(), Some(Ok(Token::Semicolon)));

        // --- } ---
        assert_eq!(lexer.next(), Some(Ok(Token::RBrace)));

        // --- return 1; ---
        assert_eq!(lexer.next(), Some(Ok(Token::Return)));
        assert_eq!(lexer.next(), Some(Ok(Token::IntConst(1))));
        assert_eq!(lexer.next(), Some(Ok(Token::Semicolon)));

        // --- } ---
        assert_eq!(lexer.next(), Some(Ok(Token::RBrace)));

        // --- End of input ---
        // ensure no more tokens
        assert_eq!(lexer.next(), None);
    }
}