use std::fmt;
use position::*;

#[derive(Clone, Debug)]
pub enum Token {
    KeywordLet,
    KeywordIf,
    KeywordElse,
    KeywordDef,
    KeywordFn,
    LBrace,
    RBrace,
    LParen,
    RParen,
    Comma,
    Colon,
    Equals,
    Plus,
    Minus,
    Star,
    Slash,
    Newline,
    Eof,
    Ident(String),
    Integer(i32),
}

#[derive(Copy, Clone, Debug)]
enum LexerState {
    Start,
    Ident,
    Integer,
}

pub type TokenContainer = PositionContainer<Token>;

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Token::*;
        match self {
            &KeywordLet => write!(f, "let"),
            &KeywordIf => write!(f, "if"),
            &KeywordElse => write!(f, "else"),
            &KeywordDef => write!(f, "def"),
            &KeywordFn => write!(f, "fn"),
            &LBrace => write!(f, "lbrace"),
            &RBrace => write!(f, "rbrace"),
            &LParen => write!(f, "lparen"),
            &RParen => write!(f, "rparen"),
            &Comma => write!(f, "comma"),
            &Colon => write!(f, "colon"),
            &Equals => write!(f, "equals"),
            &Plus => write!(f, "plus"),
            &Minus => write!(f, "minus"),
            &Star => write!(f, "star"),
            &Slash => write!(f, "slash"),
            &Newline => write!(f, "newline"),
            &Eof => write!(f, "EOF"),
            &Ident(ref name) => write!(f, "identifier({})", name),
            &Integer(x) => write!(f, "integer({})", x),
        }
    }
}

pub fn lex_source(source: String, file: Option<String>) -> Vec<TokenContainer> {
    let mut token = String::new();
    let mut state = LexerState::Start;
    let mut tokens = Vec::new();
    let mut position = Position::new(1, 1, file);
    let mut range = PositionRange::new(position.clone(), position.clone());
    let mut iter = source.chars();
    let mut c = iter.next().unwrap();
    let mut advance = false;
    let mut done = false;

    while !done {
        if advance {
            match iter.next() {
                Some(ch) => {
                    c = ch;
                    if c == '\n' {
                        position.col = 1;
                        position.line += 1;
                    }
                    else {
                        position.col += 1;
                    }
                    range.extend(position.clone());
                },
                None => {
                    c = '\0';
                    done = true;
                }
            }
        }
        advance = true;
        let mut reset = false;

        match state {
            LexerState::Start => if c.is_digit(10) {
                state = LexerState::Integer;
                token.push(c);
            }
            else if c.is_alphabetic() {
                state = LexerState::Ident;
                token.push(c);
            }
            else if c == '{' {
                tokens.push(PositionContainer(Token::LBrace, range.clone()));
            }
            else if c == '}' {
                tokens.push(PositionContainer(Token::RBrace, range.clone()));
            }
            else if c == '(' {
                tokens.push(PositionContainer(Token::LParen, range.clone()));
            }
            else if c == ')' {
                tokens.push(PositionContainer(Token::RParen, range.clone()));
            }
            else if c == ',' {
                tokens.push(PositionContainer(Token::Comma, range.clone()));
            }
            else if c == '+' {
                tokens.push(PositionContainer(Token::Plus, range.clone()));
            }
            else if c == '-' {
                tokens.push(PositionContainer(Token::Minus, range.clone()));
            }
            else if c == '*' {
                tokens.push(PositionContainer(Token::Star, range.clone()));
            }
            else if c == '/' {
                tokens.push(PositionContainer(Token::Slash, range.clone()));
            }
            else if c == '=' {
                tokens.push(PositionContainer(Token::Equals, range.clone()));
            }
            else if c == ':' {
                tokens.push(PositionContainer(Token::Colon, range.clone()));
            },

            LexerState::Ident => if c.is_alphanumeric() {
                token.push(c);
            }
            else {
                state = LexerState::Start;
                tokens.push(PositionContainer(
                    if &token[..] == "let" {
                        Token::KeywordLet
                    }
                    else if &token[..] == "if" {
                        Token::KeywordIf
                    }
                    else if &token[..] == "else" {
                        Token::KeywordElse
                    }
                    else if &token[..] == "def" {
                        Token::KeywordDef
                    }
                    else if &token[..] == "fn" {
                        Token::KeywordFn
                    }
                    else {
                        Token::Ident(token.clone())
                    }, range.clone()));
                reset = true;
                advance = false;
            },

            LexerState::Integer => if c.is_digit(10) {
                token.push(c);
            }
            else {
                state = LexerState::Start;
                tokens.push(PositionContainer(
                    Token::Integer(token.parse().unwrap()), range.clone()));
                reset = true;
                advance = false;
            }
        }

        if c == '\n' {
            tokens.push(PositionContainer(
                Token::Newline,
                PositionRange::new(position.clone(), position.clone())));
        }

        if reset {
            range = PositionRange::new(position.clone(), position.clone());
            token = String::new();
        }
    }
    tokens.push(PositionContainer(Token::Eof, range.clone()));

    tokens
}
