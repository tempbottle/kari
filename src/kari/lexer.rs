use std::fmt;
use position::*;

#[derive(Clone, Debug)]
pub enum Token {
    KeywordLet,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordFor,
    KeywordDef,
    KeywordFn,
    KeywordRef,
    KeywordDeref,
    KeywordTrue,
    KeywordFalse,
    LBrace, //{
    RBrace, //}
    LBracket, //[
    RBracket, //]
    LParen, //(
    RParen, //)
    DQuote,
    Comma,
    Semicolon,
    Colon,
    Equals,
    Less,
    Greater,
    Plus,
    Minus,
    Star,
    Slash,
    Eof,
    Str(String),
    Ident(String),
    Integer(i32),
}

#[derive(Copy, Clone, Debug)]
enum LexerState {
    Start,
    Ident,
    Str,
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
            &KeywordWhile => write!(f, "while"),
            &KeywordFor => write!(f, "for"),
            &KeywordDef => write!(f, "def"),
            &KeywordFn => write!(f, "fn"),
            &KeywordRef => write!(f, "ref"),
            &KeywordDeref => write!(f, "deref"),
            &KeywordTrue => write!(f, "true"),
            &KeywordFalse => write!(f, "false"),
            &LBrace => write!(f, "lbrace"),
            &RBrace => write!(f, "rbrace"),
            &LBracket => write!(f, "lbracket"),
            &RBracket => write!(f, "rbracket"),
            &LParen => write!(f, "lparen"),
            &RParen => write!(f, "rparen"),
            &DQuote => write!(f, "dquote"),
            &Comma => write!(f, "comma"),
            &Semicolon => write!(f, "semicolon"),
            &Colon => write!(f, "colon"),
            &Equals => write!(f, "equals"),
            &Less => write!(f, "less"),
            &Greater => write!(f, "greater"),
            &Plus => write!(f, "plus"),
            &Minus => write!(f, "minus"),
            &Star => write!(f, "star"),
            &Slash => write!(f, "slash"),
            &Eof => write!(f, "EOF"),
            &Ident(ref name) => write!(f, "identifier({})", name),
            &Integer(x) => write!(f, "integer({})", x),
            &Str(ref s) => write!(f, "str({})", s),
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
            else if c == '[' {
                tokens.push(PositionContainer(Token::LBracket, range.clone()));
            }
            else if c == ']' {
                tokens.push(PositionContainer(Token::RBracket, range.clone()));
            }
            else if c == '(' {
                tokens.push(PositionContainer(Token::LParen, range.clone()));
            }
            else if c == ')' {
                tokens.push(PositionContainer(Token::RParen, range.clone()));
            }
            else if c == '"' {
                tokens.push(PositionContainer(Token::DQuote, range.clone()));
                state = LexerState::Str;
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
            else if c == '<' {
                tokens.push(PositionContainer(Token::Less, range.clone()));
            }
            else if c == '>' {
                tokens.push(PositionContainer(Token::Greater, range.clone()));
            }
            else if c == ':' {
                tokens.push(PositionContainer(Token::Colon, range.clone()));
            }
            else if c == ';' {
                tokens.push(PositionContainer(Token::Semicolon, range.clone()));
            },

            LexerState::Ident => if c.is_alphanumeric() || c == '_' {
                token.push(c);
            }
            else {
                state = LexerState::Start;
                tokens.push(PositionContainer(
                    match &token[..] {
                        "let" => Token::KeywordLet,
                        "if" => Token::KeywordIf,
                        "else" => Token::KeywordElse,
                        "while" => Token::KeywordWhile,
                        "for" => Token::KeywordFor,
                        "def" => Token::KeywordDef,
                        "fn" => Token::KeywordFn,
                        "ref" => Token::KeywordRef,
                        "deref" => Token::KeywordDeref,
                        "true" => Token::KeywordTrue,
                        "false" => Token::KeywordFalse,
                        _ => Token::Ident(token.clone())
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
            },

            LexerState::Str => if c == '"' {
                state = LexerState::Start;
                tokens.push(PositionContainer(Token::Str(token.clone()), range.clone()));
                tokens.push(PositionContainer(
                    Token::DQuote, PositionRange::new(range.start.clone(), range.start.clone())));
                reset = true;
                //not advance=false because the DQuote has already been handled
            }
            else {
                token.push(c);
            }
        }

        if reset {
            range = PositionRange::new(position.clone(), position.clone());
            token = String::new();
        }
    }
    tokens.push(PositionContainer(Token::Eof, range.clone()));

    tokens
}
