use position::*;
use lexer::{lex_source, Token, TokenContainer};
use ast::{Expression, ExpressionContainer};

#[derive(Debug, Clone)]
pub struct ParserError(pub String, pub PositionRange);

pub type ParserResult = Result<ExpressionContainer, ParserError>;

pub struct Parser {
    tokens: Vec<TokenContainer>,
    pos: usize
}

macro_rules! expect {
    ($parser:expr, $msg:expr, { $( $token:pat => $body:expr ),* }) => {
        match $parser.next() {
            $( $token => $body ),*,
            PositionContainer(t, pos) =>
                return Err(ParserError(format!("Expected {}, got `{}'", $msg, t), pos))
        }
    }
}

impl Parser {
    pub fn new(tokens: Vec<TokenContainer>) -> Parser {
        Parser {
            tokens: tokens,
            pos: 0
        }
    }

    fn lookahead(&self) -> &Token {
        &self.tokens[self.pos].0
    }

    fn next(&mut self) -> TokenContainer {
        let tok = self.tokens[self.pos].clone();
        self.pos += 1;
        tok
    }

    pub fn parse(&mut self) -> ParserResult {
        let mut exprs = Vec::new();
        loop {
            match self.lookahead() {
                &Token::Newline | &Token::Eof => (),
                _ => exprs.push(try!(self.parse_expression()))
            }
            expect!(self, "EOF or newline", {
                PositionContainer(Token::Eof, _) => break,
                PositionContainer(Token::Newline, _) => ()
            });
        }

        let mut range = self.tokens[0].1.clone();
        range.extend(self.tokens.last().unwrap().clone().1.end);
        Ok(PositionContainer(Expression::Block(exprs), range))
    }

    //primary ::= ident | integer
    fn parse_primary(&mut self) -> ParserResult {
        expect!(self, "primary expression", {
            PositionContainer(Token::Ident(name), pos) =>
                return Ok(PositionContainer(Expression::Variable(name), pos)),
            PositionContainer(Token::Integer(x), pos) =>
                return Ok(PositionContainer(Expression::Integer(x), pos))
        });
    }

    //expression ::= 'let' ident ':=' expression | primary binoprhs
    fn parse_expression(&mut self) -> ParserResult {
        match self.lookahead() {
            &Token::KeywordLet => {
                let tok = self.next();
                let name = expect!(self, "identifier after let", {
                    PositionContainer(Token::Ident(ref name), _) => name.clone()
                });
                expect!(self, "colon", {
                    PositionContainer(Token::Colon, _) => ()
                });
                expect!(self, "equals", {
                    PositionContainer(Token::Equals, _) => ()
                });
                let rhs = try!(self.parse_expression());
                let range = tok.1.extend_new(rhs.1.end.clone());
                Ok(PositionContainer(Expression::VarDeclaration(name, Box::new(rhs)), range))
            },
            _ => {
                let primary = try!(self.parse_primary());
                self.parse_binop_rhs(primary, 0)
            }
        }
    }

    //NOTE: The EBNF here is not quite correct since it ignores operator precedence
    //binoprhs ::= (operator primary)*
    //operator ::= '+' | '-' | '*' | '/' | '='
    fn parse_binop_rhs(&mut self,
                       mut lhs: ExpressionContainer,
                       left_precedence: i32) -> ParserResult {

        fn precedence(tok: &Token) -> i32 {
            match tok {
                &Token::Plus => 3,
                &Token::Minus => 3,
                &Token::Star => 4,
                &Token::Slash => 4,
                &Token::Equals => 1,
                _ => -1
            }
        }

        fn binop(tok: &Token,
                 lhs: Box<ExpressionContainer>,
                 rhs: Box<ExpressionContainer>) -> Expression
        {
            match tok {
                &Token::Plus => Expression::Add(lhs, rhs),
                &Token::Minus => Expression::Sub(lhs, rhs),
                &Token::Star => Expression::Mul(lhs, rhs),
                &Token::Slash => Expression::Div(lhs, rhs),
                &Token::Equals => Expression::CompareEq(lhs, rhs),
                _ => unreachable!()
            }
        }

        loop
        {
            if precedence(self.lookahead()) < left_precedence {
                return Ok(lhs);
            }
            let tok = self.next().0;

            let mut rhs = try!(self.parse_primary());

            if precedence(&tok) < precedence(self.lookahead()) {
                rhs = try!(self.parse_binop_rhs(rhs.clone(), precedence(&tok) + 1));
            }

            lhs = PositionContainer(
                binop(&tok, Box::new(lhs.clone()), Box::new(rhs.clone())),
                lhs.1.clone().extend_new(rhs.1.end.clone()));
        }
    }
}

pub fn parse_source(source: String, file: Option<String>) -> ParserResult {
    Parser::new(lex_source(source, file)).parse()
}
