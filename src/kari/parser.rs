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

        let range = self.tokens[0].1.clone().extend_new(self.tokens.last().unwrap().clone().1.end);
        Ok(PositionContainer(Expression::Block(exprs), range))
    }

    fn parse_block(&mut self) -> Result<(Vec<ExpressionContainer>, PositionRange), ParserError> {
        let tok = self.next(); //assume that this is '{'
        let mut exprs = Vec::new();
        loop {
            match self.lookahead() {
                &Token::RBrace => {
                    self.next();
                    break;
                },
                &Token::Newline => (),
                _ => exprs.push(try!(self.parse_expression()))
            }
            expect!(self, "newline or '}'", {
                PositionContainer(Token::RBrace, _) => break,
                PositionContainer(Token::Newline, _) => ()
            });
        }
        let range = tok.1.clone().extend_new(exprs.last().unwrap().clone().1.end);
        Ok((exprs, range))
    }

    fn parse_primary(&mut self) -> ParserResult {
        expect!(self, "primary expression", {
            PositionContainer(Token::Ident(name), pos) =>
                return Ok(PositionContainer(Expression::Variable(name), pos)),
            PositionContainer(Token::Integer(x), pos) =>
                return Ok(PositionContainer(Expression::Integer(x), pos))
        });
    }

    fn parse_expression(&mut self) -> ParserResult {
        let expr = match self.lookahead() {
            &Token::LBrace => {
                let (exprs, range) = try!(self.parse_block());
                PositionContainer(Expression::Block(exprs), range)
            },
            &Token::KeywordLet => try!(self.parse_var_declaration()),
            &Token::KeywordIf => try!(self.parse_if_statement()),
            &Token::KeywordDef => try!(self.parse_func_declaration()),
            &Token::KeywordFn => {
                let tok = self.next();
                let mut func = try!(self.parse_func_definition());
                func.1 = tok.1.extend_new(func.1.end.clone());
                func
            },
            _ => {
                let primary = try!(self.parse_primary());
                try!(self.parse_binop_rhs(primary, 0))
            }
        };
        match self.lookahead() {
            &Token::LParen => {
                let (args, pos) = try!(self.parse_func_call_args());
                let range = expr.1.clone().extend_new(pos.end);
                Ok(PositionContainer(Expression::Call(Box::new(expr), args), range))
            },
            _ => Ok(expr)
        }
    }

    fn parse_var_declaration(&mut self) -> ParserResult {
        let tok = self.next(); //assume that this is 'let'
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
    }

    //syntax sugar for let name := fn(args) { body }
    fn parse_func_declaration(&mut self) -> ParserResult {
        let tok = self.next(); //assume that this is 'def'
        let name = expect!(self, "identifier after def", {
            PositionContainer(Token::Ident(ref name), _) => name.clone()
        });
        let func = try!(self.parse_func_definition());
        let range = tok.1.extend_new(func.1.end.clone());
        Ok(PositionContainer(Expression::VarDeclaration(name, Box::new(func)), range))
    }

    fn parse_func_definition(&mut self) -> ParserResult {
        let start = expect!(self, "left parenthesis", {
            PositionContainer(Token::LParen, pos) => pos
        });
        //parse the argument list
        let mut args = Vec::new();
        loop {
            args.push(expect!(self, "identifier", {
                PositionContainer(Token::Ident(ref name), _) => name.clone()
            }));
            expect!(self, "comma or close parenthesis", {
                PositionContainer(Token::Comma, _) => (),
                PositionContainer(Token::RParen, _) => break
            });
        }
        let body = try!(self.parse_block());
        let range = start.extend_new(body.1.end.clone());
        Ok(PositionContainer(Expression::FuncDefinition(args, body.0), range))
    }

    fn parse_if_statement(&mut self) -> ParserResult {
        let tok = self.next(); //assume that this is 'if'
        let cond = try!(self.parse_expression());
        //skip whitespace
        while let &Token::Newline = self.lookahead() {
            self.next();
        }
        expect!(self, "block after if _", {
            PositionContainer(Token::LBrace, _) => ()
        });
        let t = try!(self.parse_block());
        while let &Token::Newline = self.lookahead() {
            self.next();
        }
        let (f, range) = if let &Token::KeywordElse = self.lookahead() {
            self.next();
            expect!(self, "block after else", {
                PositionContainer(Token::LBrace, _) => ()
            });
            let f = try!(self.parse_block());
            let range = tok.1.extend_new(f.0.last().unwrap().1.end.clone());
            (Some(f), range)
        }
        else {
            (None, tok.1.extend_new(t.1.end.clone()))
        };
        Ok(PositionContainer(
            Expression::If(Box::new(cond), t.0, f.map(|f| f.0)), range))
    }

    fn parse_func_call_args(&mut self) ->
        Result<(Vec<ExpressionContainer>, PositionRange), ParserError>
    {
        let tok = self.next(); //assume that this is '('
        let mut args = Vec::new();
        let mut pos: PositionRange;
        loop {
            args.push(try!(self.parse_expression()));
            expect!(self, "comma or close parenthesis", {
                PositionContainer(Token::Comma, _) => (),
                PositionContainer(Token::RParen, pos2) => {
                    pos = pos2;
                    break;
                }
            });
        }
        Ok((args, tok.1.extend_new(pos.end)))
    }

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
