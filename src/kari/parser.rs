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

macro_rules! expect_lookahead {
    ($parser:expr, $msg:expr, { $( $token:pat => $body:expr ),* }) => {
        match $parser.lookahead() {
            $( $token => $body ),*,
            t =>
                return Err(ParserError(format!("Expected {}, got `{}'", $msg, t),
                    $parser.tokens[$parser.pos].1.clone()))
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
                &Token::Eof => break,
                _ => exprs.push(try!(self.parse_toplevel()))
            }
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
                _ => exprs.push(try!(self.parse_toplevel()))
            }
        }
        let range = tok.1.clone().extend_new(exprs.last().unwrap().clone().1.end);
        Ok((exprs, range))
    }

    fn parse_primary(&mut self) -> ParserResult {
        let expr = match self.lookahead() {
            &Token::LBrace => {
                println!("{:?}", self.lookahead());
                let (exprs, range) = try!(self.parse_block());
                PositionContainer(Expression::Block(exprs), range)
            },
            &Token::LParen => {
                self.next();
                let expr = try!(self.parse_expression());
                expect!(self, "close parenthesis", {
                    PositionContainer(Token::RParen, _) => ()
                });
                expr
            },
            &Token::LBracket => try!(self.parse_list()),
            &Token::KeywordRef => {
                let mut range = self.next().1;
                let expr = try!(self.parse_expression());
                range.extend(expr.1.end.clone());
                PositionContainer(
                    Expression::Reference(Box::new(expr)),
                    range)
            },
            &Token::KeywordDeref => {
                let mut range = self.next().1;
                let expr = try!(self.parse_expression());
                range.extend(expr.1.end.clone());
                PositionContainer(
                    Expression::Dereference(Box::new(expr)),
                    range)
            },
            &Token::KeywordLet => try!(self.parse_var_declaration()),
            &Token::KeywordIf => try!(self.parse_if_statement()),
            &Token::KeywordWhile => try!(self.parse_while_loop()),
            &Token::KeywordFor => try!(self.parse_for_loop()),
            &Token::KeywordDef => try!(self.parse_func_declaration()),
            &Token::KeywordFn => {
                let tok = self.next();
                let mut func = try!(self.parse_func_definition());
                func.1 = tok.1.extend_new(func.1.end.clone());
                func
            },
            _ => expect!(self, "primary expression", {
                PositionContainer(Token::Ident(name), pos) =>
                    PositionContainer(Expression::Variable(name), pos),
                PositionContainer(Token::Integer(x), pos) =>
                    PositionContainer(Expression::Integer(x), pos),
                PositionContainer(Token::KeywordTrue, pos) =>
                    PositionContainer(Expression::Boolean(true), pos),
                PositionContainer(Token::KeywordFalse, pos) =>
                    PositionContainer(Expression::Boolean(false), pos),
                PositionContainer(Token::DQuote, pos) => {
                    let content = expect!(self, "string after quote", {
                        PositionContainer(Token::Str(s), _) => s
                    });
                    let end = expect!(self, "closing quote", {
                        PositionContainer(Token::DQuote, pos) => pos
                    });
                    let range = pos.extend_new(end.end);
                    PositionContainer(Expression::Str(content), range)
                }
            })
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

    fn parse_toplevel(&mut self) -> ParserResult {
        let expr = try!(self.parse_expression());
        match self.lookahead() {
            &Token::Semicolon => {
                let end = self.next().1;
                let range = expr.1.clone().extend_new(end.end);
                Ok(PositionContainer(Expression::Statement(Box::new(expr)), range))
            },
            _ => Ok(expr)
        }
    }

    fn parse_expression(&mut self) -> ParserResult {
        let expr = try!(self.parse_primary());
        match self.lookahead() {
            &Token::Colon => {
                //variable assignment
                self.next();
                expect!(self, "equals", {
                    PositionContainer(Token::Equals, _) => ()
                });
                let rhs = try!(self.parse_expression());
                let range = expr.1.clone().extend_new(rhs.1.end.clone());
                Ok(PositionContainer(Expression::Assignment(Box::new(expr), Box::new(rhs)), range))
            },
            _ => Ok(try!(self.parse_binop_rhs(expr, 0)))
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
        match self.lookahead() {
            &Token::RParen => {
                self.next();
            },
            _ => loop {
                args.push(expect!(self, "identifier", {
                    PositionContainer(Token::Ident(ref name), _) => name.clone()
                }));
                expect!(self, "comma or close parenthesis", {
                    PositionContainer(Token::Comma, _) => (),
                    PositionContainer(Token::RParen, _) => break
                });
            }
        }
        let body = try!(self.parse_block());
        let range = start.extend_new(body.1.end.clone());
        Ok(PositionContainer(Expression::FuncDefinition(args, body.0), range))
    }

    fn parse_if_statement(&mut self) -> ParserResult {
        let tok = self.next(); //assume that this is 'if'
        let cond = try!(self.parse_expression());
        expect_lookahead!(self, "block after if _", {
            &Token::LBrace => ()
        });
        let t = try!(self.parse_block());
        let (f, range) = if let &Token::KeywordElse = self.lookahead() {
            self.next();
            expect_lookahead!(self, "block after else", {
                &Token::LBrace => ()
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

    fn parse_while_loop(&mut self) -> ParserResult {
        let tok = self.next(); //assume that this is 'while'
        let cond = try!(self.parse_expression());
        expect_lookahead!(self, "block after while _", {
            &Token::LBrace => ()
        });
        let body = try!(self.parse_block());
        let range = tok.1.extend_new(body.1.end.clone());
        Ok(PositionContainer(Expression::While(Box::new(cond), body.0), range))
    }

    fn parse_for_loop(&mut self) -> ParserResult {
        let tok = self.next(); //assume that this is 'for'
        let var = expect!(self, "identifier after for", {
            PositionContainer(Token::Ident(var), _) => var
        });
        expect!(self, "colon after for _", {
            PositionContainer(Token::Colon, _) => ()
        });
        expect!(self, "open parenthesis", {
            PositionContainer(Token::LParen, _) => ()
        });
        let low = try!(self.parse_expression());
        expect!(self, "comma", {
            PositionContainer(Token::Comma, _) => ()
        });
        let high = try!(self.parse_expression());
        expect!(self, "close parenthesis", {
            PositionContainer(Token::RParen, _) => ()
        });
        expect_lookahead!(self, "block after for _", {
            &Token::LBrace => ()
        });
        let mut body = try!(self.parse_block());
        let range = tok.1.extend_new(body.1.end.clone());
        let mut exprs = Vec::new();
        body.0.push(PositionContainer(Expression::Assignment(
            Box::new(PositionContainer(Expression::Variable(var.clone()), range.clone())),
            Box::new(PositionContainer(Expression::Add(
                Box::new(PositionContainer(Expression::Variable(var.clone()), range.clone())),
                Box::new(PositionContainer(Expression::Integer(1), range.clone())),
            ), range.clone()))), range.clone()));
        exprs.push(PositionContainer(
            Expression::Statement(Box::new(PositionContainer(
                Expression::VarDeclaration(var.clone(), Box::new(low)), range.clone()))),
            range.clone()));
        exprs.push(PositionContainer(
            Expression::While(Box::new(PositionContainer(
                Expression::CompareLt(
                    Box::new(PositionContainer(
                        Expression::Variable(var.clone()), range.clone())),
                    Box::new(high)),
                range.clone())), body.0), range.clone()));

        Ok(PositionContainer(Expression::Block(exprs), range.clone()))
    }

    fn parse_func_call_args(&mut self) ->
        Result<(Vec<ExpressionContainer>, PositionRange), ParserError>
    {
        let tok = self.next(); //assume that this is '('
        let mut args = Vec::new();
        let pos: PositionRange;
        match self.lookahead() {
            &Token::RParen => {
                pos = self.next().1;
            },
            _ => loop {
                args.push(try!(self.parse_expression()));
                expect!(self, "comma or close parenthesis", {
                    PositionContainer(Token::Comma, _) => (),
                    PositionContainer(Token::RParen, pos2) => {
                        pos = pos2;
                        break;
                    }
                });
            }
        }
        Ok((args, tok.1.extend_new(pos.end)))
    }

    fn parse_list(&mut self) -> ParserResult {
        let tok = self.next(); //assume that this is '['
        let mut exprs = Vec::new();
        let pos: PositionRange;
        match self.lookahead() {
            &Token::RBracket => {
                pos = self.next().1;
            },
            _ => loop {
                exprs.push(try!(self.parse_expression()));
                expect!(self, "comma or close bracket", {
                    PositionContainer(Token::Comma, _) => (),
                    PositionContainer(Token::RBracket, pos2) => {
                        pos = pos2;
                        break;
                    }
                });
            }
        }
        Ok(PositionContainer(Expression::List(exprs), tok.1.extend_new(pos.end)))
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
                &Token::Less => 1,
                &Token::Greater => 1,
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
                &Token::Less => Expression::CompareLt(lhs, rhs),
                &Token::Greater => Expression::CompareGt(lhs, rhs),
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
