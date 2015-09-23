use position::*;
use ast::{Expression, ExpressionContainer};
use bytecode::{BlockId, BytecodeBlock, BytecodeInstr, BytecodeInstrContainer};

pub type CompileError = PositionContainer<String>;

pub type CompileResult<T> = Result<T, CompileError>;

pub struct Compiler {
    next_block_id: BlockId,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            next_block_id: BlockId(1),
        }
    }

    pub fn compile_block(&mut self,
                         exprs: &Vec<ExpressionContainer>,
                         pos: &PositionRange,
                         instrs: &mut Vec<BytecodeInstrContainer>,
                         blocks: &mut Vec<BytecodeBlock>) -> CompileResult<()>
    {
        for expr in exprs.split_last().unwrap().1.iter() {
            try!(self.compile_expr(expr, instrs, blocks));
            instrs.push(PositionContainer(BytecodeInstr::Pop, expr.1.clone()));
        }
        try!(self.compile_expr(exprs.last().unwrap(), instrs, blocks));
        instrs.push(PositionContainer(BytecodeInstr::Ret, pos.clone()));
        Ok(())
    }

    pub fn compile_expr(&mut self,
                        expr: &ExpressionContainer,
                        instrs: &mut Vec<BytecodeInstrContainer>,
                        blocks: &mut Vec<BytecodeBlock>) -> CompileResult<()>
    {
        match expr {
            &PositionContainer(Expression::Variable(ref name), ref pos) =>
                instrs.push(PositionContainer(BytecodeInstr::PushVar(name.clone()), pos.clone())),
            &PositionContainer(Expression::Integer(x), ref pos) =>
                instrs.push(PositionContainer(BytecodeInstr::PushInt(x), pos.clone())),
            &PositionContainer(Expression::Str(ref s), ref pos) =>
                instrs.push(PositionContainer(BytecodeInstr::PushStr(s.clone()), pos.clone())),
            &PositionContainer(Expression::Add(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(&**lhs, instrs, blocks));
                try!(self.compile_expr(&**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Add, pos.clone()));
            },
            &PositionContainer(Expression::Sub(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(&**lhs, instrs, blocks));
                try!(self.compile_expr(&**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Sub, pos.clone()));
            },
            &PositionContainer(Expression::Mul(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(&**lhs, instrs, blocks));
                try!(self.compile_expr(&**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Mul, pos.clone()));
            },
            &PositionContainer(Expression::Div(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(&**lhs, instrs, blocks));
                try!(self.compile_expr(&**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Div, pos.clone()));
            },
            &PositionContainer(Expression::CompareEq(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(&**lhs, instrs, blocks));
                try!(self.compile_expr(&**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::CmpEq, pos.clone()));
            },
            &PositionContainer(Expression::Reference(ref expr), ref pos) => {
                match expr.0 {
                    Expression::Variable(ref name) => {
                        instrs.push(PositionContainer(
                            BytecodeInstr::PushRef(name.clone()), pos.clone()));
                    },
                    _ => return Err(PositionContainer("expected rvalue".to_string(), pos.clone()))
                }
            },
            &PositionContainer(Expression::Dereference(ref expr), ref pos) => {
                try!(self.compile_expr(&**expr, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Deref, pos.clone()));
            },
            &PositionContainer(Expression::FuncDefinition(ref args, ref body), ref pos) => {
                let id = self.next_block_id;
                self.next_block_id.0 += 1;
                {
                    let mut instrs = Vec::new();
                    for arg in args.iter().rev() {
                        instrs.push(PositionContainer(
                            BytecodeInstr::DeclareVar(arg.clone()), pos.clone()));
                        instrs.push(PositionContainer(
                            BytecodeInstr::SetVar(arg.clone()), pos.clone()));
                    }
                    try!(self.compile_block(&*body, pos, &mut instrs, blocks));
                    blocks.push(BytecodeBlock::new(id, instrs));
                }
                instrs.push(PositionContainer(
                    BytecodeInstr::PushFunc(id, args.len() as u32), pos.clone()));
            },
            &PositionContainer(Expression::VarDeclaration(ref name, ref rhs), ref pos) => {
                try!(self.compile_expr(&**rhs, instrs, blocks));
                instrs.push(PositionContainer(
                    BytecodeInstr::DeclareVar(name.clone()), pos.clone()));
                instrs.push(PositionContainer(BytecodeInstr::SetVar(name.clone()), pos.clone()));
                instrs.push(PositionContainer(BytecodeInstr::PushNil, pos.clone()));
            },
            &PositionContainer(Expression::Assignment(ref lhs, ref rhs), ref pos) => {
                match lhs.0 {
                    Expression::Variable(ref name) => {
                        try!(self.compile_expr(&**rhs, instrs, blocks));
                        instrs.push(PositionContainer(
                            BytecodeInstr::SetVar(name.clone()), pos.clone()));
                        instrs.push(PositionContainer(BytecodeInstr::PushNil, pos.clone()));
                    },
                    _ => return Err(PositionContainer("expected rvalue".to_string(), pos.clone()))
                }
            },
            &PositionContainer(Expression::If(ref cond, ref t, ref f), ref pos) => {
                try!(self.compile_expr(&**cond, instrs, blocks));
                let t_id = {
                    let t_id = self.next_block_id;
                    self.next_block_id.0 += 1;
                    let mut instrs = Vec::new();
                    try!(self.compile_block(&*t, pos, &mut instrs, blocks));
                    blocks.push(BytecodeBlock::new(t_id, instrs));
                    t_id
                };
                let f_id = match f {
                    &Some(ref f) => {
                        let f_id = self.next_block_id;
                        self.next_block_id.0 += 1;
                        let mut instrs = Vec::new();
                        try!(self.compile_block(&*f, pos, &mut instrs, blocks));
                        blocks.push(BytecodeBlock::new(f_id, instrs));
                        Some(f_id)
                    },
                    &None => None
                };
                instrs.push(PositionContainer(BytecodeInstr::If(t_id, f_id), pos.clone()));
            },
            &PositionContainer(Expression::Call(ref func, ref args), ref pos) => {
                for arg in args.iter() {
                    try!(self.compile_expr(&*arg, instrs, blocks));
                }
                try!(self.compile_expr(&**func, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Call, pos.clone()));
            },
            &PositionContainer(Expression::Block(ref exprs), ref pos) => {
                let id = self.next_block_id;
                self.next_block_id.0 += 1;
                let mut instrs = Vec::new();
                try!(self.compile_block(exprs, pos, &mut instrs, blocks));
                blocks.push(BytecodeBlock::new(id, instrs));
            },
        }
        Ok(())
    }
}

pub fn compile_ast(expr: ExpressionContainer) -> CompileResult<Vec<BytecodeBlock>> {
    let mut instrs = Vec::new();
    let mut blocks = Vec::new();
    instrs.push(PositionContainer(BytecodeInstr::Jump(BlockId(1)), expr.1.clone()));
    try!(Compiler::new().compile_expr(&expr, &mut instrs, &mut blocks));
    instrs.push(PositionContainer(BytecodeInstr::Pop, expr.1.clone()));
    blocks.push(BytecodeBlock::new(BlockId(0), instrs));
    Ok(blocks)
}
