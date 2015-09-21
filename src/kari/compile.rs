use std::rc::Rc;
use std::collections::HashMap;
use position::*;
use ast::{Expression, ExpressionContainer};
use bytecode::{VarId, BlockId, BytecodeBlock, BytecodeInstr, BytecodeInstrContainer};

#[derive(Clone, Debug)]
pub enum CompileErrorType {
    UnknownSymbol(String)
}

pub type CompileError = PositionContainer<CompileErrorType>;

pub type CompileResult<T> = Result<T, CompileError>;

pub struct SymTable {
    pub table: HashMap<String, VarId>,
    pub parent: Option<Rc<SymTable>>
}

pub struct Compiler {
    next_var_id: VarId,
    next_block_id: BlockId,
}

impl SymTable {
    pub fn new(parent: Option<Rc<SymTable>>) -> SymTable {
        SymTable {
            table: HashMap::new(),
            parent: parent
        }
    }

    pub fn lookup(&self, name: &String) -> Option<VarId> {
        match self.table.get(name) {
            Some(id) => Some(id.clone()),
            None => match self.parent {
                Some(ref parent) => parent.lookup(name),
                None => None
            }
        }
    }

    pub fn insert(&mut self, name: String, id: VarId) {
        self.table.insert(name, id);
    }
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            next_var_id: VarId(0),
            next_block_id: BlockId(1),
        }
    }

    pub fn compile_expr(&mut self,
                        symtable: &mut SymTable,
                        expr: &ExpressionContainer,
                        instrs: &mut Vec<BytecodeInstrContainer>,
                        blocks: &mut Vec<BytecodeBlock>) -> CompileResult<()>
    {
        match expr {
            &PositionContainer(Expression::Variable(ref name), ref pos) => {
                let id = try!(symtable.lookup(name).ok_or(PositionContainer(
                    CompileErrorType::UnknownSymbol(name.clone()), pos.clone())));
                instrs.push(PositionContainer(BytecodeInstr::PushVar(id), pos.clone()));
            },
            &PositionContainer(Expression::Integer(x), ref pos) =>
                instrs.push(PositionContainer(BytecodeInstr::PushInt(x), pos.clone())),
            &PositionContainer(Expression::Add(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(symtable, &**lhs, instrs, blocks));
                try!(self.compile_expr(symtable, &**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Add, pos.clone()));
            },
            &PositionContainer(Expression::Sub(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(symtable, &**lhs, instrs, blocks));
                try!(self.compile_expr(symtable, &**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Sub, pos.clone()));
            },
            &PositionContainer(Expression::Mul(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(symtable, &**lhs, instrs, blocks));
                try!(self.compile_expr(symtable, &**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Mul, pos.clone()));
            },
            &PositionContainer(Expression::Div(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(symtable, &**lhs, instrs, blocks));
                try!(self.compile_expr(symtable, &**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Div, pos.clone()));
            },
            &PositionContainer(Expression::CompareEq(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(symtable, &**lhs, instrs, blocks));
                try!(self.compile_expr(symtable, &**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::CmpEq, pos.clone()));
            },
            &PositionContainer(Expression::VarDeclaration(ref name, ref rhs), ref pos) => {
                let id = self.next_var_id;
                self.next_var_id.0 += 1;
                symtable.insert(name.clone(), id);
                try!(self.compile_expr(symtable, &**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::SetVar(id), pos.clone()));
                instrs.push(PositionContainer(BytecodeInstr::PushNil, pos.clone()));
            }
            &PositionContainer(Expression::Block(ref exprs), ref pos) => {
                let id = self.next_block_id;
                self.next_block_id.0 += 1;
                instrs.push(PositionContainer(BytecodeInstr::Jump(id), pos.clone()));
                let mut instrs = Vec::new();
                for expr in exprs.split_last().unwrap().1.iter() {
                    try!(self.compile_expr(symtable, expr, &mut instrs, blocks));
                    instrs.push(PositionContainer(BytecodeInstr::Pop, expr.1.clone()));
                }
                try!(self.compile_expr(symtable, exprs.last().unwrap(), &mut instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Ret, pos.clone()));
                blocks.push(BytecodeBlock::new(id, instrs));
            },
        }
        Ok(())
    }
}

pub fn compile_ast(expr: ExpressionContainer) -> CompileResult<Vec<BytecodeBlock>> {
    let mut instrs = Vec::new();
    let mut blocks = Vec::new();
    try!(Compiler::new().compile_expr(&mut SymTable::new(None), &expr, &mut instrs, &mut blocks));
    instrs.push(PositionContainer(BytecodeInstr::Pop, expr.1.clone()));
    blocks.push(BytecodeBlock::new(BlockId(0), instrs));
    Ok(blocks)
}
