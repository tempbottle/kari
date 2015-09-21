use std::rc::Rc;
use std::cell::RefCell;
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

#[derive(Clone, Debug)]
pub struct SymTable {
    pub table: HashMap<String, VarId>,
    pub parent: Option<Rc<RefCell<SymTable>>>
}

pub struct Compiler {
    next_var_id: VarId,
    next_block_id: BlockId,
}

impl SymTable {
    pub fn new(parent: Option<Rc<RefCell<SymTable>>>) -> SymTable {
        SymTable {
            table: HashMap::new(),
            parent: parent
        }
    }

    pub fn lookup(&self, name: &String) -> Option<VarId> {
        match self.table.get(name) {
            Some(id) => Some(id.clone()),
            None => match self.parent {
                Some(ref parent) => parent.borrow().lookup(name),
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

    pub fn compile_block(&mut self,
                         symtable: Rc<RefCell<SymTable>>,
                         exprs: &Vec<ExpressionContainer>,
                         pos: &PositionRange,
                         instrs: &mut Vec<BytecodeInstrContainer>,
                         blocks: &mut Vec<BytecodeBlock>) -> CompileResult<()>
    {
        let mut symtable = Rc::new(RefCell::new(SymTable::new(Some(symtable.clone()))));
        for expr in exprs.split_last().unwrap().1.iter() {
            try!(self.compile_expr(symtable.clone(), expr, instrs, blocks));
            instrs.push(PositionContainer(BytecodeInstr::Pop, expr.1.clone()));
        }
        try!(self.compile_expr(symtable, exprs.last().unwrap(), instrs, blocks));
        instrs.push(PositionContainer(BytecodeInstr::Ret, pos.clone()));
        Ok(())
    }

    pub fn compile_expr(&mut self,
                        symtable: Rc<RefCell<SymTable>>,
                        expr: &ExpressionContainer,
                        instrs: &mut Vec<BytecodeInstrContainer>,
                        blocks: &mut Vec<BytecodeBlock>) -> CompileResult<()>
    {
        match expr {
            &PositionContainer(Expression::Variable(ref name), ref pos) => {
                let id = try!(symtable.borrow().lookup(name).ok_or(PositionContainer(
                    CompileErrorType::UnknownSymbol(name.clone()), pos.clone())));
                instrs.push(PositionContainer(BytecodeInstr::PushVar(id), pos.clone()));
            },
            &PositionContainer(Expression::Integer(x), ref pos) =>
                instrs.push(PositionContainer(BytecodeInstr::PushInt(x), pos.clone())),
            &PositionContainer(Expression::Str(ref s), ref pos) =>
                instrs.push(PositionContainer(BytecodeInstr::PushStr(s.clone()), pos.clone())),
            &PositionContainer(Expression::Add(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(symtable.clone(), &**lhs, instrs, blocks));
                try!(self.compile_expr(symtable.clone(), &**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Add, pos.clone()));
            },
            &PositionContainer(Expression::Sub(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(symtable.clone(), &**lhs, instrs, blocks));
                try!(self.compile_expr(symtable.clone(), &**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Sub, pos.clone()));
            },
            &PositionContainer(Expression::Mul(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(symtable.clone(), &**lhs, instrs, blocks));
                try!(self.compile_expr(symtable.clone(), &**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Mul, pos.clone()));
            },
            &PositionContainer(Expression::Div(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(symtable.clone(), &**lhs, instrs, blocks));
                try!(self.compile_expr(symtable.clone(), &**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Div, pos.clone()));
            },
            &PositionContainer(Expression::CompareEq(ref lhs, ref rhs), ref pos) => {
                try!(self.compile_expr(symtable.clone(), &**lhs, instrs, blocks));
                try!(self.compile_expr(symtable.clone(), &**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::CmpEq, pos.clone()));
            },
            &PositionContainer(Expression::FuncDefinition(ref args, ref body), ref pos) => {
                let id = self.next_block_id;
                self.next_block_id.0 += 1;
                {
                    let mut instrs = Vec::new();
                    let mut symtable = SymTable::new(Some(symtable.clone()));
                    for arg in args.iter().rev() {
                        let id = self.next_var_id;
                        self.next_var_id.0 += 1;
                        symtable.insert(arg.clone(), id);
                        instrs.push(PositionContainer(BytecodeInstr::SetVar(id), pos.clone()));
                    }
                    try!(self.compile_block(
                        Rc::new(RefCell::new(symtable)), &*body, pos, &mut instrs, blocks));
                    blocks.push(BytecodeBlock::new(id, instrs));
                }
                instrs.push(PositionContainer(
                    BytecodeInstr::PushFunc(id, args.len() as u32), pos.clone()));
            },
            &PositionContainer(Expression::VarDeclaration(ref name, ref rhs), ref pos) => {
                let id = {
                    let mut symtable_mut = symtable.borrow_mut();
                    let id = self.next_var_id;
                    self.next_var_id.0 += 1;
                    symtable_mut.insert(name.clone(), id);
                    id
                };
                try!(self.compile_expr(symtable, &**rhs, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::SetVar(id), pos.clone()));
                instrs.push(PositionContainer(BytecodeInstr::PushNil, pos.clone()));
            },
            &PositionContainer(Expression::If(ref cond, ref t, ref f), ref pos) => {
                try!(self.compile_expr(symtable.clone(), &**cond, instrs, blocks));
                let t_id = {
                    let t_id = self.next_block_id;
                    self.next_block_id.0 += 1;
                    let mut instrs = Vec::new();
                    try!(self.compile_block(symtable.clone(), &*t, pos, &mut instrs, blocks));
                    blocks.push(BytecodeBlock::new(t_id, instrs));
                    t_id
                };
                let f_id = match f {
                    &Some(ref f) => {
                        let f_id = self.next_block_id;
                        self.next_block_id.0 += 1;
                        let mut instrs = Vec::new();
                        try!(self.compile_block(symtable.clone(), &*f, pos, &mut instrs, blocks));
                        blocks.push(BytecodeBlock::new(f_id, instrs));
                        Some(f_id)
                    },
                    &None => None
                };
                instrs.push(PositionContainer(BytecodeInstr::If(t_id, f_id), pos.clone()));
            },
            &PositionContainer(Expression::Call(ref func, ref args), ref pos) => {
                for arg in args.iter() {
                    try!(self.compile_expr(symtable.clone(), &*arg, instrs, blocks));
                }
                try!(self.compile_expr(symtable.clone(), &**func, instrs, blocks));
                instrs.push(PositionContainer(BytecodeInstr::Call, pos.clone()));
            },
            &PositionContainer(Expression::Block(ref exprs), ref pos) => {
                let id = self.next_block_id;
                self.next_block_id.0 += 1;
                let mut instrs = Vec::new();
                try!(self.compile_block(symtable, exprs, pos, &mut instrs, blocks));
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
    try!(Compiler::new().compile_expr(
        Rc::new(RefCell::new(SymTable::new(None))), &expr, &mut instrs, &mut blocks));
    instrs.push(PositionContainer(BytecodeInstr::Pop, expr.1.clone()));
    blocks.push(BytecodeBlock::new(BlockId(0), instrs));
    Ok(blocks)
}
