pub mod stack;
pub mod value;
pub mod runtime_err;

use std::collections::HashMap;
use position::*;
use self::runtime_err::*;
use bytecode::{VarId, BlockId, BytecodeBlock, BytecodeInstr, BytecodeInstrContainer};
use self::stack::Stack;
use self::value::Value;

pub struct Interpreter {
    pub stack: Stack,
    pub call_stack: Vec<(BlockId, usize)>,
    pub variables: HashMap<VarId, Value>,
    blocks: HashMap<BlockId, BytecodeBlock>,
    instr_idx: usize,
    current_block: BlockId
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            stack: Stack::new(),
            call_stack: Vec::new(),
            variables: HashMap::new(),
            blocks: HashMap::new(),
            instr_idx: 0,
            current_block: BlockId(0)
        }
    }

    pub fn add_block(&mut self, block: BytecodeBlock) {
        self.blocks.insert(block.id, block);
    }

    pub fn add_blocks(&mut self, blocks: Vec<BytecodeBlock>) {
        for block in blocks.iter() {
            self.add_block(block.clone());
        }
    }

    #[inline]
    pub fn get_var(&self, id: VarId) -> RuntimeResult<&Value> {
        self.variables.get(&id).ok_or(RuntimeError::UnknownVariable)
    }

    #[inline]
    pub fn set_var(&mut self, id: VarId, val: Value) {
        self.variables.insert(id, val);
    }

    pub fn run_instr(&mut self) -> RuntimeResult<()> {
        match &self.blocks[&self.current_block].instrs[self.instr_idx].0 {
            &BytecodeInstr::Pop => {
                try!(self.stack.pop());
            },
            &BytecodeInstr::Jump(id) => {
                self.call_stack.push((self.current_block, self.instr_idx));
                self.current_block = id;
                self.instr_idx = 0;
                return Ok(());
            },
            &BytecodeInstr::Ret => {
                let (block, idx) = self.call_stack.pop().unwrap();
                self.current_block = block;
                self.instr_idx = idx + 1;
                return Ok(());
            },
            &BytecodeInstr::PushNil => self.stack.push(Value::Nil),
            &BytecodeInstr::PushInt(x) => self.stack.push(Value::Integer(x)),
            &BytecodeInstr::PushVar(id) => {
                let val = try!(self.get_var(id)).clone();
                self.stack.push(val);
            },
            &BytecodeInstr::SetVar(id) => {
                let val = try!(self.stack.pop());
                self.set_var(id, val);
            },
            &BytecodeInstr::Add => {
                let lhs = try!(self.stack.pop());
                let rhs = try!(self.stack.pop());
                self.stack.push(try!(lhs + rhs));
            },
            &BytecodeInstr::Sub => {
                let lhs = try!(self.stack.pop());
                let rhs = try!(self.stack.pop());
                self.stack.push(try!(lhs - rhs));
            },
            &BytecodeInstr::Mul => {
                let lhs = try!(self.stack.pop());
                let rhs = try!(self.stack.pop());
                self.stack.push(try!(lhs * rhs));
            },
            &BytecodeInstr::Div => {
                let lhs = try!(self.stack.pop());
                let rhs = try!(self.stack.pop());
                self.stack.push(try!(lhs / rhs));
            },
            &BytecodeInstr::CmpEq => {
                let lhs = try!(self.stack.pop());
                let rhs = try!(self.stack.pop());
                self.stack.push(Value::Boolean(lhs == rhs));
            }
        }
        self.instr_idx += 1;
        Ok(())
    }

    pub fn run_block(&mut self, id: BlockId) -> RuntimeResult<()> {
        self.current_block = id;
        self.instr_idx = 0;
        while self.instr_idx < self.blocks[&self.current_block].instrs.len() {
            try!(self.run_instr());
        }
        Ok(())
    }
}
