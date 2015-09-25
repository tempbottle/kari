pub mod stack;
pub mod value;
pub mod runtime_err;
mod builtins;

use std::rc::Rc;
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::cmp::Ordering;
use self::runtime_err::*;
use bytecode::{BlockId, BytecodeBlock, BytecodeInstr};
use self::stack::Stack;
use self::value::Value;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct VarId(pub u32);

//TODO: a more efficient linked tree might be better but doesn't sound easy to borrow check
#[derive(Clone)]
pub struct EnvironmentNode {
    pub parent: Option<Rc<EnvironmentNode>>,
    symbol: String,
    id: VarId,
}

pub struct Variable {
    pub env: Rc<EnvironmentNode>,
    pub value: Value
}

pub struct Interpreter {
    pub stack: Stack,
    pub call_stack: Vec<(BlockId, usize, Option<Rc<EnvironmentNode>>)>,
    pub variables: HashMap<VarId, Variable>,
    pub blocks: HashMap<BlockId, BytecodeBlock>,
    gc_counter: u32,
    next_var_id: VarId,
    current_env: Option<Rc<EnvironmentNode>>,
    instr_idx: usize,
    current_block: BlockId
}

impl EnvironmentNode {
    pub fn new(parent: Option<Rc<EnvironmentNode>>, symbol: String, id: VarId) -> EnvironmentNode {
        EnvironmentNode {
            symbol: symbol,
            id: id,
            parent: parent
        }
    }

    pub fn lookup(&self, name: &String) -> RuntimeResult<VarId> {
        if *name == self.symbol {
            Ok(self.id)
        }
        else {
            match self.parent {
                Some(ref parent) => parent.lookup(name),
                None => Err(RuntimeError::UnknownVariable(name.clone()))
            }
        }
    }

    pub fn get_all_vars(&self) -> Vec<(String, VarId)> {
        let mut vars = Vec::new();
        vars.push((self.symbol.clone(), self.id));
        if let Some(ref parent) = self.parent {
            vars.extend(parent.get_all_vars().iter().cloned());
        }
        vars
    }
}

impl Variable {
    pub fn get_refs(&self, i: &Interpreter) -> Vec<VarId> {
        let mut vars = Vec::new();
        match &self.value {
            &Value::Ref(id) => vars.push(id),
            //TODO: possibly store this info in the function's variable
            &Value::Function(id, _) => for instr in i.blocks[&id].instrs.iter() {
                match &instr.0 {
                    &BytecodeInstr::PushVar(ref name) => if let Ok(id) = self.env.lookup(name) {
                        vars.push(id);
                    },
                    &BytecodeInstr::PushRef(ref name) => if let Ok(id) = self.env.lookup(name) {
                        vars.push(id);
                    },
                    &BytecodeInstr::SetVar(ref name) => if let Ok(id) = self.env.lookup(name) {
                        vars.push(id);
                    },
                    _ => ()
                }
            },
            _ => ()
        }
        vars
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut i = Interpreter {
            stack: Stack::new(),
            call_stack: Vec::new(),
            variables: HashMap::new(),
            blocks: HashMap::new(),
            gc_counter: 10,
            next_var_id: VarId(0),
            current_env: None,
            instr_idx: 0,
            current_block: BlockId(0)
        };
        builtins::define_builtins(&mut i);
        i
    }

    pub fn traceback(&self) -> String {
        let mut ret = String::new();
        ret.push_str(&format!("bytecode position {}:{}\n",
                              self.current_block.0,
                              self.instr_idx)[..]);
        ret.push_str(&format!("current instruction {}\n",
                              self.blocks[&self.current_block].instrs[self.instr_idx].0)[..]);
        ret.push_str(&format!("{}\n",
            self.blocks[&self.current_block].instrs[self.instr_idx].1)[..]);
        for &(block, instr, _) in self.call_stack.iter().rev() {
            ret.push_str(&format!("{}\n", self.blocks[&block].instrs[instr].1)[..]);
        }
        ret
    }

    pub fn format_vars(&self) -> String {
        let mut ret = String::new();
        for &(ref name, id) in self.current_env.as_ref().unwrap().get_all_vars().iter() {
            ret.push_str(&format!("{}: {}\n", name, self.get_var(id).value)[..]);
        }
        ret
    }

    pub fn format_stack(&self) -> String {
        let mut ret = String::new();
        for value in self.stack.0.iter() {
            ret.push_str(&format!("{}\n", value));
        }
        ret
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
    pub fn get_var(&self, id: VarId) -> &Variable {
        &self.variables[&id]
    }

    #[inline]
    pub fn declare_var(&mut self, name: String) -> VarId {
        let id = self.next_var_id;
        self.next_var_id.0 += 1;
        self.current_env = Some(Rc::new(EnvironmentNode::new(self.current_env.clone(), name, id)));
        id
    }

    #[inline]
    pub fn set_var(&mut self, id: VarId, val: Value) {
        let env = self.current_env.clone().unwrap();
        self.variables.insert(id, Variable {
            env: env,
            value: val
        });
    }

    #[inline]
    pub fn set_host_func<F, S>(&mut self, name: S, func: F) where
        F: Fn(&mut Interpreter) -> RuntimeResult<()> + 'static, S: Borrow<str>
    {
        let id = self.declare_var(name.borrow().to_string());
        self.set_var(id, Value::HostFunction(Rc::new(func)));
    }

    fn jump(&mut self, id: BlockId) {
        self.call_stack.push((self.current_block, self.instr_idx + 1, self.current_env.clone()));
        self.current_block = id;
        self.instr_idx = 0;
    }

    pub fn run_instr(&mut self) -> RuntimeResult<()> {
        //println!("{}", self.blocks[&self.current_block].instrs[self.instr_idx].0);
        match self.blocks[&self.current_block].instrs[self.instr_idx].0.clone() {
            BytecodeInstr::Pop => {
                try!(self.stack.pop());
            },
            BytecodeInstr::Jump(id) => {
                self.jump(id);
                return Ok(());
            },
            BytecodeInstr::Call => {
                match try!(self.stack.pop()) {
                    Value::Function(id, _) => {
                        self.jump(id);
                        return Ok(());
                    },
                    Value::HostFunction(ref f) => try!(f(self)),
                    _ => return Err(RuntimeError::TypeMismatch)
                }
            },
            BytecodeInstr::Ret => {
                let (block, idx, env) = self.call_stack.pop().unwrap();
                self.current_env = env;
                self.current_block = block;
                self.instr_idx = idx;
                self.gc_counter -= 1;
                if self.gc_counter == 0 {
                    self.run_gc();
                    self.gc_counter = 10;
                }
                return Ok(());
            },
            BytecodeInstr::PushNil => self.stack.push(Value::Nil),
            BytecodeInstr::PushInt(x) => self.stack.push(Value::Integer(x)),
            BytecodeInstr::PushBool(b) => self.stack.push(Value::Boolean(b)),
            BytecodeInstr::PushStr(s) => self.stack.push(Value::Str(s.clone())),
            BytecodeInstr::PushVar(name) => {
                let id = try!(self.current_env.as_ref().unwrap().lookup(&name));
                let val = self.get_var(id).value.clone();
                self.stack.push(val);
            },
            BytecodeInstr::PushFunc(id, nargs) => self.stack.push(Value::Function(id, nargs)),
            BytecodeInstr::PushRef(name) => {
                let id = try!(self.current_env.as_ref().unwrap().lookup(&name));
                self.stack.push(Value::Ref(id));
            },
            BytecodeInstr::Deref => {
                match try!(self.stack.pop()) {
                    Value::Ref(id) => {
                        let val = self.get_var(id).value.clone();
                        self.stack.push(val);
                    },
                    _ => return Err(RuntimeError::TypeMismatch)
                }
            },
            BytecodeInstr::DeclareVar(name) => {
                self.declare_var(name);
            },
            BytecodeInstr::SetVar(name) => {
                let id = try!(self.current_env.as_ref().unwrap().lookup(&name));
                let val = try!(self.stack.pop());
                self.set_var(id, val);
            },
            BytecodeInstr::Add => {
                let rhs = try!(self.stack.pop());
                let lhs = try!(self.stack.pop());
                self.stack.push(try!(lhs + rhs));
            },
            BytecodeInstr::Sub => {
                let rhs = try!(self.stack.pop());
                let lhs = try!(self.stack.pop());
                self.stack.push(try!(lhs - rhs));
            },
            BytecodeInstr::Mul => {
                let rhs = try!(self.stack.pop());
                let lhs = try!(self.stack.pop());
                self.stack.push(try!(lhs * rhs));
            },
            BytecodeInstr::Div => {
                let rhs = try!(self.stack.pop());
                let lhs = try!(self.stack.pop());
                self.stack.push(try!(lhs / rhs));
            },
            BytecodeInstr::CmpEq => {
                let rhs = try!(self.stack.pop());
                let lhs = try!(self.stack.pop());
                self.stack.push(Value::Boolean(lhs == rhs));
            },
            BytecodeInstr::CmpLt => {
                let rhs = try!(self.stack.pop());
                let lhs = try!(self.stack.pop());
                self.stack.push(Value::Boolean(try!(lhs.cmp(&rhs)) == Ordering::Less));
            },
            BytecodeInstr::CmpGt => {
                let rhs = try!(self.stack.pop());
                let lhs = try!(self.stack.pop());
                self.stack.push(Value::Boolean(try!(lhs.cmp(&rhs)) == Ordering::Greater));
            },
            BytecodeInstr::If(t, f) => {
                let cond = match try!(self.stack.pop()) {
                    Value::Boolean(cond) => cond,
                    _ => return Err(RuntimeError::TypeMismatch)
                };
                if cond {
                    self.jump(t);
                    return Ok(());
                }
                else if let Some(f) = f {
                    self.jump(f);
                    return Ok(());
                }
            },
            BytecodeInstr::While => {
                let cond = match try!(self.stack.pop()) {
                    Value::Boolean(cond) => cond,
                    _ => return Err(RuntimeError::TypeMismatch)
                };
                if cond {
                    self.call_stack.push(
                        (self.current_block, 0, self.current_env.clone()));
                }
                else {
                    let (block, idx, env) = self.call_stack.pop().unwrap();
                    self.current_env = env;
                    self.current_block = block;
                    self.instr_idx = idx;
                    return Ok(());
                }
            },
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

    pub fn run_gc(&mut self) {
        let mut unmarked = self.variables.keys().cloned().collect::<HashSet<VarId>>();
        //println!("GC cycle");
        match self.current_env.as_ref() {
            Some(env) => {
                //mark all objects in local scope
                for (_, id) in env.get_all_vars() {
                    unmarked.remove(&id);
                    for id in self.get_var(id).get_refs(self) {
                        unmarked.remove(&id);
                    }
                }
                //sweep
                for id in unmarked.iter() {
                    //println!("Garbage collecting {}", id.0);
                    self.variables.remove(&id);
                }
            },
            None => ()
        }
    }
}
