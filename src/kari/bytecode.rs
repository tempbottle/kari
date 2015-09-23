use std::fmt;
use position::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

#[derive(Clone, Debug)]
pub enum BytecodeInstr {
    Pop,
    Jump(BlockId),
    Call,
    If(BlockId, Option<BlockId>),
    Ret,
    PushNil,
    PushInt(i32),
    PushStr(String),
    PushVar(String),
    PushFunc(BlockId, u32),
    DeclareVar(String),
    SetVar(String),
    Add,
    Sub,
    Mul,
    Div,
    CmpEq
}

#[derive(Clone, Debug)]
pub struct BytecodeBlock {
    pub id: BlockId,
    pub instrs: Vec<BytecodeInstrContainer>
}

pub type BytecodeInstrContainer = PositionContainer<BytecodeInstr>;

impl fmt::Display for BytecodeInstr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::BytecodeInstr::*;
        match self {
            &Pop => write!(f, "POP"),
            &Jump(BlockId(x)) => write!(f, "JUMP {}", x),
            &Call => write!(f, "CALL"),
            &If(BlockId(a), Some(BlockId(b))) => write!(f, "IF {} {}", a, b),
            &If(BlockId(a), None) => write!(f, "IF {}", a),
            &Ret => write!(f, "RET"),
            &PushNil => write!(f, "PUSHNIL"),
            &PushInt(x) => write!(f, "PUSHINT {}", x),
            &PushStr(ref s) => write!(f, "PUSHSTR \"{}\"", s),
            &PushVar(ref name) => write!(f, "PUSHVAR {}", name),
            &PushFunc(BlockId(x), nargs) => write!(f, "PUSHFUNC {} {}", x, nargs),
            &DeclareVar(ref name) => write!(f, "DECLVAR {}", name),
            &SetVar(ref name) => write!(f, "SETVAR {}", name),
            &Add => write!(f, "ADD"),
            &Sub => write!(f, "SUB"),
            &Mul => write!(f, "MUL"),
            &Div => write!(f, "DIV"),
            &CmpEq => write!(f, "CMPEQ"),
        }
    }
}

impl BytecodeBlock {
    pub fn new(id: BlockId, instrs: Vec<BytecodeInstrContainer>) -> BytecodeBlock {
        BytecodeBlock {
            id: id,
            instrs: instrs
        }
    }
}

impl fmt::Display for BytecodeBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:\n{}", self.id.0,
            self.instrs.iter().map(|o| format!("\t{}", o.0)).collect::<Vec<String>>().join("\n"))
    }
}
