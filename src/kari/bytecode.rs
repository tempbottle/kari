use std::io::prelude::*;
use std::fmt;
use byteorder::{BigEndian, WriteBytesExt, ReadBytesExt};
use position::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

#[derive(Clone, Debug)]
pub enum BytecodeInstr {
    Pop,
    Jump(BlockId),
    Call,
    If(BlockId, Option<BlockId>),
    While,
    Ret,
    PushNil,
    PushInt(i32),
    PushBool(bool),
    PushStr(String),
    PushList(u32),
    PushVar(String),
    PushRef(String),
    PushFunc(BlockId, u32),
    Deref,
    DeclareVar(String),
    SetVar(String),
    Add,
    Sub,
    Mul,
    Div,
    CmpEq,
    CmpLt,
    CmpGt,
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
            &While => write!(f, "WHILE"),
            &Ret => write!(f, "RET"),
            &PushNil => write!(f, "PUSHNIL"),
            &PushInt(x) => write!(f, "PUSHINT {}", x),
            &PushStr(ref s) => write!(f, "PUSHSTR \"{}\"", s),
            &PushList(nelements) => write!(f, "PUSHLIST {}", nelements),
            &PushBool(b) => write!(f, "PUSHBOOL {}", b),
            &PushVar(ref name) => write!(f, "PUSHVAR {}", name),
            &PushRef(ref name) => write!(f, "PUSHREF {}", name),
            &PushFunc(BlockId(x), nargs) => write!(f, "PUSHFUNC {} {}", x, nargs),
            &Deref => write!(f, "DEREF"),
            &DeclareVar(ref name) => write!(f, "DECLVAR {}", name),
            &SetVar(ref name) => write!(f, "SETVAR {}", name),
            &Add => write!(f, "ADD"),
            &Sub => write!(f, "SUB"),
            &Mul => write!(f, "MUL"),
            &Div => write!(f, "DIV"),
            &CmpEq => write!(f, "CMPEQ"),
            &CmpLt => write!(f, "CMPLT"),
            &CmpGt => write!(f, "CMPGT"),
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

    pub fn read<R: Read>(r: &mut R) -> BytecodeBlock {
        fn read_string<R: Read>(len: usize, r: &mut R) -> String {
            let mut s = Vec::with_capacity(len);
            for _ in 0..len {
                s.push(r.read_u8().unwrap());
            }
            String::from_utf8(s).unwrap()
        }

        let id = r.read_u32::<BigEndian>().unwrap();
        let file = match r.read_u8().unwrap() {
            0 => None,
            n => Some(read_string(n as usize, r))
        };
        let n_instrs = r.read_u32::<BigEndian>().unwrap();
        let mut instrs = Vec::with_capacity(n_instrs as usize);
        for _ in 0..n_instrs {
            use self::BytecodeInstr::*;

            let pos = {
                let start_line = r.read_u16::<BigEndian>().unwrap() as u32;
                let start_col = r.read_u16::<BigEndian>().unwrap() as u32;
                let end_line = r.read_u16::<BigEndian>().unwrap() as u32;
                let end_col = r.read_u16::<BigEndian>().unwrap() as u32;
                PositionRange::new(
                    Position::new(start_line, start_col, file.clone()),
                    Position::new(end_line, end_col, file.clone()))
            };

            let token = r.read_u8().unwrap();
            let instr = match token {
                0 => Pop,
                1 => Jump(BlockId(r.read_u32::<BigEndian>().unwrap())),
                3 => Call,
                4 => {
                    let id = r.read_u32::<BigEndian>().unwrap();
                    let id2 = r.read_u32::<BigEndian>().unwrap();
                    If(BlockId(id), Some(BlockId(id2)))
                },
                5 => If(BlockId(r.read_u32::<BigEndian>().unwrap()), None),
                6 => Ret,
                7 => PushInt(r.read_i32::<BigEndian>().unwrap()),
                8 => {
                    let len = r.read_u32::<BigEndian>().unwrap() as usize;
                    PushStr(read_string(len, r))
                },
                9 => {
                    let len = r.read_u8().unwrap() as usize;
                    PushVar(read_string(len, r))
                },
                10 => {
                    let len = r.read_u8().unwrap() as usize;
                    PushRef(read_string(len, r))
                },
                11 => {
                    let block = r.read_u32::<BigEndian>().unwrap();
                    PushFunc(BlockId(block), r.read_u8().unwrap() as u32)
                },
                12 => Deref,
                13 => {
                    let len = r.read_u8().unwrap() as usize;
                    DeclareVar(read_string(len, r))
                },
                14 => Add,
                15 => Sub,
                16 => Div,
                17 => CmpEq,
                18 => CmpLt,
                19 => CmpGt,
                20 => PushNil,
                21 => PushBool(r.read_u8().unwrap() == 1),
                22 => Mul,
                23 => {
                    let len = r.read_u8().unwrap() as usize;
                    SetVar(read_string(len, r))
                },
                24 => While,
                25 => {
                    let len = r.read_u32::<BigEndian>().unwrap();
                    PushList(len)
                },
                x => panic!("Unknown token {}", x)
            };
            instrs.push(PositionContainer(instr, pos));
        }
        BytecodeBlock::new(BlockId(id), instrs)
    }

    pub fn write<W: Write>(&self, w: &mut W) {
        fn write_string<W: Write>(s: &String, w: &mut W) {
            for c in s.as_bytes().iter() {
                w.write_u8(*c).unwrap();
            }
        }

        w.write_u32::<BigEndian>(self.id.0).unwrap();
        match self.instrs[0].1.start.file {
            None => w.write_u8(0).unwrap(),
            Some(ref fname) => {
                w.write_u8(fname.len() as u8).unwrap();
                write_string(fname, w);
            }
        }
        w.write_u32::<BigEndian>(self.instrs.len() as u32).unwrap();
        for &PositionContainer(ref instr, ref pos) in self.instrs.iter() {
            use self::BytecodeInstr::*;

            w.write_u16::<BigEndian>(pos.start.line as u16).unwrap();
            w.write_u16::<BigEndian>(pos.start.col as u16).unwrap();
            w.write_u16::<BigEndian>(pos.end.line as u16).unwrap();
            w.write_u16::<BigEndian>(pos.end.col as u16).unwrap();
            match instr {
                &Pop => w.write_u8(0).unwrap(),
                &Jump(BlockId(id)) => {
                    w.write_u8(1).unwrap();
                    w.write_u32::<BigEndian>(id).unwrap();
                },
                &Call => w.write_u8(3).unwrap(),
                &If(BlockId(id), Some(BlockId(id2))) => {
                    w.write_u8(4).unwrap();
                    w.write_u32::<BigEndian>(id).unwrap();
                    w.write_u32::<BigEndian>(id2).unwrap();
                },
                &If(BlockId(id), None) => {
                    w.write_u8(5).unwrap();
                    w.write_u32::<BigEndian>(id).unwrap();
                },
                &Ret => w.write_u8(6).unwrap(),
                &PushInt(x) => {
                    w.write_u8(7).unwrap();
                    w.write_i32::<BigEndian>(x).unwrap();
                },
                &PushStr(ref s) => {
                    w.write_u8(8).unwrap();
                    w.write_u32::<BigEndian>(s.len() as u32).unwrap();
                    write_string(s, w);
                },
                &PushVar(ref s) => {
                    w.write_u8(9).unwrap();
                    w.write_u8(s.len() as u8).unwrap();
                    write_string(s, w);
                },
                &PushRef(ref s) => {
                    w.write_u8(10).unwrap();
                    w.write_u8(s.len() as u8).unwrap();
                    write_string(s, w);
                },
                &PushFunc(BlockId(id), nargs) => {
                    w.write_u8(11).unwrap();
                    w.write_u32::<BigEndian>(id).unwrap();
                    w.write_u8(nargs as u8).unwrap();
                },
                &Deref => w.write_u8(12).unwrap(),
                &DeclareVar(ref name) => {
                    w.write_u8(13).unwrap();
                    w.write_u8(name.len() as u8).unwrap();
                    write_string(name, w);
                },
                &Add => w.write_u8(14).unwrap(),
                &Sub => w.write_u8(15).unwrap(),
                &Div => w.write_u8(16).unwrap(),
                &CmpEq => w.write_u8(17).unwrap(),
                &CmpLt => w.write_u8(18).unwrap(),
                &CmpGt => w.write_u8(19).unwrap(),
                &PushNil => w.write_u8(20).unwrap(),
                &PushBool(b) => {
                    w.write_u8(21).unwrap();
                    w.write_u8(if b { 1 } else { 0 }).unwrap();
                },
                &Mul => w.write_u8(22).unwrap(),
                &SetVar(ref name) => {
                    w.write_u8(23).unwrap();
                    w.write_u8(name.len() as u8).unwrap();
                    write_string(name, w);
                },
                &While => w.write_u8(24).unwrap(),
                &PushList(nelements) => {
                    w.write_u8(25).unwrap();
                    w.write_u32::<BigEndian>(nelements).unwrap();
                }
            }
        }
    }
}

impl fmt::Display for BytecodeBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:\n{}", self.id.0,
            self.instrs.iter().map(|o| format!("\t{}", o.0)).collect::<Vec<String>>().join("\n"))
    }
}
