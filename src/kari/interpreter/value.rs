use std::rc::Rc;
use std::fmt;
use std::ops::{Add, Sub, Mul, Div};
use std::cmp::{PartialEq, Eq, Ord, Ordering};
use interpreter::{Interpreter, VarId};
use interpreter::runtime_err::*;
use bytecode::BlockId;

#[derive(Clone)]
pub enum Value {
    Nil,
    Integer(i32),
    Boolean(bool),
    Str(String),
    Ref(VarId),
    Function(BlockId, u32),
    HostFunction(Rc<Fn(&mut Interpreter) -> RuntimeResult<()>>),
}

#[derive(Copy, Clone, Debug)]
pub enum ValueType {
    Nil,
    Integer,
    Boolean,
    Str,
    Function,
    Ref,
    HostFunction,
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        use self::Value::*;
        match self {
            &Nil => ValueType::Nil,
            &Integer(_) => ValueType::Integer,
            &Boolean(_) => ValueType::Boolean,
            &Str(_) => ValueType::Str,
            &Ref(_) => ValueType::Ref,
            &Function(_, _) => ValueType::Function,
            &HostFunction(_) => ValueType::HostFunction,
        }
    }

    pub fn cmp(&self, other: &Value) -> RuntimeResult<Ordering> {
        use self::Value::*;
        match self {
            &Integer(a) => match other {
                &Integer(b) => Ok(a.cmp(&b)),
                _ => Err(RuntimeError::TypeMismatch)
            },
            _ => Err(RuntimeError::TypeMismatch)
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Value::*;
        match self {
            &Nil => write!(f, "nil"),
            &Integer(x) => write!(f, "{}", x),
            &Boolean(b) => write!(f, "{}", b),
            &Str(ref s) => write!(f, "{}", s),
            &Ref(VarId(id)) => write!(f, "ref({})", id),
            &Function(_, _) => write!(f, "<function>"),
            &HostFunction(_) => write!(f, "<hostfunction>"),
        }
    }
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Value) -> bool {
        use self::Value::*;
        match self {
            &Nil => match other {
                &Nil => true,
                _ => false
            },
            &Integer(a) => match other {
                &Integer(b) => a == b,
                _ => false
            },
            &Boolean(a) => match other {
                &Boolean(b) => a == b,
                _ => false
            },
            &Str(ref a) => match other {
                &Str(ref b) => a == b,
                _ => false
            },
            &Ref(a) => match other {
                &Ref(b) => a == b,
                _ => false
            },
            &Function(ref a, _) => match other {
                &Function(ref b, _) => a == b,
                _ => false
            },
            &HostFunction(_) => false
        }
    }
}

impl Eq for Value {
}

impl Add<Value> for Value {
    type Output = RuntimeResult<Value>;

    fn add(self, other: Value) -> RuntimeResult<Value> {
        use self::Value::*;
        match self {
            Integer(a) => match other {
                Integer(b) => Ok(Integer(a + b)),
                _ => Err(RuntimeError::TypeMismatch)
            },
            _ => Err(RuntimeError::TypeMismatch),
        }
    }
}

impl Sub<Value> for Value {
    type Output = RuntimeResult<Value>;

    fn sub(self, other: Value) -> RuntimeResult<Value> {
        use self::Value::*;
        match self {
            Integer(a) => match other {
                Integer(b) => Ok(Integer(a - b)),
                _ => Err(RuntimeError::TypeMismatch)
            },
            _ => Err(RuntimeError::TypeMismatch)
        }
    }
}

impl Mul<Value> for Value {
    type Output = RuntimeResult<Value>;

    fn mul(self, other: Value) -> RuntimeResult<Value> {
        use self::Value::*;
        match self {
            Integer(a) => match other {
                Integer(b) => Ok(Integer(a * b)),
                _ => Err(RuntimeError::TypeMismatch)
            },
            _ => Err(RuntimeError::TypeMismatch)
        }
    }
}

impl Div<Value> for Value {
    type Output = RuntimeResult<Value>;

    fn div(self, other: Value) -> RuntimeResult<Value> {
        use self::Value::*;
        match self {
            Integer(a) => match other {
                Integer(b) => Ok(Integer(a / b)),
                _ => Err(RuntimeError::TypeMismatch)
            },
            _ => Err(RuntimeError::TypeMismatch)
        }
    }
}
