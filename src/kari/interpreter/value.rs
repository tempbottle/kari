use std::ops::{Add, Sub, Mul, Div};
use interpreter::runtime_err::*;
use bytecode::BlockId;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Nil,
    Integer(i32),
    Boolean(bool),
}

#[derive(Copy, Clone, Debug)]
pub enum ValueType {
    Nil,
    Integer,
    Boolean,
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        use self::Value::*;
        match self {
            &Nil => ValueType::Nil,
            &Integer(_) => ValueType::Integer,
            &Boolean(_) => ValueType::Boolean,
        }
    }
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
