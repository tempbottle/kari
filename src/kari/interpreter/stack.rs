use interpreter::runtime_err::*;
use interpreter::value::Value;

#[derive(Clone)]
pub struct Stack(pub Vec<Value>);

impl Stack {
    pub fn new() -> Stack {
        Stack(Vec::new())
    }

    #[inline]
    pub fn push(&mut self, v: Value) {
        self.0.push(v);
    }

    #[inline]
    pub fn get<'a>(&'a self) -> RuntimeResult<&'a Value> {
        self.0.last().ok_or(RuntimeError::EmptyStack)
    }

    #[inline]
    pub fn pop(&mut self) -> RuntimeResult<Value> {
        self.0.pop().ok_or(RuntimeError::EmptyStack)
    }
}
