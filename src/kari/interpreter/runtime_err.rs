#[derive(Clone, Debug)]
pub enum RuntimeError {
    EmptyStack,
    UnknownVariable(String),
    TypeMismatch
}

pub type RuntimeResult<T> = Result<T, RuntimeError>;
