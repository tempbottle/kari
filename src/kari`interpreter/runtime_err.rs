#[derive(Clone, Debug)]
pub enum RuntimeError {
    EmptyStack,
    UnknownVariable,
    TypeMismatch
}

pub type RuntimeResult<T> = Result<T, RuntimeError>;
