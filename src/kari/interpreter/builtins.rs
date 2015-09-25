use interpreter::runtime_err::*;
use interpreter::value::Value;
use interpreter::Interpreter;

fn builtin_print(i: &mut Interpreter) -> RuntimeResult<()> {
    let val = try!(i.stack.pop());
    println!("{}", val);
    i.stack.push(Value::Nil);
    Ok(())
}

fn builtin_index(i: &mut Interpreter) -> RuntimeResult<()> {
    let idx = try!(i.stack.pop());
    let list = try!(i.stack.pop());
    match list {
        Value::List(elements) => match idx {
            Value::Integer(idx) => i.stack.push(elements[idx as usize].clone()),
            _ => return Err(RuntimeError::TypeMismatch)
        },
        _ => return Err(RuntimeError::TypeMismatch)
    }
    Ok(())
}

pub fn define_builtins(i: &mut Interpreter) {
    i.set_host_func("print", builtin_print);
    i.set_host_func("index", builtin_index);
}
