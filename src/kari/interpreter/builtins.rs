use interpreter::runtime_err::*;
use interpreter::value::Value;
use interpreter::Interpreter;

fn builtin_print(i: &mut Interpreter) -> RuntimeResult<()> {
    let val = try!(i.stack.pop());
    println!("{}", val);
    i.stack.push(Value::Nil);
    Ok(())
}

pub fn define_builtins(i: &mut Interpreter) {
    i.set_host_func("print", builtin_print);
}
