use lexer;
use parser;
use compile;
use bytecode;
use interpreter::Interpreter;
use interpreter::value::Value;

fn test_kari(source: &str, expected: Value) {
    let tokens = lexer::lex_source(source.to_string(), None);
    let ast = parser::Parser::new(tokens).parse().unwrap();
    let bytecode = compile::compile_ast(ast).unwrap();

    let mut interpreter = Interpreter::new();
    interpreter.set_host_func("test_output", move |i: &mut Interpreter| {
        assert_eq!(i.stack.pop().unwrap(), expected);
        i.stack.push(Value::Nil);
        Ok(())
    });
    interpreter.add_blocks(bytecode.clone());
    if let Err(err) = interpreter.run_block(bytecode::BlockId(0)) {
        println!("\n{}",
            bytecode.iter().map(|o| format!("{}", o)).collect::<Vec<String>>().join("\n"));

        panic!("{:?}\n{}\n{}", err, interpreter.traceback(), interpreter.format_vars());
    }
}

#[test]
fn simple_output() {
    test_kari(r#"test_output(0);"#, Value::Integer(0));
}

#[test]
fn if_statement() {
    test_kari(r#"
    if true {
        test_output(true);
    }
    else {
        test_output(false);
    }
    "#, Value::Boolean(true));
}

#[test]
fn functions() {
    test_kari(r#"
    def f(x) {
        x + 1
    }
    test_output(f(3));
    "#, Value::Integer(4));
}

#[test]
fn references() {
    test_kari(r#"
    let a := 1;
    let b := ref a;
    a := 2;
    test_output(deref b);
    "#, Value::Integer(2));
}

#[test]
fn while_loops() {
    test_kari(r#"
    let a := 10;
    let b := 0;
    while a > 0 {
        b := b + 1;
        a := a - 1;
    }
    test_output(b);"#, Value::Integer(10));
}
