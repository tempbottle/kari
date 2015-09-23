use position::*;

#[derive(Clone, Debug)]
pub enum Expression {
    VarDeclaration(String, Box<ExpressionContainer>),
    Assignment(Box<ExpressionContainer>, Box<ExpressionContainer>),
    FuncDefinition(Vec<String>, Vec<ExpressionContainer>),
    Variable(String),
    Integer(i32),
    Str(String),
    Add(Box<ExpressionContainer>, Box<ExpressionContainer>),
    Sub(Box<ExpressionContainer>, Box<ExpressionContainer>),
    Mul(Box<ExpressionContainer>, Box<ExpressionContainer>),
    Div(Box<ExpressionContainer>, Box<ExpressionContainer>),
    CompareEq(Box<ExpressionContainer>, Box<ExpressionContainer>),
    If(Box<ExpressionContainer>, Vec<ExpressionContainer>, Option<Vec<ExpressionContainer>>),
    Call(Box<ExpressionContainer>, Vec<ExpressionContainer>),
    Block(Vec<ExpressionContainer>),
}

pub type ExpressionContainer = PositionContainer<Expression>;

impl Expression {

    fn visualize_dot_render_node(&self, next_node: &mut u32, out: &mut String) -> u32 {
        use self::Expression::*;

        fn visualize_binop(name: &str,
                           lhs: &Box<ExpressionContainer>,
                           rhs: &Box<ExpressionContainer>,
                           id: u32,
                           next_node: &mut u32,
                           out: &mut String)
        {
            let lhs_id = lhs.0.visualize_dot_render_node(next_node, out);
            let rhs_id = rhs.0.visualize_dot_render_node(next_node, out);
            out.push_str(&format!("{} [label=\"{}\"];", id, name)[..]);
            out.push_str(&format!("{} -> {};", id, lhs_id)[..]);
            out.push_str(&format!("{} -> {};", id, rhs_id)[..]);
        }

        let id = *next_node;
        *next_node += 1;
        match self {
            &VarDeclaration(ref var, ref rhs) => {
                let rhs_id = rhs.0.visualize_dot_render_node(next_node, out);
                out.push_str(&format!("{} [label=\"VarDeclaration({})\"];", id, var)[..]);
                out.push_str(&format!("{} -> {};", id, rhs_id)[..]);
            },
            &Assignment(ref lhs, ref rhs) =>
                visualize_binop("Assignment", lhs, rhs, id, next_node, out),
            &FuncDefinition(ref args, ref body) => {
                out.push_str(
                    &format!("{} [label=\"FuncDefinition({})\"];", id, args.join(","))[..]);
                for expr in body.iter() {
                    let expr_id = expr.0.visualize_dot_render_node(next_node, out);
                    out.push_str(&format!("{} -> {};", id, expr_id)[..]);
                }
            },
            &Add(ref lhs, ref rhs) => visualize_binop("Add", lhs, rhs, id, next_node, out),
            &Sub(ref lhs, ref rhs) => visualize_binop("Sub", lhs, rhs, id, next_node, out),
            &Mul(ref lhs, ref rhs) => visualize_binop("Mul", lhs, rhs, id, next_node, out),
            &Div(ref lhs, ref rhs) => visualize_binop("Div", lhs, rhs, id, next_node, out),
            &CompareEq(ref lhs, ref rhs) =>
                visualize_binop("CompareEq", lhs, rhs, id, next_node, out),
            &If(ref expr, ref t, ref f) => {
                out.push_str(&format!("{} [label=\"If\"];", id)[..]);
                let expr_id = expr.0.visualize_dot_render_node(next_node, out);
                out.push_str(&format!("{} -> {};", id, expr_id)[..]);
                let t_id = *next_node;
                *next_node += 1;
                out.push_str(&format!("{} [label=\"True\"];", t_id)[..]);
                out.push_str(&format!("{} -> {};", id, t_id));
                for expr in t.iter() {
                    let expr_id = expr.0.visualize_dot_render_node(next_node, out);
                    out.push_str(&format!("{} -> {};", t_id, expr_id));
                }
                if let &Some(ref f) = f {
                    let f_id = *next_node;
                    *next_node += 1;
                    out.push_str(&format!("{} [label=\"False\"]", f_id)[..]);
                    out.push_str(&format!("{} -> {};", id, f_id)[..]);
                    for expr in f.iter() {
                        let expr_id = expr.0.visualize_dot_render_node(next_node, out);
                        out.push_str(&format!("{} -> {};", f_id, expr_id));
                    }
                }
            },
            &Call(ref func, ref args) => {
                out.push_str(&format!("{} [label=\"Call\"];", id)[..]);
                let func_id = func.0.visualize_dot_render_node(next_node, out);
                out.push_str(&format!("{} -> {};", id, func_id));
                let args_id = *next_node;
                *next_node += 1;
                out.push_str(&format!("{} [label=\"Args\"];", args_id)[..]);
                out.push_str(&format!("{} -> {};", id, args_id)[..]);
                for arg in args.iter() {
                    let arg_id = arg.0.visualize_dot_render_node(next_node, out);
                    out.push_str(&format!("{} -> {};", args_id, arg_id)[..]);
                }
            },
            &Block(ref exprs) => {
                out.push_str(&format!("{} [label=\"Block\"];", id)[..]);
                for expr in exprs.iter() {
                    let expr_id = expr.0.visualize_dot_render_node(next_node, out);
                    out.push_str(&format!("{} -> {};", id, expr_id)[..]);
                }
            },
            &Variable(ref name) =>
                out.push_str(&format!("{} [label=\"Variable({})\"];", id, name)[..]),
            &Integer(x) => out.push_str(&format!("{} [label=\"Integer({})\"];", id, x)[..]),
            &Str(ref s) => out.push_str(&format!("{} [label=\"Str({})\"];", id, s)[..]),
        }
        id
    }

    pub fn visualize_dot(&self) -> String {
        let mut out = String::new();
        out.push_str("digraph ast {");
        self.visualize_dot_render_node(&mut 0, &mut out);
        out.push_str("}");
        out
    }
}
