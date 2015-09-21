use position::*;

#[derive(Clone, Debug)]
pub enum Expression {
    VarDeclaration(String, Box<ExpressionContainer>),
    Variable(String),
    Integer(i32),
    Add(Box<ExpressionContainer>, Box<ExpressionContainer>),
    Sub(Box<ExpressionContainer>, Box<ExpressionContainer>),
    Mul(Box<ExpressionContainer>, Box<ExpressionContainer>),
    Div(Box<ExpressionContainer>, Box<ExpressionContainer>),
    CompareEq(Box<ExpressionContainer>, Box<ExpressionContainer>),
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
            &Add(ref lhs, ref rhs) => visualize_binop("Add", lhs, rhs, id, next_node, out),
            &Sub(ref lhs, ref rhs) => visualize_binop("Sub", lhs, rhs, id, next_node, out),
            &Mul(ref lhs, ref rhs) => visualize_binop("Mul", lhs, rhs, id, next_node, out),
            &Div(ref lhs, ref rhs) => visualize_binop("Div", lhs, rhs, id, next_node, out),
            &CompareEq(ref lhs, ref rhs) =>
                visualize_binop("CompareEq", lhs, rhs, id, next_node, out),
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
