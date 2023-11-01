use crate::parser;

#[derive(Default)]
pub struct Context {
    indentation: usize,
}

pub trait Format {
    fn format(&self, context: &mut Context) -> String;
}

impl<'a> Format for parser::Expr<'a> {
    fn format(&self, context: &mut Context) -> String {
        match self {
            parser::Expr::Const(s) => s.to_uppercase(),
            parser::Expr::Var(s) => format!("${{{s}}}"),
            parser::Expr::Quoted(s) => format!(r#""{s}""#),
            parser::Expr::Unary(op, expr) => {
                format!("{} {}", op.to_uppercase(), expr.format(context))
            }
            parser::Expr::Binary(op, left, right) => {
                format!(
                    "{} {} {}",
                    left.format(context),
                    op.to_uppercase(),
                    right.format(context)
                )
            }
        }
    }
}

impl<'a> Format for Vec<parser::Node<'a>> {
    fn format(&self, context: &mut Context) -> String {
        self.iter()
            .map(|expr| expr.format(context))
            .collect::<Vec<String>>()
            .join("\n")
    }
}

impl<'a> Format for parser::Node<'a> {
    fn format(&self, context: &mut Context) -> String {
        match self {
            parser::Node::If { expr, body } => {
                context.indentation += 2;
                let body = body.format(context);
                context.indentation -= 2;

                let expr = expr.format(context);

                format!(
                    "{0:indent$}if({expr})\n{body}{0:indent$}endif()\n",
                    "",
                    indent = context.indentation
                )
            }
            parser::Node::While { expr, body } => {
                context.indentation += 2;
                let body = body.format(context);
                context.indentation -= 2;

                let expr = expr.format(context);

                format!(
                    "{0:indent$}while({expr})\n{body}{0:indent$}endwhile()\n",
                    "",
                    indent = context.indentation
                )
            }
            parser::Node::Continue => {
                format!("{0:indent$}continue()\n", "", indent = context.indentation)
            }
            parser::Node::Break => {
                format!("{0:indent$}break()\n", "", indent = context.indentation)
            }
            parser::Node::Project { name, languages } => {
                let languages = languages
                    .as_ref()
                    .map(|langs| {
                        format!(
                            " {}",
                            langs
                                .iter()
                                .map(|lang| format!("{lang}"))
                                .collect::<Vec<_>>()
                                .join(" ")
                        )
                    })
                    .unwrap_or_else(String::new);

                format!(
                    "{0:indent$}project({name}{languages})\n",
                    "",
                    indent = context.indentation
                )
            }
        }
    }
}

impl std::fmt::Display for parser::Language {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            parser::Language::C => write!(f, "C"),
            parser::Language::Cxx => write!(f, "CXX"),
            parser::Language::CSharp => write!(f, "CSharp"),
            parser::Language::Cuda => write!(f, "CUDA"),
            parser::Language::ObjC => write!(f, "OBJC"),
            parser::Language::ObjCxx => write!(f, "OBJCXX"),
            parser::Language::Fortran => write!(f, "Fortran"),
            parser::Language::Hip => write!(f, "HIP"),
            parser::Language::Ispc => write!(f, "ISPC"),
            parser::Language::Swift => write!(f, "Swift"),
            parser::Language::Asm => write!(f, "ASM"),
            parser::Language::AsmNasm => write!(f, "ASM_NASM"),
            parser::Language::AsmMArmAsm => write!(f, "ASM_MARMASM"),
            parser::Language::AsmMasm => write!(f, "ASM_MASM"),
            parser::Language::AsmAtt => write!(f, "ASM-ATT"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn if_endif() {
        assert_eq!(
            parser::Node::If {
                expr: parser::Expr::Const("TRUE"),
                body: vec![]
            }
            .format(&mut Context::default()),
            "if(TRUE)\nendif()\n"
        );
    }
}
