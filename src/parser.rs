#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("failed to set language: {0}")]
    LanguageError(#[from] tree_sitter::LanguageError),
    #[error("failed to parse input")]
    ParseError,
    #[error("unexpected token: {0}")]
    UnexpectedToken(String),
    #[error("unexpected parse state")]
    UnexpectedParseState,
    #[error("failed to parse UTF-8: {0}")]
    Utf8Error(#[from] std::str::Utf8Error),
    #[error("failed to recognize language {0}")]
    UnexpectedLanguage(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'a> {
    Const(&'a str),
    Var(&'a str),
    Quoted(&'a str),
    Unary(&'a str, Box<Expr<'a>>),
    Binary(&'a str, Box<Expr<'a>>, Box<Expr<'a>>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Language {
    C,
    Cxx,
    CSharp,
    Cuda,
    ObjC,
    ObjCxx,
    Fortran,
    Hip,
    Ispc,
    Swift,
    Asm,
    AsmNasm,
    AsmMArmAsm,
    AsmMasm,
    AsmAtt,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Node<'a> {
    If {
        expr: Expr<'a>,
        body: Vec<Node<'a>>,
    },
    While {
        expr: Expr<'a>,
        body: Vec<Node<'a>>,
    },
    Continue,
    Break,
    Project {
        name: &'a str,
        languages: Option<Vec<Language>>,
    },
}

/// Helper struct to keep a reference to the source and implement consumption methods on.
struct Inner<'a> {
    source: &'a str,
}

impl<'a> Inner<'a> {
    fn node_text(&self, node: &tree_sitter::Node) -> Result<&'a str, Error> {
        Ok(node.utf8_text(self.source.as_bytes())?)
    }

    fn try_language_from(&self, node: &tree_sitter::Node) -> Result<Language, Error> {
        let text = self.node_text(node)?.to_uppercase();

        match text.as_str() {
            "C" => Ok(Language::C),
            "CXX" => Ok(Language::Cxx),
            "CSHARP" => Ok(Language::CSharp),
            "CUDA" => Ok(Language::Cuda),
            "OBJC" => Ok(Language::ObjC),
            "OBJCXX" => Ok(Language::ObjCxx),
            "FORTRAN" => Ok(Language::Fortran),
            "HIP" => Ok(Language::Hip),
            "ISCP" => Ok(Language::Ispc),
            "SWIFT" => Ok(Language::Swift),
            "ASM" => Ok(Language::Asm),
            "ASM_NASM" => Ok(Language::AsmNasm),
            "ASM_MARMASM" => Ok(Language::AsmMArmAsm),
            "ASM_MASM" => Ok(Language::AsmMasm),
            "ASM-ATT" => Ok(Language::AsmAtt),
            _ => Err(Error::UnexpectedLanguage(text.to_string())),
        }
    }

    fn consume_normal_var(
        &self,
        node: tree_sitter::Node,
        cursor: tree_sitter::TreeCursor,
    ) -> Result<Expr<'a>, Error> {
        let mut cursor = cursor;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "variable" => {
                    return Ok(Expr::Var(self.node_text(&child)?));
                }
                "$" | "{" | "}" => {}
                token => return Err(Error::UnexpectedToken(token.to_string())),
            }
        }

        Err(Error::UnexpectedParseState)
    }

    fn consume_variable_ref(
        &self,
        node: tree_sitter::Node,
        cursor: tree_sitter::TreeCursor,
    ) -> Result<Expr<'a>, Error> {
        let mut cursor = cursor;
        let inner_cursor = cursor.clone();

        for child in node.children(&mut cursor) {
            match child.kind() {
                "normal_var" => return self.consume_normal_var(child, inner_cursor),
                token => return Err(Error::UnexpectedToken(token.to_string())),
            }
        }

        Err(Error::UnexpectedParseState)
    }

    fn consume_quoted_argument(
        &self,
        node: tree_sitter::Node,
        cursor: tree_sitter::TreeCursor,
    ) -> Result<Expr<'a>, Error> {
        let mut cursor = cursor;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "\"" => {}
                "quoted_element" => {
                    return Ok(Expr::Quoted(self.node_text(&child)?));
                }
                token => return Err(Error::UnexpectedToken(token.to_string())),
            }
        }

        Err(Error::UnexpectedParseState)
    }

    fn consume_unquoted_argument(
        &self,
        node: tree_sitter::Node,
        cursor: tree_sitter::TreeCursor,
    ) -> Result<Expr<'a>, Error> {
        let mut cursor = cursor;
        let inner_cursor = cursor.clone();
        let children = node.children(&mut cursor);

        if children.len() == 0 {
            return Ok(Expr::Const(self.node_text(&node)?));
        }

        for child in children {
            match child.kind() {
                "variable_ref" => {
                    return self.consume_variable_ref(child, inner_cursor);
                }
                token => return Err(Error::UnexpectedToken(token.to_string())),
            }
        }

        Err(Error::UnexpectedParseState)
    }

    fn consume_argument(
        &self,
        node: tree_sitter::Node,
        cursor: tree_sitter::TreeCursor,
    ) -> Result<Expr<'a>, Error> {
        let mut cursor = cursor;
        let inner_cursor = cursor.clone();

        for child in node.children(&mut cursor) {
            match child.kind() {
                "quoted_argument" => {
                    return self.consume_quoted_argument(child, inner_cursor.clone())
                }
                "unquoted_argument" => {
                    return self.consume_unquoted_argument(child, inner_cursor.clone())
                }
                token => return Err(Error::UnexpectedToken(token.to_string())),
            }
        }

        Err(Error::UnexpectedParseState)
    }

    fn consume_argument_list(
        &self,
        node: tree_sitter::Node,
        cursor: tree_sitter::TreeCursor,
    ) -> Result<Expr<'a>, Error> {
        let mut cursor = cursor;
        let inner_cursor = cursor.clone();
        let children = node.children(&mut cursor);

        let mut expressions = children
            .map(|child| self.consume_argument(child, inner_cursor.clone()))
            .collect::<Result<Vec<_>, Error>>()?;

        if expressions.len() == 2 {
            let second = expressions.remove(1);
            let first = expressions.remove(0);

            match first {
                Expr::Const(value) => {
                    let uppercase = value.to_uppercase();

                    match uppercase.as_str() {
                        "NOT" => Ok(Expr::Unary(value, Box::new(second))),
                        // Existence checks
                        "COMMAND" | "POLICY" | "TARGET" | "TEST" => {
                            Ok(Expr::Unary(value, Box::new(second)))
                        }
                        // File operations
                        "EXISTS" | "IS_DIRECTORY" | "IS_SYMLINK" | "IS_ABSOLUTE" => {
                            Ok(Expr::Unary(value, Box::new(second)))
                        }
                        _ => Ok(first),
                    }
                }
                _ => Ok(first),
            }
        } else if expressions.len() == 3 {
            let third = expressions.remove(2);
            let second = expressions.remove(1);
            let first = expressions.remove(0);

            match second {
                Expr::Const(value) => {
                    let uppercase = value.to_uppercase();

                    match uppercase.as_str() {
                        "AND" | "OR" => Ok(Expr::Binary(value, Box::new(first), Box::new(third))),
                        // TODO: verify first and third are strings or variables
                        "LESS" | "GREATER" | "EQUAL" | "LESS_EQUAL" | "GREATER_EQUAL"
                        | "STRLESS" | "STRGREATER" | "STREQUAL" | "STRLESS_EQUAL"
                        | "STRGREATER_EQUAL" => {
                            Ok(Expr::Binary(value, Box::new(first), Box::new(third)))
                        }
                        "IN_LIST" => Ok(Expr::Binary(value, Box::new(first), Box::new(third))),
                        _ => Err(Error::UnexpectedParseState),
                    }
                }
                _ => Ok(first),
            }
        } else {
            Ok(expressions.pop().unwrap())
        }
    }

    fn consume_if_command(
        &self,
        node: tree_sitter::Node,
        cursor: tree_sitter::TreeCursor,
    ) -> Result<Expr<'a>, Error> {
        let mut cursor = cursor;
        let inner_cursor = cursor.clone();

        for child in node.children(&mut cursor) {
            match child.kind() {
                "if" | "(" | ")" => {}
                "argument_list" => {
                    return self.consume_argument_list(child, inner_cursor);
                }
                token => return Err(Error::UnexpectedToken(token.to_string())),
            }
        }

        Err(Error::UnexpectedParseState)
    }

    fn consume_if_condition(
        &self,
        node: tree_sitter::Node,
        cursor: tree_sitter::TreeCursor,
    ) -> Result<Node<'a>, Error> {
        let mut cursor = cursor;
        let inner_cursor = cursor.clone();
        let mut expr: Option<Expr> = None;
        let mut body: Option<Vec<Node>> = None;

        for child in node.children(&mut cursor) {
            let inner_cursor = inner_cursor.clone();

            match child.kind() {
                "if_command" => {
                    expr.replace(self.consume_if_command(child, inner_cursor)?);
                }
                "body" => {
                    body.replace(self.consume_nodes(child, inner_cursor)?);
                }
                "endif_command" => {}
                token => return Err(Error::UnexpectedToken(token.to_string())),
            }
        }

        let expr = expr.unwrap();
        let body = body.unwrap_or_default();

        Ok(Node::If { expr, body })
    }

    fn consume_while_command(
        &self,
        node: tree_sitter::Node,
        cursor: tree_sitter::TreeCursor,
    ) -> Result<Expr<'a>, Error> {
        let mut cursor = cursor;
        let inner_cursor = cursor.clone();

        for child in node.children(&mut cursor) {
            let inner_cursor = inner_cursor.clone();

            match child.kind() {
                "while" | "(" | ")" => {}
                "argument_list" => {
                    return self.consume_argument_list(child, inner_cursor);
                }
                token => return Err(Error::UnexpectedToken(token.to_string())),
            }
        }

        Err(Error::UnexpectedParseState)
    }

    fn consume_while_loop(
        &self,
        node: tree_sitter::Node,
        cursor: tree_sitter::TreeCursor,
    ) -> Result<Node<'a>, Error> {
        let mut cursor = cursor;
        let inner_cursor = cursor.clone();
        let mut expr: Option<Expr> = None;
        let mut body: Option<Vec<Node>> = None;

        for child in node.children(&mut cursor) {
            let inner_cursor = inner_cursor.clone();

            match child.kind() {
                "while_command" => {
                    expr.replace(self.consume_while_command(child, inner_cursor)?);
                }
                "body" => {
                    body.replace(self.consume_nodes(child, inner_cursor)?);
                }
                "endwhile_command" => {}
                token => return Err(Error::UnexpectedToken(token.to_string())),
            }
        }

        let expr = expr.unwrap();
        let body = body.unwrap_or_default();

        Ok(Node::While { expr, body })
    }

    fn consume_project(
        &self,
        node: tree_sitter::Node,
        cursor: tree_sitter::TreeCursor,
    ) -> Result<Node<'a>, Error> {
        let inner_cursor = cursor.clone();
        let mut cursor = cursor;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "argument_list" => {
                    let mut inner_cursor = inner_cursor.clone();
                    let mut arguments_iter = child.children(&mut inner_cursor);
                    let name = arguments_iter
                        .next()
                        .map(|node| self.node_text(&node))
                        .ok_or_else(|| Error::UnexpectedParseState)??;

                    let languages = arguments_iter
                        .map(|node| self.try_language_from(&node))
                        .collect::<Result<Vec<_>, _>>()?;

                    let languages = (languages.len() != 0).then_some(languages);

                    return Ok(Node::Project { name, languages });
                }
                "identifier" | "(" | ")" => {}
                token => return Err(Error::UnexpectedToken(token.to_string())),
            }
        }

        Err(Error::UnexpectedParseState)
    }

    fn consume_normal_command(
        &self,
        node: tree_sitter::Node,
        cursor: tree_sitter::TreeCursor,
    ) -> Result<Node<'a>, Error> {
        let inner_cursor = cursor.clone();
        let mut cursor = cursor;

        for child in node.children(&mut cursor) {
            match child.kind() {
                "identifier" => {
                    let identifier = self.node_text(&child)?.to_lowercase();

                    match identifier.as_str() {
                        "continue" => return Ok(Node::Continue),
                        "break" => return Ok(Node::Break),
                        "project" => return self.consume_project(node, inner_cursor.clone()),
                        _ => return Err(Error::UnexpectedToken(identifier)),
                    };
                }
                "(" | ")" => {}
                token => return Err(Error::UnexpectedToken(token.to_string())),
            }
        }

        Err(Error::UnexpectedParseState)
    }

    fn consume_nodes(
        &self,
        node: tree_sitter::Node,
        cursor: tree_sitter::TreeCursor,
    ) -> Result<Vec<Node<'a>>, Error> {
        let inner_cursor = cursor.clone();
        let mut cursor = cursor;

        node.children(&mut cursor)
            .into_iter()
            .map(|child| match child.kind() {
                "if_condition" => self.consume_if_condition(child, inner_cursor.clone()),
                "while_loop" => self.consume_while_loop(child, inner_cursor.clone()),
                "normal_command" => self.consume_normal_command(child, inner_cursor.clone()),
                token => Err(Error::UnexpectedToken(token.to_string())),
            })
            .collect()
    }
}

/// Parse `source` into a vector of nodes.
///
/// Note that tokens are stored as references into the source hence no capitalization happens yet.
pub fn parse<'a>(source: &'a str) -> Result<Vec<Node<'a>>, Error> {
    let mut parser = tree_sitter::Parser::new();

    parser.set_language(tree_sitter_cmake::language())?;

    let tree = parser
        .parse(source.as_bytes(), None)
        .ok_or(Error::ParseError)?;

    let inner = Inner { source };
    Ok(inner.consume_nodes(tree.root_node(), tree.walk())?)
}

#[cfg(test)]
mod tests {
    use super::*;

    impl<'a> Expr<'a> {
        fn matches<'b>(&self, expr: Expr<'b>) -> bool {
            *self == expr
        }

        fn matches_unary<'b>(&self, value: &str, expr: Expr<'b>) -> bool {
            *self == Expr::Unary(value, Box::new(expr))
        }

        fn matches_binary<'b>(&self, value: &str, left: Expr<'b>, right: Expr<'b>) -> bool {
            *self == Expr::Binary(value, Box::new(left), Box::new(right))
        }
    }

    impl<'a> Node<'a> {
        fn if_expr(&'a self) -> &'a Expr<'a> {
            match &self {
                Node::If { expr, .. } => expr,
                _ => panic!("not a Node::If<>"),
            }
        }

        fn as_while_parts(&'a self) -> (&'a Expr<'a>, &'a Vec<Node<'a>>) {
            match &self {
                Node::While { expr, body } => (expr, body),
                _ => panic!("not a Node::While<>"),
            }
        }

        fn as_project_parts(&'a self) -> (&'a str, &'a Option<Vec<Language>>) {
            match &self {
                Node::Project { name, languages } => (name, languages),
                _ => panic!("not a Node::Project<>"),
            }
        }
    }

    // if - basic expressions
    #[test]
    fn if_condition_constant() {
        let nodes = parse(r#"if(TRUE)endif()"#).unwrap();
        assert!(nodes[0].if_expr().matches(Expr::Const("TRUE")));
    }

    #[test]
    fn if_condition_variable() {
        let nodes = parse(r#"if(${foo})endif()"#).unwrap();
        assert!(nodes[0].if_expr().matches(Expr::Var("foo")));
    }

    #[test]
    fn if_condition_quoted() {
        let nodes = parse(r#"if("foo")endif()"#).unwrap();
        assert!(nodes[0].if_expr().matches(Expr::Quoted("foo")));
    }

    // if - logic operators
    #[test]
    fn if_not() {
        let nodes = parse(r#"if(NOT TRUE)endif()"#).unwrap();
        assert!(nodes[0].if_expr().matches_unary("NOT", Expr::Const("TRUE")));
    }

    #[test]
    fn if_and() {
        let nodes = parse(r#"if("foo" AND ${bar})endif()"#).unwrap();
        assert!(nodes[0]
            .if_expr()
            .matches_binary("AND", Expr::Quoted("foo"), Expr::Var("bar")));
    }

    #[test]
    fn if_or() {
        let nodes = parse(r#"if(TRUE OR FALSE)endif()"#).unwrap();
        assert!(nodes[0]
            .if_expr()
            .matches_binary("OR", Expr::Const("TRUE"), Expr::Const("FALSE")));
    }

    // if - existence checks
    #[test]
    fn if_command() {
        let nodes = parse(r#"if(COMMAND cmake_minimum_version)endif()"#).unwrap();
        assert!(nodes[0]
            .if_expr()
            .matches_unary("COMMAND", Expr::Const("cmake_minimum_version")));
    }

    #[test]
    fn if_policy() {
        let nodes = parse(r#"if(POLICY CMP123)endif()"#).unwrap();
        assert!(nodes[0]
            .if_expr()
            .matches_unary("POLICY", Expr::Const("CMP123")));
    }

    #[test]
    fn if_target() {
        let nodes = parse(r#"if(TARGET somelib0)endif()"#).unwrap();
        assert!(nodes[0]
            .if_expr()
            .matches_unary("TARGET", Expr::Const("somelib0")));
    }

    // if - file operations

    // if - comparisons
    #[test]
    fn if_condition_string_comparisons() {
        let nodes = parse(
            r#"if("foo" LESS "bar")
endif()
if(${foo} GREATER "bar")
endif()"#,
        )
        .unwrap();

        assert!(nodes[0].if_expr().matches_binary(
            "LESS",
            Expr::Quoted("foo"),
            Expr::Quoted("bar")
        ));

        assert!(nodes[1].if_expr().matches_binary(
            "GREATER",
            Expr::Var("foo"),
            Expr::Quoted("bar")
        ));
    }

    #[test]
    fn while_condition() {
        let nodes = parse("while(TRUE)\ncontinue()\nbreak()\nendwhile()").unwrap();
        let (expr, body) = nodes[0].as_while_parts();
        assert!(matches!(expr, Expr::Const("TRUE")));
        assert!(matches!(body[0], Node::Continue));
        assert!(matches!(body[1], Node::Break));
    }

    #[test]
    fn project_basic() {
        let nodes = parse("project(foo)").unwrap();
        assert!(matches!(
            nodes[0],
            Node::Project {
                name: "foo",
                languages: None
            }
        ));
    }

    #[test]
    fn project_languages() {
        let nodes = parse("project(foo C CXX)").unwrap();
        let (name, languages) = nodes[0].as_project_parts();
        assert_eq!(name, "foo");

        let languages = languages.as_ref().unwrap();
        assert_eq!(languages.len(), 2);
        assert!(matches!(languages[0], Language::C));
        assert!(matches!(languages[1], Language::Cxx));
    }
}
