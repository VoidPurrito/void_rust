use std::fmt::Display;

#[derive(Debug)]
pub enum Types {
    BoolType,
    DynamicType,
    IntType,
    // ListType,
    MapType,
    RealType,
    StringType,
    CustomType(String),
}

#[derive(Debug)]
pub enum AccessModifiers {
    Public,
    Private,
}

impl Display for AccessModifiers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Public => write!(f, "public"),
            Self::Private => write!(f, "private"),
        }
    }
}

impl Display for Types {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BoolType => write!(f, "Booltype"),
            Self::DynamicType => write!(f, "DynamicType"),
            Self::IntType => write!(f, "IntType"),
            // Self::ListType => write!(f, "ListType"),
            Self::MapType => write!(f, "MapType"),
            Self::RealType => write!(f, "RealType"),
            Self::StringType => write!(f, "StringType"),
            Self::CustomType(id) => write!(f, "{id}"),
        }
    }
}

pub struct Scope {
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BooleanOperator {
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EqualityOperator {
    Equals,
    NotEqualsTo,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ComparisonOperator {
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

pub enum ArithmeticOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Mod,
}

pub struct DtoFieldDefinition {
    pub field_type: Types,
    pub field_name: String,
}

pub struct FunctionParameter {
    pub typename: Types,
    pub name: String,
    // pub default_value: Option<Box<Expression>>,
}

pub struct FunctionArgument {
    pub expr: Expression,
}

pub enum Expression {
    // ID, RHS
    Assignment(Box<Expression>, Box<Expression>),
    // Boolean Value
    BooleanConstant(bool),
    // Operator, LHS, RHS
    BooleanExpression(BooleanOperator, Box<Expression>, Box<Expression>),
    // Operator, LHS, RHS
    EqualityExpression(EqualityOperator, Box<Expression>, Box<Expression>),
    // Operator, LHS, RHS
    ComparisonExpression(ComparisonOperator, Box<Expression>, Box<Expression>),
    // Operator, LHS, RHS
    ArithmeticExpression(ArithmeticOperator, Box<Expression>, Box<Expression>),
    // Type, Name, RHS
    Declaration(AccessModifiers, Types, String, Option<Box<Expression>>),
    // ID, Field Definitions
    Dto(String, Vec<DtoFieldDefinition>),
    // Access Modifier, Type, Name, Parameters, Body
    FunctionDefinition(
        AccessModifiers,
        Types,
        String,
        Vec<FunctionParameter>,
        Option<Scope>,
    ),
    // Function Name, Arguments
    FunctionCall(String, Vec<FunctionArgument>),
    // Name
    IdentifierExpression(String),
    // Int Value
    IntegerConstant(i64),
    // Module Name, Body
    Module(String, Scope),
    // Real Value
    RealConstant(f64),
    // String Value
    StringConstant(String),
    // Name, Function Definitions
    TraitDefinition(String, Vec<Box<Expression>>),
    // Class Name, Instance Variables, Method Definitions
    ClassDefinition(String, Vec<Box<Expression>>, Vec<Box<Expression>>),
    // Guard, Body
    WhileLoop(Box<Expression>, Scope),
    // Iterator Variable Name, Expression
    ForLoop(Box<Expression>, Box<Expression>, Scope),
    // Guard, Body, Else If
    If(Option<Box<Expression>>, Scope, Option<Box<Expression>>),
    // Expression, Next Chained Expression
    // eg: a.b.c, a.b.c()
    ChainedExpression(Box<Expression>, Box<Expression>),
    // Return Value Expression
    ReturnExpression(Box<Expression>),
    Break,
    Continue,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScopeType {
    Block,
    Function,
    Global,
    Loop,
    Method,
}

impl Scope {
    pub fn to_json(&self) -> String {
        let mut expression_jsons: Vec<String> = Vec::new();

        for expr in self.body.iter() {
            expression_jsons.push(expr.to_json());
        }

        format!("[{}]", expression_jsons.join(","))
    }
}

impl Display for BooleanOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LogicalOr => write!(f, "or"),
            Self::LogicalAnd => write!(f, "and"),
        }
    }
}

impl Display for EqualityOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Equals => write!(f, "=="),
            Self::NotEqualsTo => write!(f, "!="),
        }
    }
}

impl Display for ComparisonOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LessThan => write!(f, "<"),
            Self::LessThanOrEqual => write!(f, "<="),
            Self::GreaterThan => write!(f, ">"),
            Self::GreaterThanOrEqual => write!(f, ">="),
        }
    }
}

impl Display for ArithmeticOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Mod => write!(f, "mod"),
        }
    }
}

impl Expression {
    pub fn to_json(&self) -> String {
        match self {
            Self::Assignment(id, rhs) => format!(
                "{{\"type\": \"asignment\", \"lhs\": {}, \"rhs\": {}}}",
                id.to_json(),
                rhs.to_json()
            ),
            Self::ArithmeticExpression(op, lhs, rhs) => {
                format!(
                    "{{\"type\": \"{}\", \"lhs\": {}, \"rhs\": {}}}",
                    op,
                    lhs.to_json(),
                    rhs.to_json()
                )
            }
            Self::BooleanExpression(op, lhs, rhs) => {
                format!(
                    "{{\"type\": \"{}\", \"lhs\": {}, \"rhs\": {}}}",
                    op,
                    lhs.to_json(),
                    rhs.to_json()
                )
            }
            Self::EqualityExpression(op, lhs, rhs) => {
                format!(
                    "{{\"type\": \"{}\", \"lhs\": {}, \"rhs\": {}}}",
                    op,
                    lhs.to_json(),
                    rhs.to_json()
                )
            }
            Self::ComparisonExpression(op, lhs, rhs) => {
                format!(
                    "{{\"type\": \"{}\", \"lhs\": {}, \"rhs\": {}}}",
                    op,
                    lhs.to_json(),
                    rhs.to_json()
                )
            }
            Self::IdentifierExpression(value) => {
                format!("{{\"type\": \"identifier\", \"value\": \"{}\"}}", value)
            }
            Self::IntegerConstant(value) => {
                format!("{{\"type\": \"integer\", \"value\": \"{}\"}}", value)
            }
            Self::RealConstant(value) => {
                format!("{{\"type\": \"real\", \"value\": \"{}\"}}", value)
            }
            Self::BooleanConstant(value) => {
                format!("{{\"type\": \"bool\", \"value\": \"{}\"}}", value)
            }
            Self::StringConstant(value) => format!(
                "{{\"type\": \"string\", \"value\": \"{}\"}}",
                value.replace("\n", "\\n")
            ),
            Self::Declaration(access_modifier, typename, id, rhs) => {
                let rhs = match rhs {
                    Some(v) => v.to_json(),
                    None => String::from("\"null\""),
                };

                return format!("{{\"type\": \"declaration\", \"access_modifier\": \"{}\", \"typename\": \"{}\", \"id\": \"{}\", \"rhs\": {}}}", access_modifier, typename, id, rhs);
            }
            Self::Dto(name, fields) => {
                let strings = fields
                    .iter()
                    .map(|f| {
                        format!(
                            "{{\"field_type\": \"{}\", \"field_name\": \"{}\"}}",
                            f.field_type.to_string(),
                            f.field_name
                        )
                    })
                    .collect::<Vec<String>>()
                    .join(",");

                return format!(
                    "{{\"type\": \"dto\", \"name\": \"{}\", \"fields\": [{}]}}",
                    name, strings,
                );
            }
            Self::FunctionDefinition(access_modifier, typename, name, parameters, body) => {
                let strings = parameters
                    .iter()
                    .map(|p| {
                        format!(
                            "{{\"type\": \"{}\", \"name\": \"{}\"}}",
                            p.typename.to_string(),
                            p.name
                        )
                    })
                    .collect::<Vec<String>>()
                    .join(",");

                let body = match body {
                    Some(b) => b.to_json(),
                    None => String::from("\"null\""),
                };

                return format!(
                    "{{\"type\": \"function\", \"access_modifier\": \"{}\", \"function_type\": \"{}\", \"name\": \"{}\", \"parameters\": [{}], \"body\": {}}}",
                    access_modifier,
                    typename,
                    name,
                    strings,
                    body,
                );
            }
            Self::FunctionCall(id, arguments) => {
                let strings = arguments
                    .iter()
                    .map(|x| x.expr.to_json())
                    .collect::<Vec<String>>()
                    .join(",");

                return format!(
                    "{{\"type\": \"function_call\", \"id\": \"{}\", \"arguments\": [{}]}}",
                    id, strings,
                );
            }
            Self::Module(name, body) => {
                format!(
                    "{{\"module_name\": \"{}\", \"body\": [{}]}}",
                    name,
                    body.to_json(),
                )
            }
            Self::TraitDefinition(name, functions) => {
                let strings = functions
                    .iter()
                    .map(|f| f.to_json())
                    .collect::<Vec<String>>()
                    .join(",");

                return format!(
                    "{{\"type\": \"trait\", \"name\": \"{}\", \"body\": [{}]}}",
                    name, strings
                );
            }
            Self::ClassDefinition(name, instance_variables, methods) => {
                let variables_str = instance_variables
                    .iter()
                    .map(|x| x.to_json())
                    .collect::<Vec<String>>()
                    .join(",");
                let methods_str = methods
                    .iter()
                    .map(|x| x.to_json())
                    .collect::<Vec<String>>()
                    .join(",");

                format!(
                    "{{\"type\": \"class\", \"name\": \"{}\", \"instance_variabled\": [{}], \"methods\": [{}]}}",
                    name, variables_str, methods_str
                )
            }
            Self::WhileLoop(guard, body) => {
                let guard_str = guard.to_json();
                let body_str = body.to_json();

                format!(
                    "{{\"type\": \"while\", \"guard\": {}, \"body\": {}}}",
                    guard_str, body_str
                )
            }
            Self::ForLoop(iterator_var_name, iterator_exp, body) => format!(
                "{{\"type\": \"for\", \"var\": {}, \"iterator\": {}, \"body\": {}}}",
                iterator_var_name.to_json(),
                iterator_exp.to_json(),
                body.to_json()
            ),
            Self::If(guard, body, next) => {
                let _type: &str;

                let guard_json = match guard {
                    Some(e) => {
                        _type = "if";
                        e.to_json()
                    }
                    None => {
                        _type = "else";
                        String::from("null")
                    }
                };

                let body_json = body.to_json();

                let next_json = match next {
                    Some(e) => e.to_json(),
                    None => String::from("null"),
                };

                format!(
                    "{{\"type\": \"{}\", \"guard\": {}, \"body\": {}, \"next\": {}}}",
                    _type, guard_json, body_json, next_json
                )
            }
            Self::ReturnExpression(expression) => format!(
                "{{\"type\": \"return\", \"expr\": {}}}",
                expression.to_json()
            ),
            Self::ChainedExpression(lhs, rhs) => format!(
                "{{\"type\": \".\", \"lhs\": {}, \"rhs\": {}}}",
                lhs.to_json(),
                rhs.to_json()
            ),
            Self::Break => format!("{{\"type\": \"break\"}}"),
            Self::Continue => format!("{{\"type\": \"continue\"}}"),
        }
    }
}
