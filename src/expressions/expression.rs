use std::fmt::Display;

#[derive(Debug)]
pub enum Types {
    BoolType,
    DynamicType,
    IntType,
    ListType,
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
            Self::ListType => write!(f, "ListType"),
            Self::MapType => write!(f, "MapType"),
            Self::RealType => write!(f, "RealType"),
            Self::StringType => write!(f, "StringType"),
            Self::CustomType(id) => write!(f, "{id}"),
        }
    }
}

pub trait IExpression {
    fn exec(&self);
    fn to_json(&self) -> String;
}

pub struct InitializerList {
    pub field_count: u32,
    pub values: Vec<Box<dyn IExpression>>,
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
    pub default_value: Option<Box<Expression>>,
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
    // Type, ID, RHS
    Declaration(Types, String, Option<Box<Expression>>),
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
    FunctionCall(String, Vec<FunctionArgument>),
    // ID
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
    // Guard, Body
    WhileLoop(Box<Expression>, Scope),
    // Iterator Variable Name, Expression
    ForLoop(Box<Expression>, Box<Expression>, Scope),
    // EXPR
    ReturnExpression(Box<Expression>),
    // Nop
    NOP,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScopeLevels {
    Block,
    Function,
    Global,
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
            Self::Equals => write!(f, "equals"),
            Self::NotEqualsTo => write!(f, "not_equals_to"),
        }
    }
}

impl Display for ComparisonOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LessThan => write!(f, "less_than"),
            Self::LessThanOrEqual => write!(f, "less_than_or_equals"),
            Self::GreaterThan => write!(f, "greater_than"),
            Self::GreaterThanOrEqual => write!(f, "greater_than_or_equals"),
        }
    }
}

impl Display for ArithmeticOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus => write!(f, "plus"),
            Self::Minus => write!(f, "minus"),
            Self::Multiply => write!(f, "multiple"),
            Self::Divide => write!(f, "divide"),
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
            Self::Declaration(typename, id, rhs) => {
                let rhs = match rhs {
                    Some(v) => v.to_json(),
                    None => String::from("\"null\""),
                };

                return format!("{{\"type\": \"declaration\", \"typename\": \"{}\", \"id\": \"{}\", \"rhs\": {}}}", typename, id, rhs);
            }
            Self::Dto(name, fields) => {
                let mut strings = Vec::new();

                for f in fields.iter() {
                    strings.push(format!(
                        "{{\"field_type\": \"{}\", \"field_name\": \"{}\"}}",
                        f.field_type.to_string(),
                        f.field_name
                    ));
                }

                return format!(
                    "{{\"type\": \"dto\", \"name\": \"{}\", \"fields\": [{}]}}",
                    name,
                    strings.join(",")
                );
            }
            Self::FunctionDefinition(access_modifier, typename, name, parameters, body) => {
                let mut strings = Vec::new();

                for f in parameters.iter() {
                    strings.push(format!(
                        "{{\"type\": \"{}\", \"name\": \"{}\"}}",
                        f.typename.to_string(),
                        f.name
                    ));
                }

                let body = match body {
                    Some(b) => b.to_json(),
                    None => String::from("\"null\""),
                };

                return format!(
                    "{{\"type\": \"function\", \"access_modifier\": \"{}\", \"function_type\": \"{}\", \"name\": \"{}\", \"parameters\": [{}], \"body\": {}}}",
                    access_modifier,
                    typename,
                    name,
                    strings.join(","),
                    body,
                );
            }
            Self::FunctionCall(id, arguments) => {
                let mut strings = Vec::new();

                for a in arguments.iter() {
                    strings.push(a.expr.to_json());
                }

                return format!(
                    "{{\"type\": \"function_call\", \"id\": \"{}\", \"arguments\": [{}]}}",
                    id,
                    strings.join(","),
                );
            }
            Self::Module(name, body) => {
                format!(
                    "{{\"module_name\":\"{}\", \"body\": [{}]}}",
                    name,
                    body.to_json(),
                )
            }
            Self::TraitDefinition(name, functions) => {
                let mut strings: Vec<String> = Vec::new();

                for func in functions.iter() {
                    strings.push(func.to_json());
                }

                return format!(
                    "{{\"type\": \"trait\", \"name\": \"{}\", \"body\": [{}]}}",
                    name,
                    strings.join(",")
                );
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
            Self::ReturnExpression(expression) => format!(
                "{{\"type\": \"return\", \"expr\": {}}}",
                expression.to_json()
            ),
            Self::NOP => format!("{{\"type\": \"nop\"}}"),
        }
    }
}
