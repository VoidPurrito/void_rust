use std::{
    collections::{HashMap, VecDeque},
    error::Error,
    fmt,
};

use crate::expressions::expression::{
    AccessModifiers, ArithmeticOperator, BooleanOperator, EqualityOperator, Expression, Scope,
    Types,
};

#[derive(Clone)]
pub struct Object {
    pub access_modifier: AccessModifiers,
    pub readonly: bool,
    pub typename: Types,
    pub name: String,
    pub value: RuntimeValue,
}

impl Object {
    pub fn none() -> Self {
        Self {
            access_modifier: AccessModifiers::Public,
            readonly: true,
            typename: Types::NoneType,
            name: String::from(""),
            value: RuntimeValue::NoneValue,
        }
    }

    pub fn int(value: i64) -> Self {
        Self {
            access_modifier: AccessModifiers::Private,
            readonly: true,
            typename: Types::NoneType,
            name: value.to_string(),
            value: RuntimeValue::IntegerValue(value),
        }
    }

    pub fn real(value: f64) -> Self {
        Self {
            access_modifier: AccessModifiers::Private,
            readonly: true,
            typename: Types::NoneType,
            name: value.to_string(),
            value: RuntimeValue::RealValue(value),
        }
    }

    pub fn bool(value: bool) -> Self {
        Self {
            access_modifier: AccessModifiers::Private,
            readonly: true,
            typename: Types::NoneType,
            name: value.to_string(),
            value: RuntimeValue::BoolValue(value),
        }
    }

    pub fn string(value: String) -> Self {
        Self {
            access_modifier: AccessModifiers::Private,
            readonly: true,
            typename: Types::NoneType,
            name: value.to_string(),
            value: RuntimeValue::StringValue(value),
        }
    }

    pub fn arithmetic_operation(
        op: ArithmeticOperator,
        lhs: Object,
        rhs: Object,
    ) -> Result<Object, RuntimeError> {
        if lhs.typename != rhs.typename {
            return Err(RuntimeError {
                message: format!(
                    "cannot mix types in arithmetic operators: {} + {} is invlaid",
                    lhs.typename, rhs.typename
                ),
            });
        }

        let value = match op {
            ArithmeticOperator::Plus => match lhs.value {
                RuntimeValue::IntegerValue(lhs_value) => match rhs.value {
                    RuntimeValue::IntegerValue(rhs_value) => {
                        RuntimeValue::IntegerValue(lhs_value + rhs_value)
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: format!(
                                "cannot mix types in arithmetic operators: {} + {} is invlaid",
                                lhs.typename, rhs.typename
                            ),
                        })
                    }
                },
                RuntimeValue::RealValue(lhs_value) => match rhs.value {
                    RuntimeValue::RealValue(rhs_value) => {
                        RuntimeValue::RealValue(lhs_value + rhs_value)
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: format!(
                                "cannot mix types in arithmetic operators: {} + {} is invlaid",
                                lhs.typename, rhs.typename
                            ),
                        })
                    }
                },
                RuntimeValue::StringValue(lhs_value) => match rhs.value {
                    RuntimeValue::StringValue(rhs_value) => {
                        let mut ret_string = String::new();
                        ret_string.push_str(&*lhs_value);
                        ret_string.push_str(&*rhs_value);
                        RuntimeValue::StringValue(ret_string)
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: format!(
                                "cannot mix types in string concatenations: {} + {} is invlaid",
                                lhs.typename, rhs.typename
                            ),
                        })
                    }
                },
                _ => {
                    return Err(RuntimeError {
                        message: format!(
                            "{} has invalid type {} for + expression",
                            lhs.name, lhs.typename
                        ),
                    })
                }
            },
            ArithmeticOperator::Minus => match lhs.value {
                RuntimeValue::IntegerValue(lhs_value) => match rhs.value {
                    RuntimeValue::IntegerValue(rhs_value) => {
                        RuntimeValue::IntegerValue(lhs_value - rhs_value)
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: format!(
                                "cannot mix types in arithmetic operators: {} - {} is invlaid",
                                lhs.typename, rhs.typename
                            ),
                        })
                    }
                },
                RuntimeValue::RealValue(lhs_value) => match rhs.value {
                    RuntimeValue::RealValue(rhs_value) => {
                        RuntimeValue::RealValue(lhs_value - rhs_value)
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: format!(
                                "cannot mix types in arithmetic operators: {} - {} is invlaid",
                                lhs.typename, rhs.typename
                            ),
                        })
                    }
                },
                _ => {
                    return Err(RuntimeError {
                        message: format!(
                            "{} has invalid type {} for - expression",
                            lhs.name, lhs.typename
                        ),
                    })
                }
            },
            ArithmeticOperator::Multiply => match lhs.value {
                RuntimeValue::IntegerValue(lhs_value) => match rhs.value {
                    RuntimeValue::IntegerValue(rhs_value) => {
                        RuntimeValue::IntegerValue(lhs_value * rhs_value)
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: format!(
                                "cannot mix types in arithmetic operators: {} * {} is invlaid",
                                lhs.typename, rhs.typename
                            ),
                        })
                    }
                },
                RuntimeValue::RealValue(lhs_value) => match rhs.value {
                    RuntimeValue::RealValue(rhs_value) => {
                        RuntimeValue::RealValue(lhs_value * rhs_value)
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: format!(
                                "cannot mix types in arithmetic operators: {} * {} is invlaid",
                                lhs.typename, rhs.typename
                            ),
                        })
                    }
                },
                _ => {
                    return Err(RuntimeError {
                        message: format!(
                            "{} has invalid type {} for * expression",
                            lhs.name, lhs.typename
                        ),
                    })
                }
            },
            ArithmeticOperator::Divide => match lhs.value {
                RuntimeValue::IntegerValue(lhs_value) => match rhs.value {
                    RuntimeValue::IntegerValue(rhs_value) => {
                        RuntimeValue::IntegerValue(lhs_value / rhs_value)
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: format!(
                                "cannot mix types in arithmetic operators: {} / {} is invlaid",
                                lhs.typename, rhs.typename
                            ),
                        })
                    }
                },
                RuntimeValue::RealValue(lhs_value) => match rhs.value {
                    RuntimeValue::RealValue(rhs_value) => {
                        RuntimeValue::RealValue(lhs_value / rhs_value)
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: format!(
                                "cannot mix types in arithmetic operators: {} / {} is invlaid",
                                lhs.typename, rhs.typename
                            ),
                        })
                    }
                },
                _ => {
                    return Err(RuntimeError {
                        message: format!(
                            "{} has invalid type {} for / expression",
                            lhs.name, lhs.typename
                        ),
                    })
                }
            },
            ArithmeticOperator::Mod => match lhs.value {
                RuntimeValue::IntegerValue(lhs_value) => match rhs.value {
                    RuntimeValue::IntegerValue(rhs_value) => {
                        RuntimeValue::IntegerValue(lhs_value % rhs_value)
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: format!(
                                "cannot mix types in arithmetic operators: {} % {} is invlaid",
                                lhs.typename, rhs.typename
                            ),
                        })
                    }
                },
                _ => {
                    return Err(RuntimeError {
                        message: format!(
                            "{} has invalid type {} for % expression",
                            lhs.name, lhs.typename
                        ),
                    })
                }
            },
        };

        return Ok(Object {
            access_modifier: AccessModifiers::Private,
            readonly: true,
            typename: lhs.typename,
            name: String::from(""),
            value,
        });
    }

    pub fn boolean_operation(
        op: BooleanOperator,
        lhs: Object,
        rhs: Object,
    ) -> Result<Object, RuntimeError> {
        if lhs.typename != Types::BoolType && rhs.typename != Types::BoolType {
            return Err(RuntimeError {
                message: format!(""),
            });
        }

        if lhs.typename != Types::BoolType {
            return Err(RuntimeError {
                message: format!(""),
            });
        }

        if rhs.typename != Types::BoolType {
            return Err(RuntimeError {
                message: format!(""),
            });
        }

        let value = match lhs.value {
            RuntimeValue::BoolValue(lhs_value) => match rhs.value {
                RuntimeValue::BoolValue(rhs_value) => match op {
                    BooleanOperator::LogicalAnd => lhs_value && rhs_value,
                    BooleanOperator::LogicalOr => lhs_value || rhs_value,
                },
                _ => {
                    return Err(RuntimeError {
                        message: format!(""),
                    })
                }
            },
            _ => {
                return Err(RuntimeError {
                    message: format!(""),
                })
            }
        };

        return Ok(Object {
            access_modifier: AccessModifiers::Private,
            readonly: true,
            typename: Types::BoolType,
            name: String::from(""),
            value: RuntimeValue::BoolValue(value),
        });
    }

    pub fn equality_operation(
        op: EqualityOperator,
        lhs: Object,
        rhs: Object,
    ) -> Result<Object, RuntimeError> {
        let mut result = Object {
            access_modifier: AccessModifiers::Private,
            typename: Types::BoolType,
            readonly: false,
            name: String::from(""),
            value: RuntimeValue::BoolValue(false),
        };

        if lhs.typename != rhs.typename {
            return Ok(result);
        }

        let b_result = match op {
            EqualityOperator::Equals => match lhs.value {
                RuntimeValue::BoolValue(b1) => match rhs.value {
                    RuntimeValue::BoolValue(b2) => b1 == b2,
                    _ => panic!("should never happen"),
                },
                RuntimeValue::RealValue(r1) => match rhs.value {
                    RuntimeValue::RealValue(r2) => r1 == r2,
                    _ => panic!("should never happen"),
                },
                RuntimeValue::IntegerValue(i1) => match rhs.value {
                    RuntimeValue::IntegerValue(i2) => i1 == i2,
                    _ => panic!("should never happen"),
                },
                RuntimeValue::StringValue(s1) => match rhs.value {
                    RuntimeValue::StringValue(s2) => s1 == s2,
                    _ => panic!("should never happen"),
                },
                RuntimeValue::NoneValue => match rhs.value {
                    RuntimeValue::NoneValue => true,
                    _ => panic!("should never happen"),
                },
            },
            EqualityOperator::NotEqualsTo => match lhs.value {
                RuntimeValue::BoolValue(b1) => match rhs.value {
                    RuntimeValue::BoolValue(b2) => b1 != b2,
                    _ => panic!("should never happen"),
                },
                RuntimeValue::RealValue(r1) => match rhs.value {
                    RuntimeValue::RealValue(r2) => r1 != r2,
                    _ => panic!("should never happen"),
                },
                RuntimeValue::IntegerValue(i1) => match rhs.value {
                    RuntimeValue::IntegerValue(i2) => i1 != i2,
                    _ => panic!("should never happen"),
                },
                RuntimeValue::StringValue(s1) => match rhs.value {
                    RuntimeValue::StringValue(s2) => s1 != s2,
                    _ => panic!("should never happen"),
                },
                RuntimeValue::NoneValue => match rhs.value {
                    RuntimeValue::NoneValue => false,
                    _ => panic!("should never happen"),
                },
            },
        };

        result.value = RuntimeValue::BoolValue(b_result);
        return Ok(result);
    }
}

#[derive(Clone, PartialEq)]
pub enum RuntimeValue {
    BoolValue(bool),
    IntegerValue(i64),
    RealValue(f64),
    StringValue(String),
    NoneValue,
}

#[derive(Clone, Debug)]
pub struct RuntimeError {
    pub message: String,
}

impl Error for RuntimeError {}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "faled")
    }
}

pub struct RuntimeScope {
    pub scopes: VecDeque<HashMap<String, Object>>,
}

impl fmt::Display for RuntimeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BoolValue(b) => write!(f, "{}", b),
            Self::NoneValue => write!(f, "none"),
            Self::IntegerValue(i) => write!(f, "{}", i),
            Self::RealValue(r) => write!(f, "{}", r),
            Self::StringValue(s) => write!(f, "{}", s),
        }
    }
}

impl RuntimeScope {
    pub fn new() -> Self {
        return Self {
            scopes: VecDeque::new(),
        };
    }

    pub fn find(&mut self, key: String) -> Option<Object> {
        for scope in self.scopes.iter_mut() {
            match scope.get(&key) {
                Some(value) => return Some(value.clone()),
                None => continue,
            }
        }

        return None;
    }

    fn set(&mut self, key: &String, value: Object) -> () {
        match self.scopes.get_mut(0) {
            Some(scope) => {
                scope.insert(String::from(key), value);
                ()
            }
            None => panic!("fatal: no valid scope found"),
        };
    }

    fn add_scope(&mut self) -> () {
        self.scopes.push_front(HashMap::new());
    }

    fn pop_scope(&mut self) -> () {
        self.scopes.pop_front();
    }
}

pub trait Runtime {
    fn exec(&mut self, runtime_scope: &mut RuntimeScope) -> Result<Object, RuntimeError>;
}

impl Runtime for Scope {
    fn exec(&mut self, runtime_scope: &mut RuntimeScope) -> Result<Object, RuntimeError> {
        eprintln!("scope");
        for expr in self.body.iter_mut() {
            match expr {
                Expression::ReturnExpression(_) => {
                    return expr.exec(runtime_scope);
                }
                _ => {
                    expr.exec(runtime_scope)?;
                }
            }
        }

        return Ok(Object::none());
    }
}

impl Runtime for Expression {
    fn exec(&mut self, runtime_scope: &mut RuntimeScope) -> Result<Object, RuntimeError> {
        match self {
            Self::Assignment(id, rhs) => {
                eprintln!("assignment");
                let name = id.exec(runtime_scope)?;
                let value = rhs.exec(runtime_scope)?;

                match name.typename {
                    Types::NoneType => {
                        return Err(RuntimeError {
                            message: format!("use of undeclared variable"),
                        });
                    }
                    _ => {
                        if name.typename != value.typename {
                            return Err(RuntimeError {
                                message: format!("type error"),
                            });
                        }
                        runtime_scope.set(&name.name, value);

                        match runtime_scope.find(name.name) {
                            Some(v) => Ok(v),
                            None => Err(RuntimeError {
                                message: format!("unexpected fatal error"),
                            }),
                        }
                    }
                }
            }
            Self::ArithmeticExpression(op, lhs, rhs) => {
                let lhs_value = lhs.exec(runtime_scope)?;
                let rhs_value = rhs.exec(runtime_scope)?;
                return Object::arithmetic_operation(op.to_owned(), lhs_value, rhs_value);
            }
            Self::BooleanExpression(op, lhs, rhs) => {
                let lhs_value = lhs.exec(runtime_scope)?;
                let rhs_value = rhs.exec(runtime_scope)?;
                return Object::boolean_operation(op.to_owned(), lhs_value, rhs_value);
            }
            Self::EqualityExpression(op, lhs, rhs) => {
                let lhs_value = lhs.exec(runtime_scope)?;
                let rhs_value = rhs.exec(runtime_scope)?;
                return Object::equality_operation(op.to_owned(), lhs_value, rhs_value);
            }
            Self::ComparisonExpression(op, lhs, rhs) => Ok(Object::none()),
            Self::IdentifierExpression(value) => {
                let found_var = runtime_scope.find(value.to_owned());

                match found_var {
                    Some(var) => Ok(var),
                    None => Ok(Object::none()),
                }
            }
            Self::IntegerConstant(value) => Ok(Object::int(value.to_owned())),
            Self::RealConstant(value) => Ok(Object::real(value.to_owned())),
            Self::BooleanConstant(value) => Ok(Object::bool(value.to_owned())),
            Self::StringConstant(value) => Ok(Object::string(value.to_owned())),
            Self::Declaration(access_modifier, typename, id, rhs) => {
                let mut value: Object;

                if let Some(v) = rhs {
                    value = v.exec(runtime_scope)?;
                } else {
                    value = Object::none();
                }

                value.access_modifier = access_modifier.to_owned();
                value.typename = typename.to_owned();

                runtime_scope.set(id, value.clone());
                return Ok(value);
            }
            Self::Dto(name, fields) => Ok(Object::none()),
            Self::FunctionDefinition(access_modifier, typename, name, parameters, body) => {
                Ok(Object::none())
            }
            Self::FunctionCall(id, arguments) => Ok(Object::none()),
            Self::BuiltInCall(id, arguments) => Ok(Object::none()),
            Self::Module(name, body) => {
                eprintln!("module {}", name);
                runtime_scope.add_scope();
                let result = body.exec(runtime_scope);
                runtime_scope.pop_scope();
                return result;
            }
            Self::TraitDefinition(name, functions) => Ok(Object::none()),
            Self::ClassDefinition(name, instance_variables, methods) => Ok(Object::none()),
            Self::WhileLoop(guard, body) => Ok(Object::none()),
            Self::ForLoop(iterator_var_name, iterator_exp, body) => Ok(Object::none()),
            Self::If(guard, body, next) => Ok(Object::none()),
            Self::ReturnExpression(expression) => expression.exec(runtime_scope),
            Self::ExitExpression(expression) => {
                let exit_value = expression.exec(runtime_scope)?;
                match exit_value.value {
                    RuntimeValue::IntegerValue(i) => std::process::exit(i as i32),
                    _ => panic!("invalid exit value, must be an integer"),
                }
            }
            Self::ChainedExpression(lhs, rhs) => Ok(Object::none()),
            Self::Break => Ok(Object::none()),
            Self::Continue => Ok(Object::none()),
        }
    }
}
