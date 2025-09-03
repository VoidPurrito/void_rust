use std::{
    collections::{HashMap, VecDeque}, fmt,
};

use crate::expressions::expression::{Expression, Types};

pub trait StaticAnalysis {
    fn static_analysis(
        &mut self,
        static_analysis_scope: &mut StaticAnalysisScope,
    ) -> Types;
}

pub struct StaticAnalysisScope {
    pub scopes: VecDeque<HashMap<String, Types>>,
}


impl fmt::Display for StaticAnalysisScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let length = self.scopes.len();
        write!(f, "{} scopes", length);
        Ok(())
    }
}

impl StaticAnalysisScope {
    pub fn new() -> Self {
        return Self {
            scopes: VecDeque::new(),
        };
    }

    pub fn find(&mut self, key: String) -> Option<Types> {
        for scope in self.scopes.iter_mut() {
            match scope.get(&key) {
                Some(value) => return Some(value.clone()),
                None => continue,
            }
        }

        return None;
    }

    fn set(&mut self, key: &String, value: Types) -> () {
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

impl StaticAnalysis for Expression {
    fn static_analysis(&mut self, static_analysis_scope: &mut StaticAnalysisScope) -> Types {
        match self {
            Self::Assignment(id, rhs) => {
                let id_type = id.static_analysis(static_analysis_scope);
                let rhs_type = rhs.static_analysis(static_analysis_scope);

                if id_type != rhs_type {
                    eprintln!("type mismatch: incompatible types {}/{}", id_type, rhs_type);
                }

                Types::NoneType
            },
            Self::ArithmeticExpression(op, lhs, rhs) => Types::NoneType,
            Self::BooleanExpression(op, lhs, rhs) => {
                let lhs_type = lhs.static_analysis(static_analysis_scope);
                let rhs_type = rhs.static_analysis(static_analysis_scope);

                if lhs_type != Types::BoolType|| rhs_type != Types::BoolType {
                    eprintln!("type mismatch: boolean expression expects bool/bool, got, {}/{}", lhs_type, rhs_type);
                }

                Types::BoolType
            },
            Self::EqualityExpression(op, lhs, rhs) => Types::NoneType,
            Self::ComparisonExpression(op, lhs, rhs) => Types::NoneType,
            Self::IdentifierExpression(value) => {
                let id_type = static_analysis_scope.find(value.to_owned());

                match id_type {
                    Some(t) => t,
                    None => {
                        eprintln!("variable '{}' is not defined", value);
                        Types::NoneType
                    },
                }
            },
            Self::IntegerConstant(value) => Types::IntType,
            Self::RealConstant(value) => Types::RealType,
            Self::BooleanConstant(value) => Types::BoolType,
            Self::StringConstant(value) => Types::StringType,
            Self::Declaration(access_modifier, typename, id, rhs) => {
                static_analysis_scope.set(id, typename.to_owned());
                Types::NoneType
            },
            Self::Dto(name, fields) => Types::NoneType,
            Self::FunctionDefinition(access_modifier, typename, name, parameters, body) => Types::NoneType,
            Self::FunctionCall(id, arguments) => Types::NoneType,
            Self::BuiltInCall(id, arguments) => Types::NoneType,
            Self::Module(name, body) => {
                static_analysis_scope.add_scope();

                for expr in body.body.iter_mut() {
                    expr.static_analysis(static_analysis_scope);
                }

                Types::NoneType
            },
            Self::TraitDefinition(name, functions) => Types::NoneType,
            Self::ClassDefinition(name, instance_variables, methods) => Types::NoneType,
            Self::WhileLoop(guard, body) => Types::NoneType,
            Self::ForLoop(iterator_var_name, iterator_exp, body) => Types::NoneType,
            Self::If(guard, body, next) => Types::NoneType,
            Self::ReturnExpression(expression) => Types::NoneType,
            Self::ExitExpression(expression) => Types::NoneType,
            Self::ChainedExpression(lhs, rhs) => Types::NoneType,
            Self::Break => Types::NoneType,
            Self::Continue => Types::NoneType,
        }
    }
}
