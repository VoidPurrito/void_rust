use std::{
    fs::File,
    io::{self, BufReader},
};

use crate::expressions::expression::{
    AccessModifiers, ArithmeticOperator, BooleanOperator, ComparisonOperator, DtoFieldDefinition,
    EqualityOperator, Expression, FunctionArgument, FunctionParameter, Scope, ScopeLevels, Types,
};

use super::{
    error::ParseError,
    lexer::{Lexer, Tokens},
};

pub struct Parser {
    path: String,
    lexer: Lexer,
}

impl Parser {
    pub fn new(path: String) -> io::Result<Self> {
        let buffer = Parser::open_file(&path)?;
        let lexer = Lexer::new(buffer);

        Ok(Self { path, lexer })
    }

    fn open_file(path: &String) -> io::Result<BufReader<File>> {
        let file = File::open(path)?;
        Ok(BufReader::new(file))
    }

    pub fn parse_file(&mut self) -> Result<Expression, ParseError> {
        let scope = self.parse_scope(ScopeLevels::Global)?;
        return Ok(Expression::Module(self.path.clone(), scope));
    }

    pub fn parse_scope(&mut self, scope_level: ScopeLevels) -> Result<Scope, ParseError> {
        let mut scope = Scope { body: Vec::new() };

        let mut token = self.lexer.next_token()?;

        if scope_level == ScopeLevels::Block {
            if token != Tokens::TLbrace {
                return Err(ParseError {
                    message: format!("unexpected token {:?}, expected {{", token),
                });
            }
        } else {
            self.lexer.put_back_token(token);
        }

        loop {
            token = self.lexer.next_token()?;

            if token == Tokens::TRbrace {
                break;
            }

            if token == Tokens::TEof {
                match scope_level {
                    ScopeLevels::Block | ScopeLevels::Function => {
                        return Err(ParseError {
                            message: format!("unexpected end-of-file"),
                        })
                    }
                    ScopeLevels::Global => {
                        break;
                    }
                }
            }

            if scope_level == ScopeLevels::Global {
                if token == Tokens::TDto {
                    let dto = self.parse_dto()?;
                    scope.body.push(dto);
                    continue;
                }

                if token == Tokens::TTrait {
                    let trait_def = self.parse_trait_definition()?;
                    scope.body.push(trait_def);
                    continue;
                }
            } else {
                if token == Tokens::TReturn {
                    let return_expr = self.parse_return()?;
                    scope.body.push(return_expr);
                    continue;
                }
            }

            if token == Tokens::TFn {
                let function_def = self.parse_function_definition(false)?;
                scope.body.push(*function_def);
                continue;
            }

            if token == Tokens::TWhile {
                let while_expr = self.parse_while()?;
                scope.body.push(while_expr);
                continue;
            }

            if Lexer::is_typename(&token) {
                self.lexer.put_back_token(token);
                let decl = self.parse_declaration()?;
                scope.body.push(decl);
                continue;
            }

            let valid_expressions = match scope_level {
                ScopeLevels::Block | ScopeLevels::Function => "return, fn, while",
                ScopeLevels::Global => "dto, trait, fn, while",
            };

            return Err(ParseError {
                message: format!(
                    "unexpected token {:?}, expected one of {}",
                    token, valid_expressions
                ),
            });
        }
        return Ok(scope);
    }

    fn parse_dto(&mut self) -> Result<Expression, ParseError> {
        let mut token = self.lexer.next_token()?;

        let name = match token {
            Tokens::TIdentifier(id) => id,
            token => {
                return Err(ParseError {
                    message: format!("unexpected token: {:?}", token.to_string()),
                })
            }
        };

        token = self.lexer.next_token()?;

        if token != Tokens::TLbrace {
            return Err(ParseError {
                message: format!("expected '{{', found {token}"),
            });
        }

        let mut fields = Vec::new();
        self.parse_dto_definition(&mut fields)?;

        return Ok(Expression::Dto(name, fields));
    }

    fn parse_dto_field_definition(&mut self) -> Result<DtoFieldDefinition, ParseError> {
        let field_type = Parser::parse_typename(&self.lexer.next_token()?)?;
        let field_name = Parser::parse_identifier(self.lexer.next_token()?)?;

        return Ok(DtoFieldDefinition {
            field_type,
            field_name,
        });
    }

    fn parse_dto_definition(
        &mut self,
        list: &mut Vec<DtoFieldDefinition>,
    ) -> Result<(), ParseError> {
        let mut token = self.lexer.next_token()?;

        if token == Tokens::TRbrace {
            return Ok(());
        }

        self.lexer.put_back_token(token);

        let decl = self.parse_dto_field_definition()?;
        list.push(decl);

        token = self.lexer.next_token()?;

        if token == Tokens::TRbrace {
            return Ok(());
        }

        if token == Tokens::TComma {
            return self.parse_dto_definition(list);
        }

        return Err(ParseError {
            message: format!("unexpected token {:?}, expected , or }}", token),
        });
    }

    fn parse_function_definition(
        &mut self,
        for_trait: bool,
    ) -> Result<Box<Expression>, ParseError> {
        let name = Parser::parse_identifier(self.lexer.next_token()?)?;
        let mut parameters: Vec<FunctionParameter> = Vec::new();

        let mut token = self.lexer.next_token()?;

        match token {
            Tokens::TLparen => {
                self.parse_function_parameters(&mut parameters)?;

                token = self.lexer.next_token()?;

                if token != Tokens::TColon {
                    return Err(ParseError {
                        message: format!("unexpected token {:?}, expected :", token),
                    });
                }

                let typename = Parser::parse_typename(&self.lexer.next_token()?)?;

                let body = match for_trait {
                    true => {
                        token = self.lexer.next_token()?;

                        match token {
                            Tokens::TSemi => None,
                            Tokens::TLbrace => {
                                self.lexer.put_back_token(token);
                                Some(self.parse_scope(ScopeLevels::Block)?)
                            }
                            _ => {
                                return Err(ParseError {
                                    message: format!(
                                        "unexpected token {:?}, expected ; or {{",
                                        token
                                    ),
                                })
                            }
                        }
                    }
                    false => Some(self.parse_scope(ScopeLevels::Block)?),
                };

                return Ok(Box::new(Expression::FunctionDefinition(
                    AccessModifiers::Public,
                    typename,
                    name,
                    parameters,
                    body,
                )));
            }
            _ => Err(ParseError {
                message: format!("unexpected token {:?}, expected (", token),
            }),
        }
    }

    fn parse_function_parameters(
        &mut self,
        parameters: &mut Vec<FunctionParameter>,
    ) -> Result<(), ParseError> {
        let mut token = self.lexer.next_token()?;

        if token == Tokens::TRparen {
            return Ok(());
        }

        self.lexer.put_back_token(token);

        let param = self.parse_function_parameter()?;
        parameters.push(param);

        token = self.lexer.next_token()?;

        if token == Tokens::TRparen {
            return Ok(());
        }

        if token == Tokens::TComma {
            return self.parse_function_parameters(parameters);
        }

        return Err(ParseError {
            message: format!("unexpected token {:?}, expected , or )", token),
        });
    }

    fn parse_function_parameter(&mut self) -> Result<FunctionParameter, ParseError> {
        let param_type = Parser::parse_typename(&self.lexer.next_token()?)?;
        let name = Parser::parse_identifier(self.lexer.next_token()?)?;
        return Ok(FunctionParameter {
            typename: param_type,
            name,
            default_value: None,
        });
    }

    fn parse_trait_definition(&mut self) -> Result<Expression, ParseError> {
        let mut token = self.lexer.next_token()?;
        let name = Parser::parse_identifier(token)?;

        token = self.lexer.next_token()?;

        if token != Tokens::TLbrace {
            return Err(ParseError {
                message: format!("unexpected token {:?}, expected {{", token),
            });
        }

        let mut functions: Vec<Box<Expression>> = Vec::new();
        token = self.lexer.next_token()?;

        while token != Tokens::TRbrace {
            let function_def = self.parse_function_definition(true)?;
            functions.push(function_def);

            token = self.lexer.next_token()?;

            if token == Tokens::TFn || token == Tokens::TRbrace {
                continue;
            }

            return Err(ParseError {
                message: format!("unexpected token {:?}, expected fn or }}", token),
            });
        }

        return Ok(Expression::TraitDefinition(name, functions));
    }

    fn parse_while(&mut self) -> Result<Expression, ParseError> {
        let guard = self.parse_expression()?;
        let body = self.parse_scope(ScopeLevels::Block)?;
        return Ok(Expression::WhileLoop(Box::new(guard), body));
    }

    fn parse_declaration(&mut self) -> Result<Expression, ParseError> {
        let typename = Parser::parse_typename(&self.lexer.next_token()?)?;
        let id = Parser::parse_identifier(self.lexer.next_token()?)?;

        let mut token = self.lexer.next_token()?;

        let rhs = match token {
            Tokens::TSemi => None,
            Tokens::TAssign => {
                let rhs = self.parse_expression()?;
                //println!("decl rhs: {}", rhs.to_json());

                match rhs {
                    Expression::NOP => {
                        return Err(ParseError {
                            message: format!("unexpected token ;, expected expression"),
                        })
                    }
                    _ => {
                        token = self.lexer.next_token()?;
                        //println!("decl token: {}", token);

                        match token {
                            Tokens::TSemi => Some(Box::new(rhs)),
                            _ => {
                                return Err(ParseError {
                                    message: format!("unexpected token {:?}, expected ;", token),
                                })
                            }
                        }
                    }
                }
            }
            _ => {
                return Err(ParseError {
                    message: format!("unexpected token {:?}, expected ; or expression", token),
                })
            }
        };

        return Ok(Expression::Declaration(typename, id, rhs));
    }

    fn parse_return(&mut self) -> Result<Expression, ParseError> {
        let return_expr = self.parse_expression()?;
        return Ok(Expression::ReturnExpression(Box::new(return_expr)));
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.lexer.peek_token()?;

        if token == Tokens::TSemi {
            return Ok(Expression::NOP);
        }

        return self.parse_assignment_expression();
    }

    fn parse_assignment_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.parse_boolean_expression(BooleanOperator::LogicalOr)?;
        let token = self.lexer.next_token()?;

        match token {
            Tokens::TAssign => {
                let rhs = self.parse_assignment_expression()?;
                return Ok(Expression::Assignment(Box::new(lhs), Box::new(rhs)));
            }
            /*Tokens::TSemi => {
                self.lexer.put_back_token(token);
                Ok(lhs)
            }*/
            _ => {
                self.lexer.put_back_token(token);
                return Ok(lhs);
            }
        }
    }

    fn parse_boolean_expression(&mut self, op: BooleanOperator) -> Result<Expression, ParseError> {
        let lhs = match op {
            BooleanOperator::LogicalOr => {
                self.parse_boolean_expression(BooleanOperator::LogicalAnd)?
            }
            BooleanOperator::LogicalAnd => self.parse_equality_expression()?,
        };

        let token = self.lexer.next_token()?;

        if token == Tokens::TSemi {
            self.lexer.put_back_token(token);
            return Ok(lhs);
        }

        let rhs: Option<Expression>;

        if (op == BooleanOperator::LogicalOr && token == Tokens::TOr)
            || (op == BooleanOperator::LogicalAnd && token == Tokens::TAnd)
        {
            rhs = Some(self.parse_boolean_expression(op.clone())?);
        } else {
            rhs = None;
        }

        match rhs {
            Some(expr) => Ok(Expression::BooleanExpression(
                op,
                Box::new(lhs),
                Box::new(expr),
            )),
            None => {
                self.lexer.put_back_token(token);
                return Ok(lhs);
            }
        }
    }

    fn parse_equality_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.parse_comparison_expression()?;
        let token = self.lexer.next_token()?;

        match token {
            Tokens::TEq => Ok(Expression::EqualityExpression(
                EqualityOperator::Equals,
                Box::new(lhs),
                Box::new(self.parse_equality_expression()?),
            )),
            Tokens::TNeq => Ok(Expression::EqualityExpression(
                EqualityOperator::NotEqualsTo,
                Box::new(lhs),
                Box::new(self.parse_equality_expression()?),
            )),
            _ => {
                self.lexer.put_back_token(token);
                return Ok(lhs);
            }
        }
    }

    fn parse_comparison_expression(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_additive_arithmetic_expression()?;
        let mut token = self.lexer.next_token()?;

        while token == Tokens::TLt
            || token == Tokens::TGt
            || token == Tokens::TLte
            || token == Tokens::TGte
        {
            let rhs = self.parse_additive_arithmetic_expression()?;

            let op = match token {
                Tokens::TLt => ComparisonOperator::LessThan,
                Tokens::TGt => ComparisonOperator::GreaterThan,
                Tokens::TLte => ComparisonOperator::LessThanOrEqual,
                Tokens::TGte => ComparisonOperator::GreaterThanOrEqual,
                _ => {
                    return Err(ParseError {
                        message: format!("unexpected token {}, expected expression", token),
                    });
                }
            };

            lhs = Expression::ComparisonExpression(op, Box::new(lhs), Box::new(rhs));
            token = self.lexer.next_token()?;
        }

        self.lexer.put_back_token(token);
        return Ok(lhs);
    }

    fn parse_additive_arithmetic_expression(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_multiplicative_arithmetic_expression()?;
        let mut token = self.lexer.next_token()?;

        while token == Tokens::TPlus || token == Tokens::TMinus {
            let rhs = self.parse_multiplicative_arithmetic_expression()?;

            let op = match token {
                Tokens::TPlus => ArithmeticOperator::Plus,
                Tokens::TMinus => ArithmeticOperator::Minus,
                _ => {
                    return Err(ParseError {
                        message: format!("unexpected token {}, expected expression", token),
                    });
                }
            };

            lhs = Expression::ArithmeticExpression(op, Box::new(lhs), Box::new(rhs));
            token = self.lexer.next_token()?;
        }

        self.lexer.put_back_token(token);
        return Ok(lhs);
    }

    fn parse_multiplicative_arithmetic_expression(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_primary_expression()?;
        let mut token = self.lexer.next_token()?;

        while token == Tokens::TMult || token == Tokens::TDiv || token == Tokens::TMod {
            let rhs = self.parse_primary_expression()?;

            let op = match token {
                Tokens::TMult => ArithmeticOperator::Multiply,
                Tokens::TDiv => ArithmeticOperator::Divide,
                Tokens::TMod => ArithmeticOperator::Mod,
                _ => {
                    return Err(ParseError {
                        message: format!("unexpected token {}, expected expression", token),
                    });
                }
            };

            lhs = Expression::ArithmeticExpression(op, Box::new(lhs), Box::new(rhs));
            token = self.lexer.next_token()?;
        }

        self.lexer.put_back_token(token);
        return Ok(lhs);
    }

    fn parse_function_call(&mut self) -> Result<Expression, ParseError> {
        let id = self.lexer.next_token()?;

        match id {
            Tokens::TIdentifier(id) => {
                if self.lexer.peek_token()? == Tokens::TLparen {
                    let arguments: Vec<FunctionArgument> = Vec::new();
                    self.parse_function_arguments(&mut arguments)?;
                    return;
                } else {
                    self.parse_primary_expression();
                }
            }
            _ => self.parse_primary_expression(),
        }
    }

    fn parse_function_arguments(
        &mut self,
        arguments: &mut Vec<FunctionArgument>,
    ) -> Result<(), ParseError> {
        // consume the '('
        self.lexer.next_token()?;
        let mut token = self.lexer.next_token()?;

        while token != Tokens::TRparen {
            let arg = FunctionArgument { expr: self.parse_expression()? };
            arguments.push(arg);

            token = self.lexer.next_token();
        }

        Ok(())
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, ParseError> {
        let mut token = self.lexer.next_token()?;

        match token {
            Tokens::TLparen => {
                let expr = self.parse_expression()?;
                token = self.lexer.next_token()?;

                if token != Tokens::TRparen {
                    return Err(ParseError { message: format!("unexpected token {:?}, expected )", token) });
                }

                return Ok(expr);
            },
            Tokens::TIdentifier(value) => Ok(Expression::IdentifierExpression(value)),
            Tokens::TIntegerValue(value) => Ok(Expression::IntegerConstant(value)),
            Tokens::TTrue => Ok(Expression::BooleanConstant(true)),
            Tokens::TFalse => Ok(Expression::BooleanConstant(false)),
            Tokens::TRealValue(value) => Ok(Expression::RealConstant(value)),
            Tokens::TStringValue(value) => Ok(Expression::StringConstant(value)),
            _ => Err(ParseError { message: format!("unexpected token {:?}, expected identifier, integer value, bool value, real value, or string value", token) }),
        }
    }

    fn parse_typename(token: &Tokens) -> Result<Types, ParseError> {
        let is_typename = Lexer::is_typename(token);
        let typename = match is_typename {
            true => match token {
                Tokens::TBool => Ok(Types::BoolType),
                Tokens::TVar => Ok(Types::DynamicType),
                Tokens::TInt => Ok(Types::IntType),
                Tokens::TMap => Ok(Types::MapType),
                Tokens::TReal => Ok(Types::RealType),
                Tokens::TStr => Ok(Types::StringType),
                Tokens::TIdentifier(id) => Ok(Types::CustomType(id.clone())),
                _ => Err(ParseError {
                    message: format!(
                        "unexpected token {:?}, expected type name",
                        token.to_string()
                    ),
                }),
            },
            false => Err(ParseError {
                message: format!(
                    "unexpected token {:?}, expected type name",
                    token.to_string()
                ),
            }),
        };

        return typename;
    }

    fn parse_identifier(token: Tokens) -> Result<String, ParseError> {
        match token {
            Tokens::TIdentifier(id) => Ok(id),
            _ => Err(ParseError {
                message: format!(
                    "unexpected token {:?}, expected identifier",
                    token.to_string()
                ),
            }),
        }
    }
}
