use std::{
    collections::VecDeque,
    fs::File,
    io::{self, BufReader},
};

use crate::expressions::expression::{
    AccessModifiers, ArithmeticOperator, BooleanOperator, ComparisonOperator, DtoFieldDefinition,
    EqualityOperator, Expression, FunctionArgument, FunctionParameter, Scope, ScopeType, Types,
};

use super::{
    error::ParseError,
    lexer::{Lexer, Tokens},
};

pub struct Parser {
    path: String,
    lexer: Lexer,
    scope_list: VecDeque<ScopeType>,
}

impl Parser {
    pub fn new(path: String) -> io::Result<Self> {
        let buffer = Parser::open_file(&path)?;
        let lexer = Lexer::new(buffer);
        let mut scope_list = VecDeque::new();
        scope_list.push_front(ScopeType::Global);

        Ok(Self {
            path,
            lexer,
            scope_list,
        })
    }

    fn parse_error(&self, err: String) -> ParseError {
        ParseError {
            message: format!(
                "error at position {} - {}",
                self.lexer.get_position_string(),
                err
            ),
        }
    }

    fn open_file(path: &String) -> io::Result<BufReader<File>> {
        let file = File::open(path)?;
        Ok(BufReader::new(file))
    }

    pub fn parse_file(&mut self) -> Result<Expression, ParseError> {
        let scope = self.parse_scope(ScopeType::Global)?;
        return Ok(Expression::Module(self.path.clone(), scope));
    }

    pub fn parse_scope(&mut self, scope_level: ScopeType) -> Result<Scope, ParseError> {
        let mut scope = Scope { body: Vec::new() };
        let mut token = self.lexer.next_token()?;

        if scope_level != ScopeType::Global {
            if token != Tokens::TLbrace {
                return Err(self.parse_error(format!("unexpected token {}, expected {{", token)));
            }

            self.scope_list.push_front(scope_level.clone());
        } else {
            self.lexer.put_back_token(token);
        }

        loop {
            token = self.lexer.next_token()?;
            if token == Tokens::TRbrace {
                self.scope_list.pop_front();
                break;
            }

            if token == Tokens::TEof {
                match scope_level {
                    ScopeType::Global => break,
                    _ => return Err(self.parse_error(format!("unexpected end-of-file"))),
                }
            }

            if scope_level == ScopeType::Global {
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
            }

            let has_function_scope = self
                .scope_list
                .iter()
                .position(|&s| s == ScopeType::Function)
                != None;

            if token == Tokens::TReturn {
                if has_function_scope {
                    let return_expr = self.parse_return()?;
                    scope.body.push(return_expr);
                    continue;
                }

                return Err(
                    self.parse_error(format!("return is not allowed outside of a function"))
                );
            }

            let has_loop_scope = self.scope_list.iter().position(|&s| s == ScopeType::Loop) != None;

            if token == Tokens::TBreak {
                if has_loop_scope {
                    scope.body.push(self.parse_break()?);
                    continue;
                }

                return Err(self.parse_error(format!(
                    "break is only allowed within a while or for loop body"
                )));
            }

            if token == Tokens::TContinue {
                if has_loop_scope {
                    scope.body.push(self.parse_continue()?);
                    continue;
                }

                return Err(self.parse_error(format!(
                    "break is only allowed within a while or for loop body"
                )));
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

            if token == Tokens::TFor {
                let for_expr = self.parse_for()?;
                scope.body.push(for_expr);
                continue;
            }

            if token == Tokens::TIf {
                let if_expr = self.parse_if()?;
                scope.body.push(if_expr);
                continue;
            }

            if Lexer::is_typename(token.clone()) {
                let expr = match token.clone() {
                    Tokens::TIdentifier(_) => {
                        self.lexer.put_back_token(token);

                        match self.lexer.peek_token()? {
                            Tokens::TLparen => {
                                let fn_call = self.parse_function_call()?;
                                scope.body.push(fn_call);
                                Some(())
                            }
                            _ => None,
                        }
                    }
                    _ => {
                        self.lexer.put_back_token(token);
                        let decl = self.parse_declaration()?;
                        scope.body.push(decl);
                        continue;
                    }
                };

                if let Some(()) = expr {
                    continue;
                }
            } else {
                self.lexer.put_back_token(token);
            }

            scope.body.push(self.parse_expression(true)?);
        }
        return Ok(scope);
    }

    fn parse_break(&mut self) -> Result<Expression, ParseError> {
        let token = self.lexer.next_token()?;

        if token == Tokens::TSemi {
            return Ok(Expression::Break);
        }

        return Err(ParseError {
            message: format!("unexpected token {}, expected ';'", token),
        });
    }

    fn parse_continue(&mut self) -> Result<Expression, ParseError> {
        let token = self.lexer.next_token()?;

        if token == Tokens::TSemi {
            return Ok(Expression::Continue);
        }

        return Err(ParseError {
            message: format!("unexpected token {}, expected ';'", token),
        });
    }
    fn parse_dto(&mut self) -> Result<Expression, ParseError> {
        let mut token = self.lexer.next_token()?;

        let name = match token {
            Tokens::TIdentifier(id) => id,
            token => {
                return Err(self.parse_error(format!("unexpected token: {}", token.to_string())));
            }
        };

        token = self.lexer.next_token()?;

        if token != Tokens::TLbrace {
            return Err(self.parse_error(format!("expected '{{', found {token}")));
        }

        let mut fields = Vec::new();
        self.parse_dto_definition(&mut fields)?;

        return Ok(Expression::Dto(name, fields));
    }

    fn parse_dto_field_definition(&mut self) -> Result<DtoFieldDefinition, ParseError> {
        let mut token = self.lexer.next_token()?;
        let field_type = self.parse_typename(token)?;

        token = self.lexer.next_token()?;
        let field_name = self.parse_identifier(token)?;

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

        return Err(self.parse_error(format!("unexpected token {}, expected , or }}", token)));
    }

    fn parse_function_definition(
        &mut self,
        for_trait: bool,
    ) -> Result<Box<Expression>, ParseError> {
        let maybe_identifier = self.lexer.next_token()?;
        let name = self.parse_identifier(maybe_identifier)?;
        let mut parameters: Vec<FunctionParameter> = Vec::new();

        let mut token = self.lexer.next_token()?;

        match token {
            Tokens::TLparen => {
                self.parse_function_parameters(&mut parameters)?;

                token = self.lexer.next_token()?;

                if token != Tokens::TColon {
                    return Err(self.parse_error(format!("unexpected token {}, expected :", token)));
                }

                let maybe_typename = self.lexer.next_token()?;
                let typename = self.parse_typename(maybe_typename)?;

                let body = match for_trait {
                    true => {
                        token = self.lexer.next_token()?;

                        match token {
                            Tokens::TSemi => None,
                            Tokens::TLbrace => {
                                self.lexer.put_back_token(token);
                                Some(self.parse_scope(ScopeType::Block)?)
                            }
                            _ => {
                                return Err(self.parse_error(format!(
                                    "unexpected token {}, expected ; or {{",
                                    token
                                )));
                            }
                        }
                    }
                    false => Some(self.parse_scope(ScopeType::Function)?),
                };

                return Ok(Box::new(Expression::FunctionDefinition(
                    AccessModifiers::Public,
                    typename,
                    name,
                    parameters,
                    body,
                )));
            }
            _ => Err(self.parse_error(format!("unexpected token {}, expected (", token))),
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

        return Err(self.parse_error(format!("unexpected token {}, expected , or )", token)));
    }

    fn parse_function_parameter(&mut self) -> Result<FunctionParameter, ParseError> {
        let maybe_typename = self.lexer.next_token()?;
        let param_type = self.parse_typename(maybe_typename)?;

        let maybe_name = self.lexer.next_token()?;
        let name = self.parse_identifier(maybe_name)?;

        return Ok(FunctionParameter {
            typename: param_type,
            name,
            // default_value: None,
        });
    }

    fn parse_trait_definition(&mut self) -> Result<Expression, ParseError> {
        let mut token = self.lexer.next_token()?;
        let name = self.parse_identifier(token)?;

        token = self.lexer.next_token()?;

        if token != Tokens::TLbrace {
            return Err(self.parse_error(format!("unexpected token {}, expected {{", token)));
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

            return Err(self.parse_error(format!("unexpected token {}, expected fn or }}", token)));
        }

        return Ok(Expression::TraitDefinition(name, functions));
    }

    fn parse_for(&mut self) -> Result<Expression, ParseError> {
        let mut token = self.lexer.next_token()?;
        let has_parens = token == Tokens::TLparen;

        if !has_parens {
            self.lexer.put_back_token(token);
        }

        let maybe_iterator_var_name = self.parse_expression(false)?;
        let iterator_var_name = match maybe_iterator_var_name {
            Expression::IdentifierExpression(id) => Expression::IdentifierExpression(id),
            _ => return Err(self.parse_error(format!("expected ientifier"))),
        };

        token = self.lexer.next_token()?;

        match token {
            Tokens::TIn => (),
            _ => return Err(self.parse_error(format!("expected token {}, expected 'in'", token))),
        }

        let expr = self.parse_expression(false)?;

        token = self.lexer.next_token()?;

        if has_parens && token != Tokens::TRparen {
            return Err(self.parse_error(format!(
                "unmatched '(' in for loop expression, expected ')' before opening '{{'"
            )));
        }

        if !has_parens && token == Tokens::TRparen {
            return Err(self.parse_error(format!("found closing ')' in for loop expression with no opening '(', expected opening '{{' instead")));
        }

        if !has_parens {
            self.lexer.put_back_token(token);
        }

        let body = self.parse_scope(ScopeType::Loop)?;

        return Ok(Expression::ForLoop(
            Box::new(iterator_var_name),
            Box::new(expr),
            body,
        ));
    }

    fn parse_while(&mut self) -> Result<Expression, ParseError> {
        let guard = self.parse_expression(false)?;
        let body = self.parse_scope(ScopeType::Loop)?;
        return Ok(Expression::WhileLoop(Box::new(guard), body));
    }

    fn parse_if(&mut self) -> Result<Expression, ParseError> {
        let guard = Some(Box::new(self.parse_expression(false)?));
        let body = self.parse_scope(ScopeType::Block)?;

        let mut token = self.lexer.next_token()?;

        if token == Tokens::TElse {
            token = self.lexer.next_token()?;

            if token == Tokens::TIf {
                let else_if_expr = Box::new(self.parse_if()?);
                return Ok(Expression::If(guard, body, Some(else_if_expr)));
            }

            self.lexer.put_back_token(token);
            let else_expr = Box::new(Expression::If(
                None,
                self.parse_scope(ScopeType::Block)?,
                None,
            ));

            return Ok(Expression::If(guard, body, Some(else_expr)));
        }

        self.lexer.put_back_token(token);
        return Ok(Expression::If(guard, body, None));
    }

    fn parse_declaration(&mut self) -> Result<Expression, ParseError> {
        let maybe_typename = self.lexer.next_token()?;
        let typename = self.parse_typename(maybe_typename)?;

        let maybe_id = self.lexer.next_token()?;
        let id = self.parse_identifier(maybe_id)?;

        let token = self.lexer.next_token()?;

        let rhs = match token {
            Tokens::TSemi => None,
            Tokens::TAssign => Some(Box::new(self.parse_expression(true)?)),
            _ => {
                return Err(self.parse_error(format!(
                    "unexpected token {}, expected ; or expression",
                    token
                )));
            }
        };

        return Ok(Expression::Declaration(typename, id, rhs));
    }

    fn parse_return(&mut self) -> Result<Expression, ParseError> {
        Ok(Expression::ReturnExpression(Box::new(
            self.parse_expression(true)?,
        )))
    }

    fn parse_expression(&mut self, requires_delimiter: bool) -> Result<Expression, ParseError> {
        let expr = self.parse_assignment_expression()?;
        let token = self.lexer.next_token()?;

        if (requires_delimiter && token != Tokens::TSemi)
            || (!requires_delimiter && token == Tokens::TSemi)
        {
            return Err(self.parse_error(format!("unexpected token {}, expected ';'", token)));
        }

        if !requires_delimiter {
            self.lexer.put_back_token(token);
        }

        return Ok(expr);
    }

    fn parse_assignment_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.parse_boolean_expression(BooleanOperator::LogicalOr)?;
        let token = self.lexer.next_token()?;

        match token {
            Tokens::TAssign => {
                let rhs = self.parse_assignment_expression()?;
                return Ok(Expression::Assignment(Box::new(lhs), Box::new(rhs)));
            }
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
                    return Err(self
                        .parse_error(format!("unexpected token {}, expected expression", token)));
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
                    return Err(self
                        .parse_error(format!("unexpected token {}, expected expression", token)));
                }
            };

            lhs = Expression::ArithmeticExpression(op, Box::new(lhs), Box::new(rhs));
            token = self.lexer.next_token()?;
        }

        self.lexer.put_back_token(token);
        return Ok(lhs);
    }

    fn parse_multiplicative_arithmetic_expression(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_function_call()?;
        let mut token = self.lexer.next_token()?;

        while token == Tokens::TMult || token == Tokens::TDiv || token == Tokens::TMod {
            let rhs = self.parse_function_call()?;

            let op = match token {
                Tokens::TMult => ArithmeticOperator::Multiply,
                Tokens::TDiv => ArithmeticOperator::Divide,
                Tokens::TMod => ArithmeticOperator::Mod,
                _ => {
                    return Err(self
                        .parse_error(format!("unexpected token {}, expected expression", token)));
                }
            };

            lhs = Expression::ArithmeticExpression(op, Box::new(lhs), Box::new(rhs));
            token = self.lexer.next_token()?;
        }

        self.lexer.put_back_token(token);
        return Ok(lhs);
    }

    fn parse_function_call(&mut self) -> Result<Expression, ParseError> {
        let expr = self.parse_primary_expression()?;

        match expr {
            Expression::IdentifierExpression(id) => {
                let token = self.lexer.next_token()?;
                if token == Tokens::TLparen {
                    let mut arguments: Vec<FunctionArgument> = Vec::new();
                    self.parse_function_arguments(&mut arguments)?;
                    return Ok(Expression::FunctionCall(id, arguments));
                } else {
                    self.lexer.put_back_token(token);
                    return Ok(Expression::IdentifierExpression(id));
                }
            }
            e => Ok(e),
        }
    }

    fn parse_function_arguments(
        &mut self,
        arguments: &mut Vec<FunctionArgument>,
    ) -> Result<(), ParseError> {
        let mut token = self.lexer.next_token()?;

        while token != Tokens::TRparen {
            self.lexer.put_back_token(token);

            let arg = FunctionArgument {
                expr: self.parse_expression(false)?,
            };
            arguments.push(arg);

            token = self.lexer.next_token()?;

            if token == Tokens::TComma {
                token = self.lexer.next_token()?;
                continue;
                //return Err(ParseError { message: format!("unexpected token '{}', expected ',' in function argument list", token) });
            }

            if token == Tokens::TRparen {
                continue;
            }

            return Err(self.parse_error(format!(
                "unexpected token '{}', expected ',' or ')' in function argument list",
                token
            )));
        }

        Ok(())
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, ParseError> {
        let mut token = self.lexer.next_token()?;

        match token {
            Tokens::TLparen => {
                let expr = self.parse_expression(false)?;
                token = self.lexer.next_token()?;

                if token != Tokens::TRparen {
                    return Err(self.parse_error(format!("unexpected token {}, expected )", token)));
                }

                return Ok(expr);
            },
            Tokens::TIdentifier(value) => Ok(Expression::IdentifierExpression(value)),
            Tokens::TIntegerValue(value) => Ok(Expression::IntegerConstant(value)),
            Tokens::TTrue => Ok(Expression::BooleanConstant(true)),
            Tokens::TFalse => Ok(Expression::BooleanConstant(false)),
            Tokens::TRealValue(value) => Ok(Expression::RealConstant(value)),
            Tokens::TStringValue(value) => Ok(Expression::StringConstant(value)),
            _ => Err(self.parse_error(format!("unexpected token {}, expected identifier, integer value, bool value, real value, or string value", token))),
        }
    }

    fn parse_typename(&self, token: Tokens) -> Result<Types, ParseError> {
        let is_typename = Lexer::is_typename(token.clone());
        let typename = match is_typename {
            true => match token {
                Tokens::TBool => Ok(Types::BoolType),
                Tokens::TVar => Ok(Types::DynamicType),
                Tokens::TInt => Ok(Types::IntType),
                Tokens::TMap => Ok(Types::MapType),
                Tokens::TReal => Ok(Types::RealType),
                Tokens::TStr => Ok(Types::StringType),
                Tokens::TIdentifier(id) => Ok(Types::CustomType(id.clone())),
                _ => Err(self.parse_error(format!(
                    "unexpected token {}, expected type name",
                    token.to_string()
                ))),
            },
            false => Err(self.parse_error(format!(
                "unexpected token {}, expected type name",
                token.to_string()
            ))),
        };

        return typename;
    }

    fn parse_identifier(&self, token: Tokens) -> Result<String, ParseError> {
        match token {
            Tokens::TIdentifier(id) => Ok(id),
            _ => Err(self.parse_error(format!(
                "unexpected token {}, expected identifier",
                token.to_string()
            ))),
        }
    }
}
