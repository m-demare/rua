pub mod ast;
mod tests;
mod utils;

use std::iter::Peekable;

use self::{
    ast::{
        precedence_of_binary, BlockType, Expression, FunctionArg, ParseError, Precedence, Statement,
    },
    utils::peek_token_is,
};
use crate::{
    lex::tokens::{BinaryOp, Token, TokenType, UnaryOp},
    parser::utils::debug_peek_token,
};
use ast::Program;
use rua_identifiers::Identifier;

// https://www.lua.org/manual/5.1/manual.html#8

pub type ParseResult<T> = Result<T, ParseError>;

pub fn parse<T: Iterator<Item = Token>>(tokens: T) -> ParseResult<Program> {
    let mut tokens_it = tokens.peekable();

    Ok(Program { statements: parse_block(tokens_it.by_ref(), BlockType::TopLevel)? })
}

fn parse_block<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
    block_type: BlockType,
) -> ParseResult<Vec<Statement>> {
    use TokenType as TT;

    let mut statements = Vec::with_capacity(if block_type == BlockType::TopLevel { 20 } else { 2 });
    while let Some(token) = tokens_it.peek() {
        let statement = match &token.ttype {
            TT::LOCAL => parse_local_st(tokens_it),
            TT::RETURN => parse_return_st(tokens_it),
            TT::BREAK => parse_break_st(tokens_it),
            TT::SEMICOLON => {
                tokens_it.next();
                continue;
            }
            TT::FUNCTION => parse_function_st(tokens_it),
            TT::END | TT::ELSE | TT::ELSEIF => {
                return match block_type {
                    BlockType::If if matches!(token.ttype, TT::ELSE | TT::ELSEIF | TT::END) => {
                        Ok(statements)
                    }
                    BlockType::Else if matches!(token.ttype, TT::END) => Ok(statements),
                    BlockType::Function if matches!(token.ttype, TT::END) => Ok(statements),
                    BlockType::While if matches!(token.ttype, TT::END) => Ok(statements),
                    _ => Err(ParseError::UnexpectedClose(
                        Box::new(block_type),
                        Box::new(token.clone()),
                    )),
                }
            }
            TT::IDENTIFIER(_) => handle_identifier_st(tokens_it),
            TT::LPAREN => handle_lparen_st(tokens_it),
            TT::ILLEGAL(s) => return Err(ParseError::IllegalToken(s.clone())),
            TT::IF => parse_if(tokens_it),
            TT::WHILE => parse_while_st(tokens_it),
            TT::NUMBER(_) | TT::BINARY_OP(_) | TT::LBRACE | TT::NIL | TT::TRUE | TT::FALSE => {
                Err(ParseError::UnexpectedExpression)
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    Box::new(token.clone()),
                    Box::new([TT::LOCAL, TT::RETURN, TT::SEMICOLON, TT::END, TT::ELSE, TT::ELSEIF]),
                ))
            }
        }?;
        statements.push(statement);
    }
    match block_type {
        BlockType::TopLevel => Ok(statements),
        _ => Err(ParseError::UnexpectedEOF),
    }
}

fn handle_lparen_st<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
) -> Result<Statement, ParseError> {
    match parse_expression(tokens_it, &Precedence::Lowest)? {
        Expression::Call(lhs, args) => Ok(Statement::Call(lhs, args)),
        _ => Err(ParseError::UnexpectedExpression),
    }
}

fn parse_return_st<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
) -> ParseResult<Statement> {
    debug_peek_token!(tokens_it, TokenType::RETURN);

    tokens_it.next();
    if peek_token_is!(tokens_it, TokenType::END | TokenType::SEMICOLON)
        || tokens_it.peek().is_none()
    {
        return Ok(Statement::Return(None));
    }
    let expr = parse_expression(tokens_it, &Precedence::Lowest)?;
    if !peek_token_is!(tokens_it, TokenType::END | TokenType::ELSE | TokenType::ELSEIF) {
        if let Some(t) = tokens_it.peek() {
            return Err(ParseError::UnexpectedToken(
                Box::new(t.clone()),
                Box::new([TokenType::END, TokenType::ELSE, TokenType::ELSEIF]),
            ));
        }
    }
    Ok(Statement::Return(Some(Box::new(expr))))
}

fn parse_break_st<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
) -> ParseResult<Statement> {
    debug_peek_token!(tokens_it, TokenType::BREAK);

    tokens_it.next();
    if !peek_token_is!(tokens_it, TokenType::END | TokenType::ELSE | TokenType::ELSEIF) {
        if let Some(t) = tokens_it.peek() {
            return Err(ParseError::UnexpectedToken(
                Box::new(t.clone()),
                Box::new([TokenType::END, TokenType::ELSE, TokenType::ELSEIF]),
            ));
        }
    }
    Ok(Statement::Break)
}

fn handle_identifier_st<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
) -> ParseResult<Statement> {
    if let Some(Token { ttype: TokenType::IDENTIFIER(id), .. }) = tokens_it.next() {
        match tokens_it.peek() {
            Some(Token { ttype: TokenType::ASSIGN | TokenType::COMMA, .. }) => {
                parse_assign_st(tokens_it, id)
            }
            Some(Token { ttype: TokenType::LPAREN | TokenType::DOT, .. }) => {
                match parse_infix_exp(tokens_it, Expression::Identifier(id), &Precedence::Lowest)? {
                    Expression::Call(lhs, args) => Ok(Statement::Call(lhs, args)),
                    _ => Err(ParseError::UnexpectedExpression),
                }
            }
            Some(t) => {
                Err(ParseError::UnexpectedToken(Box::new(t.clone()), Box::new([TokenType::ASSIGN])))
            }
            None => Err(ParseError::UnexpectedEOF),
        }
    } else {
        unreachable!("Can't call handle_identifier_st without an identifier");
    }
}

fn parse_assign_st<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
    id: Identifier,
) -> ParseResult<Statement> {
    let mut identifiers = vec![id];
    let mut rhs = Vec::new();
    loop {
        match tokens_it.next() {
            Some(Token { ttype: TokenType::COMMA, .. }) => {}
            Some(Token { ttype: TokenType::ASSIGN, .. }) => break,
            Some(t) => {
                return Err(ParseError::UnexpectedToken(
                    t.into(),
                    [TokenType::COMMA, TokenType::ASSIGN].into(),
                ))
            }
            None => return Err(ParseError::UnexpectedEOF),
        }
        match tokens_it.next() {
            Some(Token { ttype: TokenType::IDENTIFIER(id), .. }) => identifiers.push(id),
            Some(t) => {
                return Err(ParseError::UnexpectedToken(
                    t.into(),
                    [TokenType::IDENTIFIER(Identifier::default())].into(),
                ))
            }
            None => return Err(ParseError::UnexpectedEOF),
        }
    }
    loop {
        match tokens_it.peek() {
            Some(..) => rhs.push(parse_expression(tokens_it, &Precedence::Lowest)?),
            None => return Err(ParseError::UnexpectedEOF),
        }
        match tokens_it.peek() {
            Some(Token { ttype: TokenType::COMMA, .. }) => {
                tokens_it.next();
            }
            Some(..) | None => break,
        }
    }
    Ok(Statement::Assign(identifiers, rhs))
}

fn parse_function_st<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
) -> ParseResult<Statement> {
    debug_peek_token!(tokens_it, TokenType::FUNCTION);

    tokens_it.next();
    let (func, name) = parse_function_expr(tokens_it)?;
    match name {
        Some(name) => Ok(Statement::Assign(vec![name], vec![func])),
        None => Err(ParseError::UnnamedFunctionSt),
    }
}

fn parse_local_st<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
) -> ParseResult<Statement> {
    debug_peek_token!(tokens_it, TokenType::LOCAL);

    tokens_it.next();
    if Some(&Token { ttype: TokenType::FUNCTION }) == tokens_it.peek() {
        if let Statement::Assign(name, func) = parse_function_st(tokens_it)? {
            Ok(Statement::Local(name, func))
        } else {
            unreachable!("parse_function_st can only return successfully an AssignStatement")
        }
    } else {
        let mut identifiers = Vec::new();
        let mut rhs = Vec::new();
        let mut has_assign = false;
        loop {
            match tokens_it.peek() {
                Some(Token { ttype: TokenType::IDENTIFIER(id), .. }) => {
                    identifiers.push(*id);
                    tokens_it.next();
                }
                Some(t) => {
                    return Err(ParseError::UnexpectedToken(
                        Box::new(t.clone()),
                        [TokenType::IDENTIFIER(Identifier::default())].into(),
                    ))
                }
                None => return Err(ParseError::UnexpectedEOF),
            }
            match tokens_it.peek() {
                Some(Token { ttype: TokenType::COMMA, .. }) => {
                    tokens_it.next();
                }
                Some(Token { ttype: TokenType::ASSIGN, .. }) => {
                    has_assign = true;
                    tokens_it.next();
                    break;
                }
                Some(..) | None => break,
            }
        }
        if has_assign {
            loop {
                match tokens_it.peek() {
                    Some(..) => rhs.push(parse_expression(tokens_it, &Precedence::Lowest)?),
                    None => return Err(ParseError::UnexpectedEOF),
                }
                match tokens_it.peek() {
                    Some(Token { ttype: TokenType::COMMA, .. }) => {
                        tokens_it.next();
                    }
                    Some(..) | None => break,
                }
            }
        }
        Ok(Statement::Local(identifiers, rhs))
    }
}

pub fn parse_expression<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
    precedence: &Precedence,
) -> ParseResult<Expression> {
    let expr = parse_prefix_exp(tokens_it)?;

    parse_infix_exp(tokens_it, expr, precedence)
}

fn parse_infix_exp<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
    mut lhs: Expression,
    precedence: &Precedence,
) -> Result<Expression, ParseError> {
    loop {
        match tokens_it.peek().cloned() {
            Some(Token { ttype: TokenType::BINARY_OP(ref op), .. }) => {
                if precedence < &precedence_of_binary(op) {
                    lhs = parse_binary_op(lhs, op, tokens_it)?;
                } else {
                    break Ok(lhs);
                }
            }
            Some(Token { ttype: TokenType::MINUS, .. }) => {
                if precedence < &Precedence::Sum {
                    tokens_it.next();
                    let rhs = parse_expression(tokens_it, &Precedence::Sum)?;
                    lhs = Expression::Minus(Box::new((lhs, rhs)));
                } else {
                    break Ok(lhs);
                }
            }
            Some(Token { ttype: TokenType::LPAREN, .. }) => {
                if precedence < &Precedence::Call {
                    lhs = parse_call_expr(lhs, tokens_it)?;
                } else {
                    break Ok(lhs);
                }
            }
            Some(Token { ttype: TokenType::DOT, .. }) => {
                if precedence < &Precedence::FieldAccess {
                    tokens_it.next();
                    match tokens_it.next() {
                        Some(Token { ttype: TokenType::IDENTIFIER(id), .. }) => {
                            lhs = Expression::FieldAccess(Box::new(lhs), id);
                        }
                        Some(t) => {
                            return Err(ParseError::UnexpectedToken(
                                Box::new(t),
                                [TokenType::IDENTIFIER(Identifier::default())].into(),
                            ))
                        }
                        None => return Err(ParseError::UnexpectedEOF),
                    }
                } else {
                    break Ok(lhs);
                }
            }
            // TODO LBRACK
            _ => break Ok(lhs),
        }
    }
}

fn parse_prefix_exp<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
) -> Result<Expression, ParseError> {
    Ok(match tokens_it.next() {
        Some(Token { ttype: TokenType::UNARY_OP(ref op), .. }) => parse_unary_op(op, tokens_it)?,
        Some(Token { ttype: TokenType::MINUS, .. }) => {
            Expression::Neg(Box::new(parse_expression(tokens_it, &Precedence::Prefix)?))
        }
        Some(Token { ttype: TokenType::IDENTIFIER(id), .. }) => Expression::Identifier(id),
        Some(Token { ttype: TokenType::NUMBER(n), .. }) => Expression::NumberLiteral(n),
        Some(Token { ttype: TokenType::STRING(s), .. }) => Expression::StringLiteral(s),
        Some(Token { ttype: TokenType::TRUE, .. }) => Expression::BooleanLiteral(true),
        Some(Token { ttype: TokenType::FALSE, .. }) => Expression::BooleanLiteral(false),
        Some(Token { ttype: TokenType::LPAREN, .. }) => parse_group_expr(tokens_it)?,
        Some(Token { ttype: TokenType::NIL, .. }) => Expression::Nil,
        Some(Token { ttype: TokenType::FUNCTION, .. }) => match parse_function_expr(tokens_it)? {
            (exp, None) => exp,
            (_, Some(id)) => return Err(ParseError::NamedFunctionExpr(id)),
        },
        Some(t) => {
            return Err(ParseError::UnexpectedTokenWithErrorMsg(
                Box::new(t),
                "an expression".into(),
            ))
        }
        None => return Err(ParseError::UnexpectedEOF),
    })
}

fn parse_call_expr<T: Iterator<Item = Token>>(
    lhs: Expression,
    tokens_it: &mut Peekable<T>,
) -> ParseResult<Expression> {
    debug_peek_token!(tokens_it, TokenType::LPAREN);

    tokens_it.next();
    let mut args = Vec::new();
    loop {
        match tokens_it.peek() {
            Some(Token { ttype: TokenType::RPAREN, .. }) => {
                tokens_it.next();
                break;
            }
            Some(_) => {
                args.push(parse_expression(tokens_it, &Precedence::Lowest)?);
                if peek_token_is!(tokens_it, TokenType::COMMA) {
                    tokens_it.next();
                }
            }
            None => return Err(ParseError::UnexpectedEOF),
        }
    }
    Ok(Expression::Call(Box::new(lhs), args))
}

fn parse_group_expr<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
) -> ParseResult<Expression> {
    let expr = parse_expression(tokens_it, &Precedence::Lowest)?;
    return match tokens_it.peek() {
        Some(Token { ttype: TokenType::RPAREN }) => {
            tokens_it.next();
            Ok(expr)
        }
        Some(t) => {
            Err(ParseError::UnexpectedToken(Box::new(t.clone()), Box::new([TokenType::RPAREN])))
        }
        None => Err(ParseError::UnexpectedEOF),
    };
}

fn parse_function_expr<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
) -> ParseResult<(Expression, Option<Identifier>)> {
    let name = match tokens_it.peek().cloned() {
        Some(Token { ttype: TokenType::IDENTIFIER(id) }) => {
            tokens_it.next();
            Some(id)
        }
        _ => None,
    };
    if !peek_token_is!(tokens_it, TokenType::LPAREN) {
        return match tokens_it.peek() {
            Some(t) => {
                Err(ParseError::UnexpectedToken(Box::new(t.clone()), Box::new([TokenType::LPAREN])))
            }
            None => Err(ParseError::UnexpectedEOF),
        };
    }
    tokens_it.next();
    let mut args = Vec::new();
    loop {
        match tokens_it.peek().cloned() {
            Some(Token { ttype: TokenType::IDENTIFIER(id) }) => {
                tokens_it.next();
                args.push(FunctionArg::Identifier(id));
                if peek_token_is!(tokens_it, TokenType::COMMA) {
                    tokens_it.next();
                }
            }
            Some(Token { ttype: TokenType::DOTDOTDOT }) => {
                tokens_it.next();
                args.push(FunctionArg::Dotdotdot);
                if !peek_token_is!(tokens_it, TokenType::RPAREN) {
                    return match tokens_it.peek() {
                        Some(t) => Err(ParseError::UnexpectedToken(
                            Box::new(t.clone()),
                            Box::new([TokenType::RPAREN]),
                        )),
                        None => Err(ParseError::UnexpectedEOF),
                    };
                }
            }
            Some(Token { ttype: TokenType::RPAREN }) => {
                tokens_it.next();
                break;
            }
            Some(t) => {
                return Err(ParseError::UnexpectedToken(
                    Box::new(t),
                    Box::new([TokenType::RPAREN, TokenType::IDENTIFIER(Identifier::default())]),
                ))
            }
            None => return Err(ParseError::UnexpectedEOF),
        }
    }
    let body = parse_block(tokens_it, BlockType::Function)?;
    return match tokens_it.peek() {
        Some(Token { ttype: TokenType::END }) => {
            tokens_it.next();
            Ok((Expression::Function(args.into(), body.into()), name))
        }
        Some(t) => {
            Err(ParseError::UnexpectedToken(Box::new(t.clone()), Box::new([TokenType::RPAREN])))
        }
        None => return Err(ParseError::UnexpectedEOF),
    };
}

fn parse_unary_op<T: Iterator<Item = Token>>(
    op: &UnaryOp,
    tokens_it: &mut Peekable<T>,
) -> ParseResult<Expression> {
    let right = parse_expression(tokens_it, &Precedence::Prefix)?;
    Ok(match op {
        UnaryOp::NOT => Expression::Not(Box::new(right)),
        UnaryOp::LEN => Expression::Len(Box::new(right)),
    })
}

fn parse_binary_op<T: Iterator<Item = Token>>(
    lhs: Expression,
    op: &BinaryOp,
    tokens_it: &mut Peekable<T>,
) -> ParseResult<Expression> {
    tokens_it.next();
    let precedence = precedence_of_binary(op);
    let rhs = parse_expression(tokens_it, &precedence)?;
    Ok(match op {
        BinaryOp::PLUS => Expression::Plus(Box::new((lhs, rhs))),
        BinaryOp::TIMES => Expression::Times(Box::new((lhs, rhs))),
        BinaryOp::DIV => Expression::Div(Box::new((lhs, rhs))),
        BinaryOp::MOD => Expression::Mod(Box::new((lhs, rhs))),
        BinaryOp::EXP => Expression::Exp(Box::new((lhs, rhs))),
        BinaryOp::EQ => Expression::Eq(Box::new((lhs, rhs))),
        BinaryOp::NEQ => Expression::Neq(Box::new((lhs, rhs))),
        BinaryOp::LE => Expression::Le(Box::new((lhs, rhs))),
        BinaryOp::GE => Expression::Ge(Box::new((lhs, rhs))),
        BinaryOp::LT => Expression::Lt(Box::new((lhs, rhs))),
        BinaryOp::GT => Expression::Gt(Box::new((lhs, rhs))),
        BinaryOp::AND => Expression::And(Box::new((lhs, rhs))),
        BinaryOp::OR => Expression::Or(Box::new((lhs, rhs))),
        BinaryOp::DOTDOT => Expression::Dotdot(Box::new((lhs, rhs))),
    })
}

fn parse_if<T: Iterator<Item = Token>>(tokens_it: &mut Peekable<T>) -> ParseResult<Statement> {
    debug_peek_token!(tokens_it, TokenType::IF);

    tokens_it.next();
    let cond = parse_expression(tokens_it, &Precedence::Lowest)?;
    if !peek_token_is!(tokens_it, TokenType::THEN) {
        return match tokens_it.peek() {
            Some(t) => {
                Err(ParseError::UnexpectedToken(Box::new(t.clone()), Box::new([TokenType::THEN])))
            }
            None => Err(ParseError::UnexpectedEOF),
        };
    }
    tokens_it.next();
    let block = parse_block(tokens_it, BlockType::If)?;

    match tokens_it.peek() {
        Some(Token { ttype: TokenType::ELSE }) => {
            tokens_it.next();
            let else_block = parse_block(tokens_it, BlockType::Else)?;
            if !peek_token_is!(tokens_it, TokenType::END) {
                return match tokens_it.peek() {
                    Some(t) => Err(ParseError::UnexpectedToken(
                        Box::new(t.clone()),
                        Box::new([TokenType::END]),
                    )),
                    None => Err(ParseError::UnexpectedEOF),
                };
            }
            tokens_it.next();
            Ok(Statement::IfThenElse(Box::new(cond), block, else_block))
        }
        Some(Token { ttype: TokenType::ELSEIF }) => parse_if_elseif(tokens_it, cond, block),
        Some(Token { ttype: TokenType::END }) => {
            tokens_it.next();
            Ok(Statement::IfThen(Box::new(cond), block))
        }
        Some(t) => {
            Err(ParseError::UnexpectedToken(Box::new(t.clone()), Box::new([TokenType::END])))
        }
        None => Err(ParseError::UnexpectedEOF),
    }
}

fn parse_if_elseif<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
    if_cond: Expression,
    if_block: Vec<Statement>,
) -> ParseResult<Statement> {
    let mut conds = vec![(if_cond, if_block)];
    loop {
        match tokens_it.next() {
            Some(Token { ttype: TokenType::ELSEIF }) => {
                let new_cond = parse_expression(tokens_it, &Precedence::Lowest)?;
                if !peek_token_is!(tokens_it, TokenType::THEN) {
                    return match tokens_it.peek() {
                        Some(t) => Err(ParseError::UnexpectedToken(
                            Box::new(t.clone()),
                            Box::new([TokenType::THEN]),
                        )),
                        None => Err(ParseError::UnexpectedEOF),
                    };
                }
                tokens_it.next();
                let new_block = parse_block(tokens_it, BlockType::If)?;
                conds.push((new_cond, new_block));
            }
            Some(Token { ttype: TokenType::ELSE }) => {
                let new_cond = Expression::BooleanLiteral(true);
                let new_block = parse_block(tokens_it, BlockType::If)?;
                conds.push((new_cond, new_block));
            }
            Some(Token { ttype: TokenType::END }) => {
                break Ok(Statement::IfThenElseIf(conds.into_boxed_slice()))
            }
            Some(t) => {
                break Err(ParseError::UnexpectedToken(Box::new(t), Box::new([TokenType::END])))
            }
            None => return Err(ParseError::UnexpectedEOF),
        }
    }
}

fn parse_while_st<T: Iterator<Item = Token>>(
    tokens_it: &mut Peekable<T>,
) -> ParseResult<Statement> {
    debug_peek_token!(tokens_it, TokenType::WHILE);

    tokens_it.next();
    let cond = parse_expression(tokens_it, &Precedence::Lowest)?;
    if !peek_token_is!(tokens_it, TokenType::DO) {
        return match tokens_it.peek() {
            Some(t) => {
                Err(ParseError::UnexpectedToken(Box::new(t.clone()), Box::new([TokenType::DO])))
            }
            None => Err(ParseError::UnexpectedEOF),
        };
    }
    tokens_it.next();
    let block = parse_block(tokens_it, BlockType::While)?;
    tokens_it.next();
    Ok(Statement::While(Box::new(cond), block))
}
