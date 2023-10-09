pub mod ast;
mod utils;
mod tests;

use std::{iter::Peekable, slice::Iter};

use crate::{lex::tokens::{Token, TokenType, UnaryOp, BinaryOp}, identifiers::Identifier};
use ast::Program;
use self::{ast::{Statement, Expression, Precedence, precedence_of_binary, BlockType, FunctionArg, ExpressionContext, ParseError}, utils::peek_token_is};

// https://www.lua.org/manual/5.1/manual.html#8

pub type ParseResult<T> = Result<T, ParseError>;

pub fn parse(tokens: &[Token]) -> ParseResult<Program> {
    let mut tokens_it = tokens.iter().peekable();

    Ok(Program {
        statements: parse_block(tokens_it.by_ref(), BlockType::TopLevel)?,
    })
}

fn parse_block(tokens_it: &mut Peekable<Iter<Token>>, block_type: BlockType) -> ParseResult<Vec<Statement>> {
    use TokenType as TT;

    let mut statements = Vec::with_capacity(if block_type == BlockType::TopLevel {20} else {2});
    while let Some(token) = tokens_it.peek() {
        let statement = match &token.ttype {
            TT::LOCAL => parse_local_st(tokens_it),
            TT::RETURN => parse_return_st(tokens_it),
            TT::SEMICOLON => {tokens_it.next(); continue;},
            TT::FUNCTION => parse_function_st(tokens_it),
            TT::EOF | TT::END | TT::ELSE | TT::ELSEIF => {
                return match block_type {
                    BlockType::TopLevel if matches!(token.ttype, TT::EOF) => Ok(statements),
                    BlockType::If if matches!(token.ttype, TT::ELSE | TT::ELSEIF | TT::END) => Ok(statements),
                    BlockType::Else if matches!(token.ttype, TT::EOF | TT::END) => Ok(statements),
                    BlockType::Function if matches!(token.ttype, TT::END) => Ok(statements),
                    BlockType::While if matches!(token.ttype, TT::END) => Ok(statements),
                    _ => Err(ParseError::UnexpectedClose(Box::new(block_type), Box::new((*token).clone()))),
                }
            },
            TT::IDENTIFIER(_) => { handle_identifier_st(tokens_it) },
            TT::LPAREN => { handle_lparen_st(tokens_it) },
            TT::ILLEGAL(s) => return Err(ParseError::IllegalToken(s.clone())),
            TT::IF => parse_if(tokens_it),
            TT::WHILE => parse_while_st(tokens_it),
            TT::NUMBER(_) | TT::BINARY_OP(_) | TT::LBRACE |
                TT::NIL | TT::TRUE | TT::FALSE => Err(ParseError::UnexpectedExpression),
            _ => return Err(ParseError::UnexpectedToken(Box::new((*token).clone()), Box::new([TT::LOCAL, TT::RETURN, TT::SEMICOLON, TT::EOF, TT::END, TT::ELSE, TT::ELSEIF]))),
        }?;
        statements.push(statement);
    }
    unreachable!("Should have found EOF before end");
}

fn handle_lparen_st(tokens_it: &mut Peekable<Iter<Token>>) -> Result<Statement, ParseError> {
    match parse_expression(tokens_it, &Precedence::Lowest, &ExpressionContext::Group)? {
        Expression::Call(lhs, args) => Ok(Statement::Call(lhs, args)),
        _ => Err(ParseError::UnexpectedExpression),
    }
}

fn parse_return_st(tokens_it: &mut Peekable<Iter<Token>>) -> ParseResult<Statement> {
    let keyword = tokens_it.next();
    debug_assert!(keyword.is_some() && keyword.expect("Ain't None").ttype == TokenType::RETURN);
    if peek_token_is!(tokens_it, TokenType::EOF | TokenType::END | TokenType::SEMICOLON) {
        return Ok(Statement::Return(None))
    }
    let expr = parse_expression(tokens_it, &Precedence::Lowest, &ExpressionContext::Return)?;
    if !peek_token_is!(tokens_it, TokenType::EOF | TokenType::END | TokenType::ELSE | TokenType::ELSEIF){
        return Err(ParseError::UnexpectedToken(Box::new((*tokens_it.peek().expect("Ain't None")).clone()),
            Box::new([TokenType::EOF, TokenType::END, TokenType::ELSE, TokenType::ELSEIF])));
    }
    Ok(Statement::Return(Some(Box::new(expr))))
}

fn handle_identifier_st(tokens_it: &mut Peekable<Iter<Token>>) -> ParseResult<Statement> {
    if let Some(Token { ttype: TokenType::IDENTIFIER(id), ..}) = tokens_it.next() {
        match tokens_it.peek() {
            Some(Token { ttype: TokenType::ASSIGN, ..}) => {
                tokens_it.next();
                parse_assign_st(tokens_it, id.clone()) },
            Some(Token { ttype: TokenType::LPAREN | TokenType::DOT, ..}) => {
                match parse_infix_exp(tokens_it, Expression::Identifier(id.clone()), &Precedence::Lowest, &ExpressionContext::Group)? {
                    Expression::Call(lhs, args) => Ok(Statement::Call(lhs, args)),
                    t => {println!("{t:?}"); Err(ParseError::UnexpectedExpression) },
                }
            },
            Some(t) => Err(ParseError::UnexpectedToken(Box::new((*t).clone()), Box::new([TokenType::ASSIGN]))),
            None => unreachable!("Should have found EOF before end"),
        }
    } else {
        unreachable!("Can't call handle_identifier_st without an identifier");
    }
}

fn parse_assign_st(tokens_it: &mut Peekable<Iter<Token>>, id: Identifier) -> ParseResult<Statement> {
    let rhs = parse_expression(tokens_it, &Precedence::Lowest, &ExpressionContext::Assign)?;
    Ok(Statement::Assign(id, Box::new(rhs)))
}

fn parse_function_st(tokens_it: &mut Peekable<Iter<Token>>) -> ParseResult<Statement> {
    let keyword = tokens_it.next();
    debug_assert!(keyword.is_some() && keyword.expect("Ain't None").ttype == TokenType::FUNCTION);
    let (func, name) = parse_function_expr(tokens_it)?;
    match name {
        Some(name) => Ok(Statement::Assign(name, Box::new(func))),
        None => Err(ParseError::UnnamedFunctionSt),
    }
}

fn parse_local_st(tokens_it: &mut Peekable<Iter<Token>>) -> ParseResult<Statement> {
    let keyword = tokens_it.next();
    debug_assert!(keyword.is_some() && keyword.expect("Ain't None").ttype == TokenType::LOCAL);
    if Some(&&Token { ttype: TokenType::FUNCTION }) == tokens_it.peek() {
        if let Statement::Assign(name, func) = parse_function_st(tokens_it)? {
            Ok(Statement::Local(name, Some(func)))
        } else {
            unreachable!("parse_function_st can only return successfully an AssignStatement")
        }
    } else {
        let id = parse_identifier(tokens_it)?;
        if !peek_token_is!(tokens_it, TokenType::ASSIGN) {
            return Ok(Statement::Local(id, None));
        }
        tokens_it.next();
        let expr = parse_expression(tokens_it, &Precedence::Lowest, &ExpressionContext::Assign)?;
        Ok(Statement::Local(id, Some(Box::new(expr))))
    }
}

fn parse_identifier(tokens_it: &mut Peekable<Iter<Token>>) -> ParseResult<Identifier> {
    Ok(match tokens_it.next() {
        Some(Token { ttype: TokenType::IDENTIFIER(s) }) => s.clone(),
        Some(t) => return Err(ParseError::UnexpectedToken(Box::new(t.clone()), [TokenType::IDENTIFIER(Identifier::default())].into())),
        None => unreachable!("Should have found EOF before end"),
    })
}

pub fn parse_expression(tokens_it: &mut Peekable<Iter<Token>>, precedence: &Precedence, context: &ExpressionContext) -> ParseResult<Expression> {
    let expr = parse_prefix_exp(tokens_it, context)?;

    parse_infix_exp(tokens_it, expr, precedence, context)

    // TODO terminators (depending on ExpressionContext)
    // if peek_token_is(tokens_it, &TokenType::SEMICOLON) {
    //     tokens_it.next();
    // }
    // Ok(expr)
}

fn parse_infix_exp(tokens_it: &mut Peekable<Iter<Token>>, mut lhs: Expression,precedence: &Precedence, context: &ExpressionContext) -> Result<Expression, ParseError> {
    loop {
        match tokens_it.peek() {
            Some(Token { ttype: TokenType::BINARY_OP(op), .. }) => 
                if precedence < &precedence_of_binary(op) {
                    lhs = parse_binary_op(lhs, op, tokens_it, context)?;
                } else {
                    break Ok(lhs)
                },
            Some(Token { ttype: TokenType::MINUS, .. }) =>
                if precedence < &Precedence::Sum {
                    tokens_it.next();
                    let rhs = parse_expression(tokens_it, &Precedence::Sum, context)?;
                    lhs = Expression::Minus(Box::new((lhs, rhs)));
                } else {
                    break Ok(lhs)
                },
            Some(Token { ttype: TokenType::LPAREN, .. }) =>
                if precedence < &Precedence::Call {
                    lhs = parse_call_expr(lhs, tokens_it)?;
                } else {
                    break Ok(lhs)
                },
            Some(Token { ttype: TokenType::DOT, .. }) =>
                if precedence < &Precedence::FieldAccess {
                    tokens_it.next();
                    match tokens_it.next() {
                        Some(Token { ttype: TokenType::IDENTIFIER(id), .. }) => {
                            lhs = Expression::FieldAccess(Box::new(lhs), id.clone());
                        }
                        Some(t) => return Err(ParseError::UnexpectedToken(Box::new(t.clone()), [TokenType::IDENTIFIER(Identifier::default())].into())),
                        None => unreachable!("Should have found EOF before end"),
                    }
                } else {
                    break Ok(lhs)
                },
            // TODO LBRACK
            _ => break Ok(lhs),
        }
    }
}

fn parse_prefix_exp(tokens_it: &mut Peekable<Iter<Token>>, context: &ExpressionContext) -> Result<Expression, ParseError> {
    Ok(match tokens_it.next() {
        Some(Token { ttype: TokenType::UNARY_OP(ref op), .. }) => parse_unary_op(op, tokens_it, context)?,
        Some(Token { ttype: TokenType::MINUS, .. }) => Expression::Neg(Box::new(parse_expression(tokens_it, &Precedence::Prefix, context)?)),
        Some(Token { ttype: TokenType::IDENTIFIER(id), .. }) => Expression::Identifier(id.clone()),
        Some(Token { ttype: TokenType::NUMBER(n), .. }) => Expression::NumberLiteral(*n),
        Some(Token { ttype: TokenType::TRUE, .. }) => Expression::BooleanLiteral(true),
        Some(Token { ttype: TokenType::FALSE, .. }) => Expression::BooleanLiteral(false),
        Some(Token { ttype: TokenType::LPAREN, .. }) => parse_group_expr(tokens_it)?,
        Some(Token { ttype: TokenType::NIL, .. }) => Expression::Nil,
        Some(Token { ttype: TokenType::FUNCTION, .. }) => match parse_function_expr(tokens_it)? {
            (exp, None) => exp,
            (_, Some(id)) => return Err(ParseError::NamedFunctionExpr(id)),
        },
        Some(t) => return Err(ParseError::UnexpectedTokenWithErrorMsg(Box::new(t.clone()), "an expression".into())),
        None => unreachable!("Should have found EOF before end"),
    })
}

fn parse_call_expr(lhs: Expression, tokens_it: &mut Peekable<Iter<Token>>) -> ParseResult<Expression> {
    let lparen = tokens_it.next();
    debug_assert_eq!(lparen.expect("Ain't None").ttype, TokenType::LPAREN);

    let mut args = Vec::new();
    loop {
        match tokens_it.peek() {
            Some(Token{ ttype: TokenType::RPAREN, .. }) => {tokens_it.next(); break;},
            Some(_) => {
                args.push(parse_expression(tokens_it, &Precedence::Lowest, &ExpressionContext::Call)?);
                if peek_token_is!(tokens_it, TokenType::COMMA) { tokens_it.next(); }
            },
            None => unreachable!("Should have found EOF before end"),
        }
    }
    Ok(Expression::Call(Box::new(lhs), args))
}

fn parse_group_expr(tokens_it: &mut Peekable<Iter<Token>>) -> ParseResult<Expression> {
    let expr = parse_expression(tokens_it, &Precedence::Lowest, &ExpressionContext::Group)?;
    return match tokens_it.peek() {
        Some(Token { ttype: TokenType::RPAREN }) => {tokens_it.next(); Ok(expr)},
        Some(t) => Err(ParseError::UnexpectedToken(Box::new((*t).clone()), Box::new([TokenType::RPAREN]))),
        None => unreachable!("Should have found EOF before end"),
    }
}

fn parse_function_expr(tokens_it: &mut Peekable<Iter<Token>>) -> ParseResult<(Expression, Option<Identifier>)> {
    let name = match tokens_it.peek() {
        Some(Token { ttype: TokenType::IDENTIFIER(id) }) => { tokens_it.next(); Some(id.clone()) },
        _ => None,
    };
    if !peek_token_is!(tokens_it, TokenType::LPAREN){
        return Err(ParseError::UnexpectedToken(Box::new((*tokens_it.peek().expect("Ain't None")).clone()),
            Box::new([TokenType::LPAREN])));
    }
    tokens_it.next();
    let mut args = Vec::new();
    loop {
        match tokens_it.peek() {
            Some(Token{ttype: TokenType::IDENTIFIER(id)}) => {
                tokens_it.next();
                args.push(FunctionArg::Identifier(id.clone()));
                if peek_token_is!(tokens_it, TokenType::COMMA)
                    {tokens_it.next();}
            },
            Some(Token{ttype: TokenType::DOTDOTDOT}) => {
                tokens_it.next();
                args.push(FunctionArg::Dotdotdot);
                if !peek_token_is!(tokens_it, TokenType::RPAREN) {
                    return Err(ParseError::UnexpectedToken(Box::new((*tokens_it.peek().expect("Ain't None")).clone()), Box::new([TokenType::RPAREN])));
                }
            }
            Some(Token{ttype: TokenType::RPAREN}) => {tokens_it.next(); break;}
            Some(t) => return Err(ParseError::UnexpectedToken(Box::new((*t).clone()), Box::new([TokenType::RPAREN, TokenType::IDENTIFIER(Identifier::default())]))),
            None => unreachable!("Should have found EOF before end"),
        }
    }
    let body = parse_block(tokens_it, BlockType::Function)?;
    return match tokens_it.peek() {
        Some(Token { ttype: TokenType::END }) => {tokens_it.next(); Ok((Expression::Function(args.into_boxed_slice(), body), name))},
        Some(t) => Err(ParseError::UnexpectedToken(Box::new((*t).clone()), Box::new([TokenType::RPAREN]))),
        None => unreachable!("Should have found EOF before end"),
    }
}

fn parse_unary_op(op: &UnaryOp, tokens_it: &mut Peekable<Iter<Token>>, context: &ExpressionContext) -> ParseResult<Expression> {
    let right = parse_expression(tokens_it, &Precedence::Prefix, context)?;
    Ok(match op {
        UnaryOp::NOT => {Expression::Not(Box::new(right))},
        UnaryOp::LEN => {Expression::Len(Box::new(right))},
    })
}

fn parse_binary_op(lhs: Expression, op: &BinaryOp, tokens_it: &mut Peekable<Iter<Token>>, context: &ExpressionContext) -> ParseResult<Expression>{
    tokens_it.next();
    let precedence = precedence_of_binary(op);
    let rhs = parse_expression(tokens_it, &precedence, context)?;
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

fn parse_if(tokens_it: &mut Peekable<Iter<Token>>) -> ParseResult<Statement> {
    let keyword = tokens_it.next();
    debug_assert!(keyword.is_some() && keyword.expect("Ain't None").ttype == TokenType::IF);
    let cond = parse_expression(tokens_it, &Precedence::Lowest, &ExpressionContext::Condition)?;
    if !peek_token_is!(tokens_it, TokenType::THEN){
        return Err(ParseError::UnexpectedToken(Box::new((*tokens_it.peek().expect("Ain't None")).clone()),
            Box::new([TokenType::THEN])));
    }
    tokens_it.next();
    let block = parse_block(tokens_it, BlockType::If)?;

    match tokens_it.peek() {
        Some(Token { ttype: TokenType::ELSE }) => {
            tokens_it.next();
            let else_block = parse_block(tokens_it, BlockType::Else)?;
            if !peek_token_is!(tokens_it, TokenType::END){
                return Err(ParseError::UnexpectedToken(Box::new((*tokens_it.peek().expect("Ain't None")).clone()),
                    Box::new([TokenType::END])));
            }
            tokens_it.next();
            Ok(Statement::IfThenElse(Box::new(cond), block, else_block))
        },
        Some(Token { ttype: TokenType::ELSEIF }) => {
            parse_if_elseif(tokens_it, cond, block)
        },
        Some(Token { ttype: TokenType::END }) => {tokens_it.next(); Ok(Statement::IfThen(Box::new(cond), block)) },
        Some(t) => Err(ParseError::UnexpectedToken(Box::new((*t).clone()), Box::new([TokenType::END]))),
        None => unreachable!("Should have found EOF before end"),
    }
}

fn parse_if_elseif(tokens_it: &mut Peekable<Iter<Token>>, if_cond: Expression, if_block: Vec<Statement>) -> ParseResult<Statement> {
    let mut conds = vec![(if_cond, if_block)];
    loop {
        match tokens_it.next() {
            Some(Token { ttype: TokenType::ELSEIF }) => {
                let new_cond = parse_expression(tokens_it, &Precedence::Lowest, &ExpressionContext::Condition)?;
                if !peek_token_is!(tokens_it, TokenType::THEN){
                    return Err(ParseError::UnexpectedToken(Box::new((*tokens_it.peek().expect("Ain't None")).clone()),
                        Box::new([TokenType::THEN])));
                }
                tokens_it.next();
                let new_block = parse_block(tokens_it, BlockType::If)?;
                conds.push((new_cond, new_block));
            }
            Some(Token { ttype: TokenType::ELSE }) => {
                let new_cond = Expression::BooleanLiteral(true);
                let new_block = parse_block(tokens_it, BlockType::If)?;
                conds.push((new_cond, new_block));
            },
            Some(Token { ttype: TokenType::END }) => break Ok(Statement::IfThenElseIf(conds.into_boxed_slice())),
            Some(t) => break Err(ParseError::UnexpectedToken(Box::new(t.clone()), Box::new([TokenType::END]))),
            None => unreachable!("Should have found EOF before end"),
        }
    }
}

fn parse_while_st(tokens_it: &mut Peekable<Iter<Token>>) -> ParseResult<Statement> {
    let keyword = tokens_it.next();
    debug_assert!(keyword.is_some() && keyword.expect("Ain't None").ttype == TokenType::WHILE);
    let cond = parse_expression(tokens_it, &Precedence::Lowest, &ExpressionContext::Condition)?;
    if !peek_token_is!(tokens_it, TokenType::DO){
        return Err(ParseError::UnexpectedToken(Box::new((*tokens_it.peek().expect("Ain't None")).clone()),
            Box::new([TokenType::DO])));
    }
    tokens_it.next();
    let block = parse_block(tokens_it, BlockType::While)?;
    tokens_it.next();
    Ok(Statement::While(Box::new(cond), block))
}

