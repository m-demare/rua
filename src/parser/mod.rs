pub mod ast;
mod utils;

use std::{iter::Peekable, slice::Iter};

use crate::{lex::tokens::{Token, TokenType, UnaryOp, BinaryOp}, identifiers::Identifier};
use ast::Program;
use self::{ast::{Statement, Expression, Precedence, precedence_of_binary, BlockType, FunctionArg, ExpressionContext}, utils::{peek_token_is, ParseError}};

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
            TT::LPAREN => { let prefix = parse_prefix_exp(tokens_it)?; parse_call_st(tokens_it, prefix) },
            TT::ILLEGAL(s) => return Err(ParseError::IllegalToken(s.clone().into_boxed_str())),
            TT::IF => parse_if(tokens_it),
            TT::WHILE => parse_while(tokens_it),
            _ => return Err(ParseError::UnexpectedToken(Box::new((*token).clone()), Box::new([TT::LOCAL, TT::RETURN, TT::SEMICOLON, TT::EOF, TT::END, TT::ELSE, TT::ELSEIF]))),
        }?;
        statements.push(statement);
    }
    unreachable!("Should have found EOF before end");
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
    let mut cloned_it = tokens_it.clone();
    if let Some(Token { ttype: TokenType::IDENTIFIER(id), ..}) = cloned_it.next() {
        match cloned_it.peek() {
            Some(Token { ttype: TokenType::ASSIGN, ..}) => {
                tokens_it.next();
                tokens_it.next();
                parse_assign_st(tokens_it, id.clone()) },
            Some(Token { ttype: TokenType::LPAREN, ..}) => { let prefix = parse_prefix_exp(tokens_it)?; parse_call_st(tokens_it, prefix) },
            Some(t) => Err(ParseError::UnexpectedToken(Box::new((*t).clone()), Box::new([TokenType::ASSIGN]))),
            None => unreachable!("Should have found EOF before end"),
        }
    } else {
        unreachable!("Can't call handle_identifier_st without an identifier");
    }
}

fn parse_prefix_exp(tokens_it: &mut Peekable<Iter<Token>>) -> ParseResult<Expression> {
    match tokens_it.next() {
        Some(Token{ ttype: TokenType::IDENTIFIER(id), .. }) => parse_var(tokens_it, id.clone()),
        Some(Token{ ttype: TokenType::LPAREN, .. }) =>  parse_group_expr(tokens_it),
        Some(t) => Err(ParseError::UnexpectedToken(Box::new((*t).clone()), [TokenType::IDENTIFIER(Identifier::default()), TokenType::LPAREN].into())),
        None => unreachable!("Should have found EOF before end"),
    }
}

fn parse_var(tokens_it: &mut Peekable<Iter< Token>>, id: Identifier) -> ParseResult<Expression> {
    // TODO var ::=  Name | prefixexp `[´ exp `]´ | prefixexp `.´ Name 
    Ok(Expression::Identifier(id))
}

fn parse_assign_st(tokens_it: &mut Peekable<Iter<Token>>, id: Identifier) -> ParseResult<Statement> {
    let rhs = parse_expression(tokens_it, &Precedence::Lowest, &ExpressionContext::Assign)?;
    Ok(Statement::Assign(id, Box::new(rhs)))
}

fn parse_call_st(tokens_it: &mut Peekable<Iter<Token>>, lhs: Expression) -> ParseResult<Statement> {
    let lparen = tokens_it.next();
    assert_eq!(lparen.expect("Ain't None").ttype, TokenType::LPAREN);

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
    if let Some(Token {ttype: TokenType::LPAREN, ..}) = tokens_it.peek() {
        parse_call_st(tokens_it, Expression::Call(Box::new(lhs), args))
    } else {
        Ok(Statement::Call(Box::new(lhs), args))
    }
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

fn parse_expression(tokens_it: &mut Peekable<Iter<Token>>, precedence: &Precedence, context: &ExpressionContext) -> ParseResult<Expression> {
    let token = tokens_it.next();

    // Prefix tokens
    let mut expr = match token {
        Some(Token { ttype: TokenType::UNARY_OP(ref op), .. }) => parse_unary_op(op, tokens_it, context)?,
        Some(Token { ttype: TokenType::MINUS, .. }) => Expression::Neg(Box::new(parse_expression(tokens_it, &Precedence::Prefix, context)?)),
        Some(Token { ttype: TokenType::IDENTIFIER(ref s), .. }) => parse_var(tokens_it, s.clone())?,
        Some(Token { ttype: TokenType::NUMBER(n), .. }) => Expression::NumberLiteral(*n),
        Some(Token { ttype: TokenType::TRUE, .. }) => Expression::BooleanLiteral(true),
        Some(Token { ttype: TokenType::FALSE, .. }) => Expression::BooleanLiteral(false),
        Some(Token { ttype: TokenType::LPAREN, .. }) => parse_group_expr(tokens_it)?,
        Some(Token { ttype: TokenType::FUNCTION, .. }) => match parse_function_expr(tokens_it)? {
            (exp, None) => exp,
            (_, Some(id)) => return Err(ParseError::NamedFunctionExpr(id)),
        },
        Some(t) => return Err(ParseError::UnexpectedTokenWithErrorMsg(Box::new(t.clone()), "an expression".into())),
        None => unreachable!("Should have found EOF before end"),
    };

    // Infix tokens
    loop {
        match tokens_it.peek() {
            Some(Token { ttype: TokenType::BINARY_OP(op), .. }) => 
                if precedence < &precedence_of_binary(op) {
                    expr = parse_binary_op(expr, op, tokens_it, context)?;
                } else {
                    break Ok(expr)
                },
            Some(Token { ttype: TokenType::MINUS, .. }) =>
                if precedence < &Precedence::Sum {
                    tokens_it.next();
                    let rhs = parse_expression(tokens_it, &Precedence::Sum, context)?;
                    expr = Expression::Minus(Box::new(expr), Box::new(rhs));
                } else {
                    break Ok(expr)
                },
            Some(Token { ttype: TokenType::LPAREN, .. }) =>
                if precedence < &Precedence::Call {
                    expr = parse_call_expr(expr, tokens_it)?;
                } else {
                    break Ok(expr)
                },
                // TODO DOT, LBRACK
            _ => break Ok(expr),
        }
    }

    // TODO terminators (depending on ExpressionContext)
    // if peek_token_is(tokens_it, &TokenType::SEMICOLON) {
    //     tokens_it.next();
    // }
    // Ok(expr)
}

fn parse_call_expr(expr: Expression, tokens_it: &mut Peekable<Iter<Token>>) -> ParseResult<Expression> {
    match parse_call_st(tokens_it, expr)? {
        Statement::Call(lhs, args) => Ok(Expression::Call(lhs, args)),
        _ => unreachable!("parse_call_st returned a non-call statement")
    }
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
        BinaryOp::PLUS => Expression::Plus(Box::new(lhs), Box::new(rhs)),
        BinaryOp::TIMES => Expression::Times(Box::new(lhs), Box::new(rhs)),
        BinaryOp::DIV => Expression::Div(Box::new(lhs), Box::new(rhs)),
        BinaryOp::MOD => Expression::Mod(Box::new(lhs), Box::new(rhs)),
        BinaryOp::EXP => Expression::Exp(Box::new(lhs), Box::new(rhs)),
        BinaryOp::EQ => Expression::Eq(Box::new(lhs), Box::new(rhs)),
        BinaryOp::NEQ => Expression::Neq(Box::new(lhs), Box::new(rhs)),
        BinaryOp::LE => Expression::Le(Box::new(lhs), Box::new(rhs)),
        BinaryOp::GE => Expression::Ge(Box::new(lhs), Box::new(rhs)),
        BinaryOp::LT => Expression::Lt(Box::new(lhs), Box::new(rhs)),
        BinaryOp::GT => Expression::Gt(Box::new(lhs), Box::new(rhs)),
        BinaryOp::AND => Expression::And(Box::new(lhs), Box::new(rhs)),
        BinaryOp::OR => Expression::Or(Box::new(lhs), Box::new(rhs)),
        BinaryOp::DOTDOT => Expression::Dotdot(Box::new(lhs), Box::new(rhs)),
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

fn parse_while(tokens_it: &mut Peekable<Iter<Token>>) -> ParseResult<Statement> {
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

