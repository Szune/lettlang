//
// Lettlang is a programming language
// Copyright (C) 2022  Carl Erik Patrik Iwarson
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

use crate::ast::{
    Ast, AstAssignmentLet, AstCallFunction, AstDefineFunction, AstPartialCallFunction, Expr,
};
use crate::error::ParserError;
use crate::interpreter::CallingConvention;
use crate::token::{Token, TokenSpan};
use crate::{error, FileTracker, LettlangFile, Lexer};
use std::collections::HashMap;

macro_rules! require {
    ($self:expr, inner $token:path) => {
        if let Some(it) = $self.lexer.next()? {
            match it {
                TokenSpan {
                    token: $token(a), ..
                } => a,
                _ => {
                    return Err(error::parser(format!(
                        "Unexpected token: {}, expected {:?}",
                        it.token_string(&$self.file.content),
                        stringify!($token)
                    )))
                }
            }
        } else {
            return Err(error::parser("Unexpected end of file".into()));
        }
    };
    ($self:expr, $token:pat) => {
        if let Some(it) = $self.lexer.next()? {
            match it {
                TokenSpan { token: $token, .. } => {}
                _ => {
                    panic!(
                        "Unexpected token: {}, expected {:?}",
                        it.token_string(&$self.file.content),
                        stringify!($token)
                    );
                    return Err(error::parser(format!(
                        "Unexpected token: {}, expected {:?}",
                        it.token_string(&$self.file.content),
                        stringify!($token)
                    )));
                }
            }
        } else {
            return Err(error::parser("Unexpected end of file".into()));
        }
    };
}

macro_rules! consume {
    ($self:expr) => {
        match $self.lexer.next()? {
            Some(s) => s,
            None => return error::parser("Unexpected end of file".into()).into(),
        }
    };
}

macro_rules! expect_indent {
    ($self:expr, $count:expr) => {
        if $count < 1 {
            // do nothing
        } else {
            match $self.lexer.next()? {
                Some(TokenSpan {
                    token: Token::Indentation(count),
                    ..
                }) if count == $count => {}
                Some(s) => {
                    return error::parser(format!(
                        "Missing indentation({}), found {:?}",
                        $count,
                        s.token_string(&$self.file.content),
                    ))
                    .into()
                }
                None => return error::parser("Missing indentation".into()).into(),
            }
        }
    };
}

//pub struct ParsedOperator {
//    pub text: String,
//    pub kind: OperatorKind,
//}

pub struct FunctionContext {
    pub name: String,
    /// <Parameter Name, Parameter Index>
    pub arg_translation: HashMap<String, usize>,
    /// <Function Name, Parameter Count>
    pub fn_translation: HashMap<String, usize>,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    file: &'a LettlangFile,
    tracker: &'a FileTracker,
    //operators: Vec<ParsedOperator>, // TODO: change to hashmap
    /// Stack of indentations
    indentation: Vec<u8>,
    fn_context_stack: Vec<FunctionContext>,
    anon_func: u64,
}

impl<'a> Parser<'a> {
    pub fn new(file: &'a LettlangFile, tracker: &'a FileTracker) -> Parser<'a> {
        Self {
            lexer: Lexer::new(file.content.chars()),
            file,
            tracker,
            //operators: Vec::new(),
            indentation: Vec::new(),
            fn_context_stack: vec![FunctionContext {
                name: "main".to_string(),
                arg_translation: HashMap::new(),
                fn_translation: HashMap::from_iter(
                    [
                        ("prelude_add", 2),
                        ("prelude_sub", 2),
                        ("prelude_eq", 2),
                        ("prelude_if", 3),
                        ("prelude_is_fn", 1),
                        ("prelude_print", 1),
                        ("prelude_throw", 1),
                    ]
                    .into_iter()
                    .map(|x| (x.0.to_string(), x.1)),
                ),
            }],
            anon_func: 0,
        }
    }

    //pub fn new(lexer: &'a mut Lexer<'a>) -> Parser<'a> {
    //    Self {
    //        lexer,
    //        //operators: Vec::new(),
    //        indentation: Vec::new(),
    //        fn_context_stack: vec![FunctionContext {
    //            name: "main".to_string(),
    //            arg_translation: HashMap::new(),
    //            fn_translation: HashMap::from_iter(
    //                [
    //                    ("prelude_add", 2),
    //                    ("prelude_sub", 2),
    //                    ("prelude_eq", 2),
    //                    ("prelude_if", 3),
    //                    ("prelude_is_fn", 1),
    //                    ("prelude_print", 1),
    //                    ("prelude_throw", 1),
    //                ]
    //                .into_iter()
    //                .map(|x| (x.0.to_string(), x.1)),
    //            ),
    //        }],
    //        anon_func: 0,
    //    }
    //}

    pub fn next(&mut self) -> Result<Option<Ast>, ParserError> {
        // skip empty newlines
        // not skipping incorrect indentation as that would be error-prone,
        // due to the indentation sensitive nature of the language

        // reset indentation
        self.indentation.clear();
        self.indentation.push(0);

        while let Some(TokenSpan {
            token: Token::Newline(_),
            ..
        }) = self.lexer.peek()?
        {
            self.lexer.next()?;
        }
        if self.lexer.peek()?.is_none() {
            Ok(None)
        } else {
            self.parse_root().map(Some)
        }
    }

    fn parse_root(&mut self) -> Result<Ast, ParserError> {
        let token = self
            .lexer
            .peek()?
            .expect("found nothing when trying to parse root expr");
        match token {
            TokenSpan {
                token: Token::Let, ..
            } => {
                self.lexer.next()?;
                self.parse_root_let()
            }
            // TODO: prefix op call
            //Token::Operator(op) => todo!("prefix op call"),
            //TokenSpan {
            //    token: Token::Identifier(ident),
            //    ..
            //} => {
            //    self.lexer.next()?;
            //    self.parse_global_reference_assignment_fn_call(ident)
            //}
            TokenSpan {
                token:
                    Token::Identifier(_)
                    | Token::String(_)
                    | Token::Double(_)
                    | Token::Integer(_)
                    | Token::LeftParenthesis
                    | Token::RightParenthesis
                    | Token::Operator(_)
                    | Token::Unit,
                ..
            } => self.parse_bin_ops(),
            _ => error::parser(format!(
                "{:?} is not the start of a root expr",
                token.token_string(&self.file.content)
            ))
            .into(),
            //_ => Ok(()),
        }
    }

    fn parse_exprs_in_fn(&mut self) -> Result<Ast, ParserError> {
        let token = self.lexer.peek()?;
        match token {
            Some(TokenSpan {
                token: Token::Let, ..
            }) => {
                self.lexer.next()?;
                self.parse_root_let()
            }
            // TODO: prefix op call
            Some(TokenSpan {
                token: Token::Operator(op),
                ..
            }) => {
                self.lexer.next()?;
                todo!("prefix op call")
            }
            //Token::Identifier(ident) => {
            //    self.parse_global_reference_assignment_fn_call(ident)
            //}
            //Token::Identifier(fn_call_or_ref) =>
            // TODO: unexpected indentation error
            //Token::Indentation(_) =>
            _ => self.parse_bin_ops(),
            //_ => Ok(()),
        }
    }

    fn parse_let_expr(&mut self) -> Result<Ast, ParserError> {
        require!(self, Token::Let);
        let token_span = consume!(self);
        match token_span.token {
            Token::Unit => self.parse_anonymous_function(),
            Token::LeftParenthesis => self.parse_anonymous_function_with_args(),
            _ => {
                return error::parser(format!(
                    "parse_let_expr: Invalid token after let '{:?}'",
                    token_span
                ))
                .into()
            }
        }
    }

    fn parse_root_let(&mut self) -> Result<Ast, ParserError> {
        //println!("'let'");
        let token_span = consume!(self);
        match token_span.token {
            Token::Identifier(s) => self.parse_let_ident(s),
            Token::OperatorLiteral(s) => self.parse_prefix_op_decl(s),
            Token::Unit => self.parse_anonymous_function(),
            Token::LeftParenthesis => self.parse_anonymous_function_with_args(),
            _ => {
                return error::parser(format!(
                    "parse_root_let: Invalid token after let '{:?}'",
                    token_span
                ))
                .into()
            }
        }
    }

    fn parse_anonymous_function_with_args(&mut self) -> Result<Ast, ParserError> {
        self.anon_func += 1;
        let name = format!("anon #{}", self.anon_func);

        // has to have at least 1 arg
        let arg1 = require!(self, inner Token::Identifier);
        let mut args = vec![arg1];

        while let Some(Token::Identifier(arg_name)) = self.lexer.peek()?.map(|x| x.token) {
            self.lexer.next()?;
            args.push(arg_name);
        }
        require!(self, Token::RightParenthesis);
        require!(self, Token::Equals);

        self.push_fn_context(&name, &args);
        let block = self.parse_fn_block_or_expr()?;
        self.pop_fn_context();

        let definition = Ast::new(Expr::DefineFunction(AstDefineFunction {
            name: name.clone(),
            args,
            code: block,
        }));

        Ok(definition)
    }

    fn parse_anonymous_function(&mut self) -> Result<Ast, ParserError> {
        require!(self, Token::Equals);
        self.anon_func += 1;
        let name = format!("anon #{}", self.anon_func);
        let args = vec!["()".into()];
        self.push_fn_context(&name, &args);
        let block = self.parse_fn_block_or_expr()?;
        self.pop_fn_context();

        let definition = Ast::new(Expr::DefineFunction(AstDefineFunction {
            name: name.clone(),
            args,
            code: block,
        }));

        Ok(definition)
    }

    fn parse_let_ident(&mut self, name_or_fst_arg: String) -> Result<Ast, ParserError> {
        //println!("let ident '{}'", name_or_fst_arg);
        let token_span = consume!(self);
        match token_span.token {
            Token::Equals => self.parse_assignment_let(name_or_fst_arg),
            Token::Unit => self.parse_fn_decl_without_args(name_or_fst_arg),
            Token::Identifier(fst_arg) => self.parse_fn_decl(name_or_fst_arg, fst_arg),
            Token::OperatorLiteral(operator) => {
                self.parse_infix_or_postfix_op_decl(name_or_fst_arg, operator)
            }
            _ => {
                return error::parser(format!(
                    "parse_let_ident: Invalid token after let '{:?}'",
                    token_span
                ))
                .into()
            }
        }
    }

    fn parse_assignment_let(&mut self, name: String) -> Result<Ast, ParserError> {
        //println!("let {} =", name);

        let expr = self.parse_bin_ops()?;
        //println!("assignment to {:?}", name);
        Ok(Ast::new(Expr::AssignmentLet(AstAssignmentLet {
            name,
            code: expr,
        })))
    }

    fn parse_fn_decl_without_args(&mut self, name: String) -> Result<Ast, ParserError> {
        let args = vec!["()".into()];
        require!(self, Token::Equals);

        self.push_fn_context(&name, &args);
        let block = self.parse_fn_block_or_expr()?;
        self.pop_fn_context();

        let args_len = args.len();

        let definition = Ast::new(Expr::DefineFunction(AstDefineFunction {
            name: name.clone(),
            args,
            code: block,
        }));

        self.define_fn(name.clone(), args_len);

        let assignment = Ast::new(Expr::AssignmentLet(AstAssignmentLet {
            name,
            code: definition,
        }));
        Ok(assignment)
    }

    fn parse_fn_decl(&mut self, name: String, fst_arg: String) -> Result<Ast, ParserError> {
        //println!("let {} {}", name, fst_arg);
        let mut args = vec![fst_arg];
        while let Some(TokenSpan {
            span, token: tok, ..
        }) = self.lexer.next()?
        {
            match tok {
                Token::Identifier(arg) => args.push(arg),
                Token::Equals => break,
                _ => {
                    return error::parser(format!(
                        "in fn decl {:?} ({:?}) expected arg/'=' but got {:?}",
                        name, span, tok
                    ))
                    .into()
                }
            }
        }

        self.push_fn_context(&name, &args);
        let block = self.parse_fn_block_or_expr()?;
        self.pop_fn_context();

        let args_len = args.len();

        let definition = Ast::new(Expr::DefineFunction(AstDefineFunction {
            name: name.clone(),
            args,
            code: block,
        }));

        self.define_fn(name.clone(), args_len);

        let assignment = Ast::new(Expr::AssignmentLet(AstAssignmentLet {
            name,
            code: definition,
        }));
        Ok(assignment)
    }

    fn parse_fn_block_or_expr(&mut self) -> Result<Ast, ParserError> {
        // if no new line after equals, there can _only_ be *ONE* expression in the fn
        // if new line after equals, next line must be indented to the appropriate length
        match self
            .lexer
            .peek()?
            .expect("unterminated function block")
            .token
        {
            Token::Newline(_) => {
                self.lexer.next()?;
                self.parse_block()
            }
            _ => {
                // single expr
                self.parse_bin_ops()
            }
        }
    }

    fn parse_block(&mut self) -> Result<Ast, ParserError> {
        // expect block of expressions (indented block)
        // blocks should allow empty lines - meaning if we meet an empty newline,
        // (i.e. now: NewLine(_) - consume it, if peek() returns Indentation (current level))
        // we should keep processing the current block,
        // and if we don't, the block is done

        let last = *self.indentation.last().unwrap();
        let dedent = last > 0;
        let indent_now = require!(self, inner Token::Indentation);
        if indent_now <= last {
            return error::parser(format!(
                "new block found with less indentation, last was {}, current is {}",
                last, indent_now
            ))
            .into();
        }
        self.indentation.push(indent_now);

        let mut exprs = Vec::new();
        loop {
            if let Some(TokenSpan {
                token: Token::Newline(_),
                ..
            }) = self.lexer.peek()?
            {
                return error::parser(format!(
                    "last line in function '{}' contained indentation on an otherwise empty line",
                    self.fn_context_stack.last().unwrap().name
                ))
                .into();
            }
            exprs.push(self.parse_exprs_in_fn()?);

            match self.lexer.peek()? {
                None => break,
                Some(TokenSpan {
                    token: Token::Indentation(ind),
                    ..
                }) => {
                    // less indentation means getting back to previous block
                    if ind < indent_now {
                        break;
                    } else {
                        expect_indent!(self, indent_now);
                    }
                }
                Some(TokenSpan {
                    token: Token::Newline(_),
                    ..
                }) => {
                    self.lexer.next()?;
                    match self.lexer.peek()? {
                        None => break,
                        Some(TokenSpan {
                            token: Token::Indentation(ind),
                            ..
                        }) => {
                            // less indentation means getting back to previous block
                            if ind < indent_now {
                                break;
                            } else {
                                expect_indent!(self, indent_now);
                            }
                        }
                        _ => {
                            break;
                        }
                    }
                }
                _ => break,
            }
        }
        self.indentation.pop();

        if dedent {
            expect_indent!(self, last);
        }

        Ok(Ast::new(Expr::Block(exprs)))
    }

    fn parse_infix_or_postfix_op_decl(
        &mut self,
        fst_arg: String,
        operator: String,
    ) -> Result<Ast, ParserError> {
        let token_span = consume!(self);
        match token_span.token {
            Token::Equals => self.parse_postfix_op(fst_arg, operator),
            Token::Identifier(snd_arg) => self.parse_infix_op_decl(fst_arg, operator, snd_arg),
            _ => {
                return error::parser(format!(
                    "parse_infix_or_postfix_op_decl: Invalid token after let '{:?}'",
                    token_span
                ))
                .into()
            }
        }
    }

    fn parse_postfix_op(&mut self, fst_arg: String, operator: String) -> Result<Ast, ParserError> {
        let expr = self.parse_fn_block_or_expr()?;
        todo!("don't just return ok");
        //Ok(())
    }

    #[inline]
    fn push_fn_context(&mut self, name: &String, args: &Vec<String>) {
        self.fn_context_stack.push(FunctionContext {
            name: name.clone(),
            arg_translation: HashMap::from_iter(
                args.clone()
                    .into_iter()
                    .enumerate()
                    .map(|(idx, arg)| (arg, idx)),
            ),
            fn_translation: HashMap::new(),
        });
    }

    #[inline]
    fn pop_fn_context(&mut self) {
        self.fn_context_stack.pop();
    }

    /// If `None`, the function was not found
    fn get_arg(&self, ident: &String) -> Option<usize> {
        self.fn_context_stack
            .iter()
            .rev() // get closest arg
            .find_map(|ctx| ctx.arg_translation.get(ident).copied())
    }

    /// If `None`, the function was not found
    fn get_fn_arg_count(&self, ident: &String) -> Option<usize> {
        self.fn_context_stack
            .iter()
            .rev() // get closest fn
            .find_map(|ctx| ctx.fn_translation.get(ident).copied())
    }

    fn define_fn(&mut self, ident: String, arg_count: usize) {
        self.fn_context_stack
            .last_mut()
            .unwrap()
            .fn_translation
            .insert(ident, arg_count);
    }

    fn parse_infix_op_decl(
        &mut self,
        fst_arg: String,
        operator: String,
        snd_arg: String,
    ) -> Result<Ast, ParserError> {
        require!(self, Token::Equals);

        let name = format!("op_{}", operator);
        let args = vec![fst_arg, snd_arg];
        self.push_fn_context(&name, &args);
        let expr = self.parse_fn_block_or_expr()?;
        self.pop_fn_context();

        let name = format!("op_{}", operator);

        let args_len = args.len();

        let definition = Ast::new(Expr::DefineFunction(AstDefineFunction {
            name: name.clone(),
            args,
            code: expr,
        }));

        self.define_fn(name.clone(), args_len);

        let assignment = Ast::new(Expr::AssignmentLet(AstAssignmentLet {
            name,
            code: definition,
        }));
        Ok(assignment)
    }

    fn parse_prefix_op_decl(&mut self, operator: String) -> Result<Ast, ParserError> {
        let arg = require!(self, inner Token::Identifier);
        require!(self, Token::Equals);
        let expr = self.parse_fn_block_or_expr()?;
        todo!("don't just return ok");
    }

    fn parse_bin_ops(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_expr()?;
        while let Some(TokenSpan {
            token: Token::Operator(_),
            ..
        }) = self.lexer.peek()?
        {
            expr = self.parse_second_arg_to_infix_op(expr)?;
        }
        Ok(expr)
    }

    fn parse_fn_arg_expr(&mut self) -> Result<Ast, ParserError> {
        match self.lexer.peek()?.expect("unterminated expression") {
            TokenSpan {
                token: Token::Identifier(ident),
                ..
            } => {
                //println!("fn arg ref {:?}", ident);
                self.lexer.next()?;
                let fn_arg = self.get_arg(&ident);
                if let Some(arg_idx) = fn_arg {
                    Ok(Ast::new(Expr::ReferenceArg(arg_idx)))
                } else {
                    Ok(Ast::new(Expr::Reference(ident)))
                }
            }
            _ => self.parse_expr(),
        }
    }

    fn parse_second_arg_to_infix_op(&mut self, arg1: Ast) -> Result<Ast, ParserError> {
        match self.lexer.next()? {
            Some(TokenSpan {
                token: Token::Operator(op),
                ..
            }) => {
                // turn it into a fn call
                let arg2 = self.parse_expr()?;
                Ok(Ast::new(Expr::CallFunction(AstCallFunction {
                    calling_convention: CallingConvention::Variable(format!("op_{}", op)),
                    args: vec![arg1, arg2],
                })))
            }
            s => {
                return error::parser(format!(
                    "Expected operator token, was {}",
                    s.map_or("EOF".into(), |s| s.token_string(&self.file.content))
                ))
                .into();
            }
        }
    }

    fn parse_expr(&mut self) -> Result<Ast, ParserError> {
        let token_span = self.lexer.peek()?.expect("unterminated expression");
        match token_span.token {
            Token::Let => self.parse_let_expr(),
            Token::LeftParenthesis => {
                self.lexer.next()?;
                let token_span = self.lexer.peek()?.expect("unterminated parens");
                let exp = match token_span.token {
                    Token::Newline(_) => {
                        self.lexer.next()?;
                        self.parse_block()
                    }
                    _ => self.parse_bin_ops(),
                };
                require!(self, Token::RightParenthesis);
                exp
            }
            Token::String(s) => {
                self.lexer.next()?;
                Ok(Ast::new(Expr::StringConstant(s)))
            }
            Token::Integer(i) => {
                self.lexer.next()?;
                Ok(Ast::new(Expr::IntegerConstant(i)))
            }
            Token::Double(d) => {
                self.lexer.next()?;
                Ok(Ast::new(Expr::DoubleConstant(d)))
            }
            Token::True => {
                self.lexer.next()?;
                Ok(Ast::new(Expr::BoolConstant(true)))
            }
            Token::False => {
                self.lexer.next()?;
                Ok(Ast::new(Expr::BoolConstant(false)))
            }
            Token::Unit => {
                self.lexer.next()?;
                Ok(Ast::new(Expr::UnitConstant))
            }
            Token::Identifier(_) => self.parse_fn_call_or_reference_or_assignment(),
            _ => todo!(
                "not sure which expr this is {}",
                token_span.token_string(&self.file.content)
            ),
        }
    }

    fn parse_fn_call_or_reference_or_assignment(&mut self) -> Result<Ast, ParserError> {
        // 1. get first ident
        let ident = require!(self, inner Token::Identifier);
        // 2. if ident = fn, look up how many args it takes and try to get as many args
        let (known_count, expected_args) = match self.get_fn_arg_count(&ident) {
            None => {
                if let Some(Token::Newline(_)) = self.lexer.peek()?.map(|x| x.token) {
                    return Ok(Ast::new(
                        self.get_arg(&ident)
                            .map(Expr::ReferenceArg)
                            .unwrap_or_else(|| Expr::Reference(ident.clone())),
                    ));
                }
                (false, 0)
            }
            Some(count) => (true, count),
        };

        // 3. start parsing arguments until:
        //  * all args are given
        //  * we meet an operator
        //  * we meet a right parenthesis ')'
        //  * we meet a newline without increased indentation
        // 4. handle amount of args:
        //  * if no args given, it's a reference
        //  * if some args given, it's partial application
        //  * if all args given, it's a call
        let mut args = Vec::<Ast>::new();

        if known_count {
            for _ in 0..expected_args {
                let peeked = self.lexer.peek()?;
                match peeked {
                    None => {
                        return match args.len() {
                            // fn ref
                            0 => self.make_reference(ident),
                            _ => self.make_partial_call(ident, args, expected_args),
                        };
                    }
                    Some(token) => {
                        match token.token {
                            Token::Newline(_) => {
                                // TODO: implement allowing arguments on more indented newline
                                return match args.len() {
                                    // fn ref
                                    0 => self.make_reference(ident),
                                    _ => self.make_partial_call(ident, args, expected_args),
                                };
                            }
                            Token::RightParenthesis | Token::Operator(_) => {
                                return match args.len() {
                                    // fn ref
                                    0 => self.make_reference(ident),
                                    _ => self.make_partial_call(ident, args, expected_args),
                                };
                            }
                            Token::Identifier(arg_ident) => {
                                // parse var reference or fn reference
                                args.push(self.make_reference(arg_ident)?);
                                self.lexer.next()?;
                            }
                            _ => {
                                // parse argument as expression
                                args.push(self.parse_expr()?);
                            }
                        }
                    }
                }
            }
        } else {
            loop {
                let peeked = self.lexer.peek()?;
                match peeked {
                    None => return self.make_fn_call(ident, args),
                    Some(token) => {
                        match token.token {
                            // not allowing more arguments after newline unless we have an arg count
                            Token::Newline(_) | Token::RightParenthesis | Token::Operator(_) => {
                                return match args.len() {
                                    // fn ref
                                    0 => self.make_reference(ident),
                                    _ => return self.make_fn_call(ident, args),
                                };
                            }
                            Token::Identifier(arg_ident) => {
                                // parse var reference or fn reference
                                args.push(self.make_reference(arg_ident)?);
                                self.lexer.next()?;
                            }
                            _ => {
                                // parse argument as expression
                                args.push(self.parse_expr()?);
                            }
                        }
                    }
                }
            }
        }

        // full function call
        self.make_fn_call(ident, args)
    }

    fn make_calling_convention(&self, ident: String) -> CallingConvention {
        self.get_arg(&ident)
            .map(CallingConvention::Arg)
            .unwrap_or_else(|| CallingConvention::Variable(ident))
    }

    fn make_fn_call(&self, ident: String, args: Vec<Ast>) -> Result<Ast, ParserError> {
        Ok(Ast::new(Expr::CallFunction(AstCallFunction {
            calling_convention: self.make_calling_convention(ident),
            args,
        })))
    }

    fn make_partial_call(
        &self,
        ident: String,
        args: Vec<Ast>,
        expected_args: usize,
    ) -> Result<Ast, ParserError> {
        Ok(Ast::new(Expr::PartialCallFunction(
            AstPartialCallFunction {
                fn_name: format!("{} partial {}", &ident, &args.len()),
                calling_convention: self.make_calling_convention(ident),
                missing_args: expected_args - args.len(),
                args,
            },
        )))
    }

    fn make_reference(&mut self, ident: String) -> Result<Ast, ParserError> {
        Ok(Ast::new(
            self.get_arg(&ident)
                .map(Expr::ReferenceArg)
                .unwrap_or_else(|| Expr::Reference(ident)),
        ))
    }
}
