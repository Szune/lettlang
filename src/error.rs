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

use crate::token::Span;

pub enum LettlangError {
    Lexer(LexerError),
    Parser(ParserError),
    Interpreter(InterpreterError),
}

#[derive(Debug)]
pub struct LexerError {
    pub msg: String,
    pub span: Span,
}

impl LexerError {
    pub fn new(msg: String, span: Span) -> Self {
        Self { msg, span }
    }
}

#[derive(Debug)]
pub struct ParserError {
    pub msg: String,
}
impl ParserError {
    pub fn new(msg: String) -> Self {
        Self { msg }
    }
}

#[derive(Debug)]
pub struct InterpreterError {
    pub msg: String,
}
impl InterpreterError {
    pub fn new(msg: String) -> Self {
        Self { msg }
    }
}

impl From<LexerError> for ParserError {
    fn from(lex: LexerError) -> Self {
        Self {
            msg: format!("lexer: {}", lex.msg),
        }
    }
}

impl From<ParserError> for Result<crate::ast::Ast, ParserError> {
    fn from(e: ParserError) -> Self {
        Err(e)
    }
}

impl From<ParserError> for InterpreterError {
    fn from(lex: ParserError) -> Self {
        Self {
            msg: format!("parser: {}", lex.msg),
        }
    }
}

pub fn lexer(msg: String, span: Span) -> LexerError {
    LexerError::new(msg, span)
}

pub fn parser(msg: String) -> ParserError {
    ParserError::new(msg)
}

pub fn interpreter(msg: String) -> InterpreterError {
    InterpreterError::new(msg)
}
