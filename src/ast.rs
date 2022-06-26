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

use crate::interpreter::CallingConvention;
use std::rc::Rc;

#[derive(Debug)]
pub enum Expr {
    DefineFunction(AstDefineFunction),
    CallFunction(AstCallFunction),
    PartialCallFunction(AstPartialCallFunction),
    StringConstant(String),
    Reference(String),
    ReferenceArg(usize),
    AssignmentArg(AstAssignmentArg),
    AssignmentLet(AstAssignmentLet),
    Reassignment(AstReassignment),
    IntegerConstant(i64),
    DoubleConstant(f64),
    BoolConstant(bool),
    UnitConstant,
    Block(Vec<Ast>),
}

#[derive(Debug)]
pub struct AstDefineFunction {
    pub name: String,
    pub args: Vec<String>,
    pub code: Ast,
}

#[derive(Debug)]
pub struct AstAssignmentArg {
    pub idx: usize,
    pub code: Ast,
}

#[derive(Debug)]
pub struct AstAssignmentLet {
    pub name: String,
    pub code: Ast,
}

#[derive(Debug)]
pub struct AstReassignment {
    pub name: String,
    pub code: Ast,
}

#[derive(Debug)]
pub struct AstCallFunction {
    pub calling_convention: CallingConvention,
    pub args: Vec<Ast>,
}

#[derive(Debug)]
pub struct AstPartialCallFunction {
    pub fn_name: String,
    pub calling_convention: CallingConvention,
    pub args: Vec<Ast>,
    pub missing_args: usize,
}

#[derive(Debug)]
pub struct Ast {
    pub expr: Rc<Expr>,
}

impl Ast {
    pub fn new(expr: Expr) -> Self {
        Self {
            expr: Rc::new(expr),
        }
    }
}
