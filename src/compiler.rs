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

use crate::ast::{Ast, Expr};
use crate::interpreter::{CallingConvention, Op};
use crate::value::Value;
use std::rc::Rc;

#[derive(Debug)]
pub struct CompiledFunction {
    pub name: String,
    pub arg_len: usize,
    pub code: Rc<Vec<Op>>,
}

pub struct Compiler {
    funcs: Vec<Rc<CompiledFunction>>,
    pub unit_const: Rc<Value>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            funcs: Vec::new(),
            unit_const: Rc::new(Value::unit()),
        }
    }

    pub fn get_fn(&self, fn_name: &str) -> Option<Rc<CompiledFunction>> {
        self.funcs
            .iter()
            .find(|&f| f.name == fn_name)
            .map(Rc::clone)
    }

    pub fn compile(&mut self, ast: &Ast, root: bool) -> Vec<Op> {
        let mut code: Vec<Op> = vec![];
        match *ast.expr {
            Expr::DefineFunction(ref definition) => {
                let mut compiled_code = self.compile(&definition.code, false);
                compiled_code.push(Op::Return);
                let new_func = Rc::new(CompiledFunction {
                    name: definition.name.clone(),
                    arg_len: definition.args.len(),
                    code: Rc::new(compiled_code),
                });

                code.push(Op::Push(Rc::new(Value::Function(Rc::clone(&new_func)))));

                self.funcs.push(new_func);
            }
            Expr::PartialCallFunction(ref partial) => {
                // a partial call is basically a combination of DefineFunction and CallFunction
                let mut partial_func_code: Vec<Op> = partial
                    .args
                    .iter()
                    .flat_map(|arg| self.compile(arg, false))
                    .collect();
                for i in 0..partial.missing_args {
                    partial_func_code.push(Op::RefArg(i));
                }

                // TODO: fix this, it's not going to work in certain cases
                // figure out a better way to implement partial application
                let fn_name = match &partial.calling_convention {
                    CallingConvention::Variable(f) => f,
                    f => panic!("compiler: could not find function for partial application, was not variable: `{:?}`", f),
                };

                let called_fn = self
                    .get_fn(fn_name)
                    .unwrap_or_else(|| panic!("compiler: could not find function `{}`", fn_name));

                //partial_func_code.push(Op::Call(
                //    partial.calling_convention.clone(),
                //    partial.args.len() + partial.missing_args,
                //));

                partial_func_code.push(Op::Call(
                    CallingConvention::Reference(called_fn),
                    partial.args.len() + partial.missing_args,
                ));

                partial_func_code.push(Op::Return);

                let partial_func = Rc::new(CompiledFunction {
                    name: partial.fn_name.clone(),
                    arg_len: partial.args.len(),
                    code: Rc::new(partial_func_code),
                });

                code.push(Op::Push(Rc::new(Value::Function(Rc::clone(&partial_func)))));
                self.funcs.push(partial_func);
            }
            Expr::CallFunction(ref call) => {
                let mut args_code: Vec<Op> = call
                    .args
                    .iter()
                    .flat_map(|arg| self.compile(arg, false))
                    .collect();
                code.append(&mut args_code);
                code.push(Op::Call(call.calling_convention.clone(), call.args.len()));
                if root {
                    code.push(Op::Pop);
                }
            }
            Expr::Reference(ref reference) => {
                code.push(Op::Ref(reference.clone()));
            }
            Expr::AssignmentLet(ref assignment) => {
                let mut assign_code = self.compile(&assignment.code, false);
                code.append(&mut assign_code);
                code.push(Op::AssignLet(assignment.name.clone()));
            }
            Expr::DoubleConstant(d) => code.push(Op::Push(Rc::new(Value::double(d)))),
            Expr::IntegerConstant(i) => code.push(Op::Push(Rc::new(Value::int(i)))),
            Expr::StringConstant(ref s) => code.push(Op::Push(Rc::new(Value::string(s.clone())))),
            Expr::BoolConstant(b) => code.push(Op::Push(Rc::new(Value::bool(b)))),
            Expr::UnitConstant => code.push(Op::Push(Rc::clone(&self.unit_const))),
            Expr::Block(ref blk) => {
                let len = blk.len();
                if len > 1 {
                    // treat all expressions in a block as "root" expressions when compiling
                    code.extend(
                        blk.iter()
                            .take(len - 1)
                            .flat_map(|exp| self.compile(exp, true)),
                    );
                    // except for the last one, where we want the result to act as a return value
                    code.extend(self.compile(blk.last().unwrap(), false));
                } else {
                    code.extend(blk.iter().flat_map(|exp| self.compile(exp, false)));
                }
            }
            Expr::ReferenceArg(idx) => code.push(Op::RefArg(idx)),
            Expr::AssignmentArg(_) => {}
            Expr::Reassignment(ref assignment) => {
                let mut assign_code = self.compile(&assignment.code, false);
                code.append(&mut assign_code);
                code.push(Op::Reassign(assignment.name.clone()));
            }
        }
        code
    }
}
