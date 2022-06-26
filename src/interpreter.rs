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

use crate::compiler::Compiler;
use crate::error::InterpreterError;
use crate::scope::ScopeStack;
use crate::value::Value;
use crate::{prelude, Parser};
use std::rc::Rc;

pub struct Interpreter<'a> {
    parser: Parser<'a>,
    compiler: Compiler,
    last_pop: Rc<Value>,
}

#[derive(Debug, Clone)]
pub struct CallFrame {
    name: String,
    args: Vec<Rc<Value>>,
    code: Rc<Vec<Op>>,
    addr: usize,
}

enum InterpreterResult {
    Call,
    Return,
    Stay,
}

impl<'a> Interpreter<'a> {
    pub fn new(parser: Parser<'a>) -> Self {
        Self {
            parser,
            compiler: Compiler::new(),
            last_pop: Rc::new(Value::unit()),
        }
    }

    pub fn interpret(&mut self) -> Result<Rc<Value>, InterpreterError> {
        let mut call_stack: Vec<CallFrame> = vec![];
        let mut eval_stack: Vec<Rc<Value>> = Vec::new();
        let mut scope = ScopeStack::new();

        while let Some(syn) = self.parser.next()? {
            //println!("ast: {:?}", syn);

            // if no call frame in call_stack (stack empty)
            // then execute whatever is compiled,
            // otherwise execute the current call frame until call_stack is empty again
            let code = self.compiler.compile(&syn, true);
            //println!("compiled code: {:?}", code);
            for op in code {
                // global scope code
                //println!("eval_stack {:?}", &eval_stack);
                self.interpret_op(op, &mut eval_stack, &mut scope, &mut call_stack);
                while let Some(current_frame) = call_stack.last() {
                    //let cf = current_frame.clone();
                    //println!("{:#?}", cf);
                    let op = current_frame.code.get(current_frame.addr).unwrap();
                    self.interpret_op(op.clone(), &mut eval_stack, &mut scope, &mut call_stack);
                }
            }
        }

        println!("eval stack on exit {:?}", eval_stack);

        Ok(eval_stack
            .pop()
            .unwrap_or_else(|| Rc::clone(&self.compiler.unit_const)))
        //.filter(|v| !matches!(&**v, Value::Unit))
        //.unwrap_or_else(|| Rc::clone(&self.last_pop)))
        // 1. parse next expression
        // 1.1 if defining new operator,
        //     add it to the lexer so it can be correctly lexed + parsed when it appears
        // 2. compile expression
        // 3. interpret it
    }

    fn interpret_op(
        &mut self,
        op: Op,
        eval_stack: &mut Vec<Rc<Value>>,
        scope: &mut ScopeStack,
        call_stack: &mut Vec<CallFrame>,
    ) -> InterpreterResult {
        //println!("eval stack {:?}", eval_stack);
        match op {
            Op::Call(call_conv, arg_count) => {
                //println!("call stack {:#?}", call_stack);
                //println!("eval stack {:#?}", eval_stack);
                let mut args: Vec<Rc<Value>> = (0..arg_count)
                    .map(|x| {
                        eval_stack.pop().unwrap_or_else(|| match call_stack.last() {
                            None => {
                                panic!(
                                    "not enough arguments ({}<{}) to fn {:?} inside fn 'main'",
                                    x - 1,
                                    arg_count,
                                    call_conv,
                                )
                            }
                            Some(cf) => {
                                panic!(
                                    "not enough arguments ({}<{}) to fn {:?} inside fn '{}'\nargs:\n{}",
                                    x - 1,
                                    arg_count,
                                    call_conv,
                                    cf.name,
                                    cf.args
                                        .iter()
                                        .map(|x| x.coerce_string())
                                        .collect::<Vec<String>>()
                                        .join(" "),
                                )
                            }
                        })
                    })
                    .collect();
                args.reverse();

                match call_conv {
                    CallingConvention::Arg(arg_idx) => {
                        let fn_value = call_stack
                            .last()
                            .expect("tried to get function arg from global scope")
                            .args
                            .get(arg_idx)
                            .map(Rc::clone)
                            .unwrap_or_else(|| panic!("could not find function {:?}", call_conv));

                        let called_fn = match &*fn_value {
                            Value::Function(d) => d,
                            v => panic!(
                                "could not find function '{:?}', was a {:?}, inside fn {}",
                                call_conv,
                                v,
                                call_stack.last().unwrap().name
                            ),
                        };

                        let frame = CallFrame {
                            name: called_fn.name.clone(),
                            args,
                            code: Rc::clone(&called_fn.code),
                            addr: 0,
                        };

                        if let Some(cf) = call_stack.last_mut() {
                            cf.addr += 1;
                        }

                        call_stack.push(frame);
                        scope.push_scope();
                    }
                    CallingConvention::Variable(fn_name) => {
                        if let Some(ret_val) = prelude::try_dispatch(&fn_name, args.clone()) {
                            eval_stack.push(ret_val);

                            if let Some(cf) = call_stack.last_mut() {
                                cf.addr += 1;
                            }
                        } else {
                            let called_fn = scope.get_variable(&fn_name);
                            let called_fn = match &*called_fn {
                                Value::Function(d) => d,
                                _ => panic!("could not find function '{}'", fn_name),
                            };

                            let frame = CallFrame {
                                name: fn_name,
                                args,
                                code: Rc::clone(&called_fn.code),
                                addr: 0,
                            };

                            if let Some(cf) = call_stack.last_mut() {
                                cf.addr += 1;
                            }

                            call_stack.push(frame);
                            scope.push_scope();
                        }
                    }
                    CallingConvention::Reference(called_fn) => {
                        let frame = CallFrame {
                            name: called_fn.name.clone(),
                            args,
                            code: Rc::clone(&called_fn.code),
                            addr: 0,
                        };

                        if let Some(cf) = call_stack.last_mut() {
                            cf.addr += 1;
                        }

                        call_stack.push(frame);
                        scope.push_scope();
                    }
                }
                return InterpreterResult::Call;
            }
            Op::AssignArg(_) => {}
            Op::RefArg(idx) => {
                eval_stack.push(
                    call_stack
                        .last()
                        .expect("tried to get function arg from global scope")
                        .args
                        .get(idx)
                        .map(Rc::clone)
                        .unwrap(),
                );
            }
            Op::AssignLet(var) => {
                let val = eval_stack.pop().unwrap();
                scope.define_variable(var, val);
            }
            Op::Reassign(var) => {
                let val = eval_stack.pop().unwrap();
                scope.set_variable(var, val);
            }
            Op::Ref(var) => {
                let val = scope.get_variable(var.as_str());
                eval_stack.push(val);
            }
            Op::Return => {
                call_stack.pop();
                scope.pop_scope();
                return InterpreterResult::Return;
            }
            Op::BrTrue(_) => {}
            Op::BrFalse(_) => {}
            Op::Jmp(_) => {}
            Op::Pop => {
                if let Some(last) = eval_stack.pop() {
                    self.last_pop = last;
                }
            }
            Op::Push(val) => {
                eval_stack.push(val);
            }
        }
        if let Some(cf) = call_stack.last_mut() {
            cf.addr += 1;
        }
        InterpreterResult::Stay
    }
}

#[derive(Debug, Clone)]
pub enum Op {
    /// (Function name, argument count)
    Call(CallingConvention, usize),
    /// (argument index)
    AssignArg(usize),
    /// (argument index)
    RefArg(usize),
    AssignLet(String),
    Reassign(String),
    Ref(String),
    Return,
    BrTrue(usize),
    BrFalse(usize),
    Jmp(usize),
    Pop,
    Push(Rc<Value>),
}

#[derive(Debug, Clone)]
pub enum CallingConvention {
    /// Calls a function found in the given argument to the current functions
    Arg(usize),
    /// Calls a function found in the nearest scope with the given identifier
    Variable(String),
    /// Calls a function found in the given reference
    Reference(std::rc::Rc<crate::compiler::CompiledFunction>),
}
