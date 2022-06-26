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

use crate::interpreter::Op;
use crate::value::Value;
use std::rc::Rc;

macro_rules! dispatcher {
    ($name:expr, $args:expr, [ $($fn:ident),* ]) => {
        match $name {
            $(
            stringify!($fn) => Some($fn($args)),
            )*
            _ => None,
        }
    }
}

//macro_rules! dispatcher_fns {
//    ($name:expr, [ $($fn:ident),* ]) => {
//        match $name {
//            $(
//            stringify!($fn) => true,
//            )*
//            _ => false,
//        }
//    }
//}
//
//pub fn has_fn(fn_name: &str) -> bool {
//    dispatcher_fns!(fn_name, [ prelude_print, prelude_add ])
//}

pub fn try_dispatch(fn_name: &str, args: Vec<Rc<Value>>) -> Option<Rc<Value>> {
    dispatcher!(
        fn_name,
        args,
        [
            prelude_add,
            prelude_sub,
            prelude_eq,
            prelude_if,
            prelude_is_fn,
            prelude_print,
            prelude_throw
        ]
    )
}

fn prelude_throw(args: Vec<Rc<Value>>) -> Rc<Value> {
    let first_arg = args.first().expect("prelude_throw requires 1 arg");
    if !first_arg.is_unit() {
        panic!("{}", first_arg.coerce_string());
    } else {
        panic!();
    }
}

fn prelude_print(args: Vec<Rc<Value>>) -> Rc<Value> {
    println!(
        "{}",
        args.first()
            .expect("prelude_print requires 1 arg")
            .coerce_string()
    );
    Rc::new(Value::unit())
}

fn prelude_add(args: Vec<Rc<Value>>) -> Rc<Value> {
    let (arg1, arg2) = (
        args.get(0).expect("prelude_add requires 2 args"),
        args.get(1).expect("prelude_add requires 2 args"),
    );

    Rc::new(Value::add(arg1, arg2))
}
fn prelude_sub(args: Vec<Rc<Value>>) -> Rc<Value> {
    let (arg1, arg2) = (
        args.get(0).expect("prelude_sub requires 2 args"),
        args.get(1).expect("prelude_sub requires 2 args"),
    );

    Rc::new(Value::sub(arg1, arg2))
}

fn prelude_eq(args: Vec<Rc<Value>>) -> Rc<Value> {
    let (arg1, arg2) = (
        args.get(0).expect("prelude_eq requires 2 args"),
        args.get(1).expect("prelude_eq requires 2 args"),
    );

    Rc::new(Value::Bool(Value::eq(arg1, arg2)))
}
fn prelude_if(args: Vec<Rc<Value>>) -> Rc<Value> {
    let (arg1, arg2, arg3) = (
        args.get(0).expect("prelude_if requires 3 args"),
        args.get(1).expect("prelude_if requires 3 args"),
        args.get(2).expect("prelude_if requires 3 args"),
    );

    let then_or_else = if let Value::Bool(true) = **arg1 {
        arg2
    } else {
        arg3
    };

    if let Value::Function(_) = **then_or_else {
        Rc::clone(then_or_else)
    } else {
        Rc::new(Value::Function(Rc::new(
            crate::compiler::CompiledFunction {
                name: String::from("anon #if"),
                code: Rc::new(vec![Op::Push(Rc::clone(then_or_else)), Op::Return]),
                arg_len: 1,
            },
        )))
    }
}

fn prelude_is_fn(args: Vec<Rc<Value>>) -> Rc<Value> {
    let arg1 = args.get(0).expect("prelude_is_fn requires 1 args");

    Rc::new(Value::Bool(matches!(**arg1, Value::Function(_))))
}
