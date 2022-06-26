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

#[derive(Debug)]
pub enum Value {
    Unit,
    Integer(i64),
    Double(f64),
    String(String),
    Bool(bool),
    Function(std::rc::Rc<crate::compiler::CompiledFunction>),
}

impl Value {
    pub const fn int(value: i64) -> Value {
        Value::Integer(value)
    }

    pub const fn double(value: f64) -> Value {
        Value::Double(value)
    }

    pub const fn string(value: String) -> Value {
        Value::String(value)
    }

    pub const fn unit() -> Value {
        Value::Unit
    }

    pub const fn bool(value: bool) -> Value {
        Value::Bool(value)
    }

    pub fn coerce_string(&self) -> String {
        match self {
            Value::Unit => "()".into(),
            Value::Integer(i) => i.to_string(),
            Value::Double(d) => d.to_string(),
            Value::String(s) => s.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Function(d) => format!("{} {}/{}", d.name, d.arg_len, d.arg_len),
        }
    }

    pub fn is_unit(&self) -> bool {
        match self {
            Value::Unit => true,
            _ => false,
        }
    }

    pub fn add(a: &Self, b: &Self) -> Self {
        match (a, b) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
            (Value::Integer(a), Value::Double(b)) => Value::Double(*a as f64 + b),
            (Value::Integer(a), Value::String(b)) => Value::String(a.to_string() + b.as_str()),
            (Value::Integer(a), Value::Bool(b)) => Value::Integer(a + (if *b { 1 } else { 0 })), // probably a bad idea :-)
            (Value::Integer(a), Value::Unit) => Value::Integer(*a),
            (Value::Double(a), Value::Integer(b)) => Value::Double(a + *b as f64),
            (Value::Double(a), Value::Double(b)) => Value::Double(a + b),
            (Value::Double(a), Value::String(b)) => Value::String(a.to_string() + b.as_str()),
            (Value::Double(a), Value::Bool(b)) => Value::Double(a + (if *b { 1. } else { 0. })), // probably a bad idea :-)
            (Value::Double(a), Value::Unit) => Value::Double(*a),
            (Value::String(a), Value::Integer(b)) => {
                Value::String(a.clone() + b.to_string().as_str())
            }
            (Value::String(a), Value::Double(b)) => {
                Value::String(a.clone() + b.to_string().as_str())
            }
            (Value::String(a), Value::String(b)) => Value::String(a.clone() + b.as_str()),
            (Value::String(a), Value::Bool(b)) => Value::String(a.clone() + b.to_string().as_str()),
            (Value::String(a), Value::Unit) => Value::String(a.clone()),
            (Value::Bool(a), Value::Integer(b)) => Value::Integer(if *a { 1 } else { 0 } + b),
            (Value::Bool(a), Value::Double(b)) => Value::Double(if *a { 1. } else { 0. } + b),
            (Value::Bool(a), Value::String(b)) => Value::String(a.to_string() + b),
            (Value::Bool(a), Value::Bool(b)) => {
                Value::Integer(if *a { 1 } else { 0 } + if *b { 1 } else { 0 })
            } // probably a bad idea :-)
            (Value::Bool(a), Value::Unit) => Value::Bool(*a),
            (Value::Unit, Value::Integer(b)) => Value::Integer(*b),
            (Value::Unit, Value::Double(b)) => Value::Double(*b),
            (Value::Unit, Value::String(b)) => Value::String(b.clone()),
            (Value::Unit, Value::Bool(b)) => Value::Bool(*b),
            (Value::Unit, Value::Unit) => Value::Unit,
            (Value::Function(_), _) => panic!("Functions cannot be added using addition"),
            (_, Value::Function(_)) => panic!("Functions cannot be added using addition"),
        }
    }

    pub fn sub(a: &Self, b: &Self) -> Self {
        match (a, b) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
            (Value::Integer(a), Value::Double(b)) => Value::Double(*a as f64 - b),
            (Value::Integer(a), Value::Bool(b)) => Value::Integer(a - (if *b { 1 } else { 0 })), // probably a bad idea :-)
            (Value::Integer(a), Value::Unit) => Value::Integer(*a),
            (Value::Double(a), Value::Integer(b)) => Value::Double(a - *b as f64),
            (Value::Double(a), Value::Double(b)) => Value::Double(a - b),
            (Value::Double(a), Value::Bool(b)) => Value::Double(a - (if *b { 1. } else { 0. })), // probably a bad idea :-)
            (Value::Double(a), Value::Unit) => Value::Double(*a),
            (Value::String(a), Value::Unit) => Value::String(a.clone()),
            (Value::Bool(a), Value::Integer(b)) => Value::Integer(if *a { 1 } else { 0 } - b),
            (Value::Bool(a), Value::Double(b)) => Value::Double(if *a { 1. } else { 0. } - b),
            (Value::Bool(a), Value::Bool(b)) => {
                Value::Integer(if *a { 1 } else { 0 } - if *b { 1 } else { 0 })
            } // probably a bad idea :-)
            (Value::Bool(a), Value::Unit) => Value::Bool(*a),
            (Value::Unit, Value::Integer(b)) => Value::Integer(*b),
            (Value::Unit, Value::Double(b)) => Value::Double(*b),
            (Value::Unit, Value::String(b)) => Value::String(b.clone()),
            (Value::Unit, Value::Bool(b)) => Value::Bool(*b),
            (Value::Unit, Value::Unit) => Value::Unit,
            (a, b) => panic!("Subtraction of {:?} and {:?} is not allowed", a, b),
        }
    }

    pub fn eq(a: &Self, b: &Self) -> bool {
        match (a, b) {
            (Value::Integer(a), Value::Integer(b)) => (a == b),
            (Value::Integer(a), Value::Double(b)) => (*a as f64 == *b),
            (Value::Integer(_), Value::String(_)) => false,
            (Value::Integer(a), Value::Bool(b)) => (*a == (if *b { 1i64 } else { 0i64 })), // probably a bad idea :-)
            (Value::Integer(_), Value::Unit) => false,
            (Value::Double(a), Value::Integer(b)) => (*a == *b as f64),
            (Value::Double(a), Value::Double(b)) => (a == b),
            (Value::Double(_), Value::String(_)) => false,
            (Value::Double(a), Value::Bool(b)) => (*a == (if *b { 1. } else { 0. })), // probably a bad idea :-)
            (Value::Double(_), Value::Unit) => false,
            (Value::String(_), Value::Integer(_)) => false,
            (Value::String(_), Value::Double(_)) => false,
            (Value::String(a), Value::String(b)) => (a == b),
            (Value::String(_), Value::Bool(_)) => false,
            (Value::String(_), Value::Unit) => false,
            (Value::Bool(_), Value::Integer(_)) => false,
            (Value::Bool(_), Value::Double(_)) => false,
            (Value::Bool(_), Value::String(_)) => false,
            (Value::Bool(a), Value::Bool(b)) => (a == b),
            (Value::Bool(a), Value::Unit) => (*a),
            (Value::Unit, Value::Integer(_)) => false,
            (Value::Unit, Value::Double(_)) => false,
            (Value::Unit, Value::String(_)) => false,
            (Value::Unit, Value::Bool(_)) => false,
            (Value::Unit, Value::Unit) => true,
            (Value::Function(_), _) => panic!("Functions cannot be compared yet"),
            (_, Value::Function(_)) => panic!("Functions cannot be compared yet"),
        }
    }
}
