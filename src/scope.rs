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

use crate::value::Value;
use std::collections::HashMap;
use std::rc::Rc;

pub struct ScopeStack(Vec<Scope>);

impl ScopeStack {
    pub fn new() -> Self {
        Self(vec![Scope::new()])
    }

    pub fn define_variable(&mut self, var: String, val: Rc<Value>) {
        self.0.last_mut().unwrap().define_variable(var, val);
    }

    pub fn push_scope(&mut self) {
        self.0.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        self.0.pop();
    }

    /// Set value of variable in its containing scope
    pub fn set_variable(&mut self, var: String, val: Rc<Value>) {
        for scope in self.0.iter_mut().rev() {
            if scope.has_variable(var.as_ref()) {
                scope.set_variable(var, val);
                return;
            }
        }
        panic!("Variable {} has not been defined yet.", &var);
    }

    pub fn get_variable(&self, var: &str) -> Rc<Value> {
        for scope in self.0.iter().rev() {
            if scope.has_variable(var.as_ref()) {
                return scope.get_variable(var);
            }
        }
        panic!("Variable {} has not been defined yet.", &var);
    }
}

pub struct Scope {
    vars: HashMap<String, Rc<Value>>,
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

impl Scope {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    /// Define variable in current scope
    pub fn define_variable(&mut self, var: String, val: Rc<Value>) {
        // TODO: error on defining the same variable twice
        self.vars
            .entry(var)
            .and_modify(|e| *e = Rc::clone(&val))
            .or_insert_with(|| Rc::clone(&val));
    }

    /// Set value of variable in its containing scope
    pub fn set_variable(&mut self, var: String, val: Rc<Value>) {
        self.vars.insert(var, val);
    }

    pub fn has_variable(&self, var: &str) -> bool {
        self.vars.contains_key(var)
    }

    pub fn get_variable(&self, var: &str) -> Rc<Value> {
        Rc::clone(self.vars.get(var).unwrap())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn scope_get_var_from_parent() {
        let mut scope_stack = ScopeStack::new();
        scope_stack.define_variable("test".into(), Rc::new(Value::bool(true)));
        scope_stack.push_scope();
        let test_var = scope_stack.get_variable("test");

        match *test_var {
            Value::Bool(b) => assert!(b),
            _ => unreachable!(),
        }
    }

    #[test]
    pub fn scope_get_local_var_that_also_exists_in_parent() {
        let mut scope_stack = ScopeStack::new();
        scope_stack.define_variable("some_var".into(), Rc::new(Value::bool(false)));
        scope_stack.push_scope();
        scope_stack.define_variable("some_var".into(), Rc::new(Value::bool(true)));

        let some_var = scope_stack.get_variable("some_var");

        match *some_var {
            Value::Bool(b) => assert!(b),
            _ => unreachable!(),
        }
    }

    #[test]
    pub fn scope_get_local_var_that_only_exists_in_child() {
        let mut scope_stack = ScopeStack::new();
        scope_stack.define_variable("some_var2".into(), Rc::new(Value::bool(false)));
        scope_stack.push_scope();
        scope_stack.define_variable("some_var1".into(), Rc::new(Value::bool(true)));

        let some_var = scope_stack.get_variable("some_var1");

        match *some_var {
            Value::Bool(b) => assert!(b),
            _ => unreachable!(),
        }
    }

    #[test]
    #[should_panic]
    pub fn scope_try_get_var_that_is_not_defined() {
        let mut scope_stack = ScopeStack::new();
        scope_stack.define_variable("some_var".into(), Rc::new(Value::bool(false)));
        scope_stack.push_scope();
        scope_stack.define_variable("some_var".into(), Rc::new(Value::bool(true)));

        let _ = scope_stack.get_variable("not_defined");
    }
}
