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

use crate::ast::Expr;
use crate::compiler::Compiler;
use crate::Parser;

/// Compiles the AST and prints the code
pub fn walk(mut parser: Parser) {
    let mut compiler = Compiler::new();
    while let Some(syn) = parser.next().unwrap() {
        match &*syn.expr {
            Expr::CallFunction(fn_ast) => {
                println!("-- call to {:?} --", fn_ast.calling_convention);
            }
            Expr::AssignmentLet(assignment) => {
                if let Expr::CallFunction(fn_ast) = &*assignment.code.expr {
                    println!(
                        "-- call to {:?} assigns to {} --",
                        fn_ast.calling_convention, assignment.name
                    );
                }
            }
            _ => {}
        };
        let code = compiler.compile(&syn, true);
        println!(
            "{}",
            code.iter()
                .map(|op| format!("{:?}", op))
                .collect::<Vec<String>>()
                .join("\n")
        );
    }
}
