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

// TODO: remove this allow when the implementation is further along
#![allow(dead_code, unused)]

use crate::file::{FileTracker, LettlangFile};
use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::env;

pub(crate) mod ast;
pub(crate) mod compiler;
pub(crate) mod error;
pub(crate) mod file;
pub(crate) mod flags;
pub(crate) mod interpreter;
pub(crate) mod lexer;
pub(crate) mod op_code_printer;
pub(crate) mod parser;
pub(crate) mod prelude;
pub(crate) mod scope;
pub(crate) mod token;
pub(crate) mod value;

fn main() {
    // Possibly better solution to custom operator lexing:
    // Trädstruktur där rötterna är varje custom operators första bokstav eller symbol (tror det är så några regex-lösningar funkar)
    // bygg upp/endre (hvis den begynner med samme bokstav som noen annen) operator-trädet medan du bygger operator-stringen
    // Text-operators måste vara separerade med någon typ av space, symbol-operators behöver ingen space för att separera argument
    // get_identifier måste bli anpassad så den också hanterar custom operators om den matchar operator-trädet

    //Infix surround operator stuff? probably an awful idea, should implement it
    //let x `[` y `]` = ()

    // TODO: go through the implementation of partial application

    if let Some(file) = env::args().find(|a| a.ends_with(".lett")) {
        let mut file_tracker = FileTracker::new();
        let file_id = 1;
        let code = std::fs::read_to_string(&file).unwrap();
        file_tracker.add(LettlangFile::new(file_id, file, code));

        let parser = Parser::new(file_tracker.get(file_id).unwrap(), &file_tracker);
        let mut args = env::args();

        if args.any(|x| &x == "--opcodes") {
            op_code_printer::walk(parser);
        } else {
            let mut interpreter = Interpreter::new(parser);
            let result = interpreter.interpret().unwrap();
            println!("result of script: {:?}", result);
        }
        return;
    }

    let prelude = include_str!("../prelude.lett");
    let code = include_str!("../test.lett");
    let code = vec![prelude, code].join("\n\n");
    let mut file_tracker = FileTracker::new();
    let file_id = 1;
    file_tracker.add(LettlangFile::new(file_id, "test.lett".to_string(), code));

    let parser = Parser::new(file_tracker.get(file_id).unwrap(), &file_tracker);
    let mut args = env::args();

    if args.any(|x| &x == "--opcodes") {
        op_code_printer::walk(parser);
    } else {
        let mut interpreter = Interpreter::new(parser);
        let result = interpreter.interpret().unwrap();
        println!("result of script: {:?}", result);
    }
}
