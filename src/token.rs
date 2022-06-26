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

// TODO: parser will have to keep track of the type of operator
// when implementing prefix and postfix operators
pub enum OperatorKind {
    Prefix,
    Infix,
    Postfix,
}

#[derive(Debug, Clone)]
pub enum Token {
    Newline(u8),
    Indentation(u8),
    Integer(i64),
    Double(f64),
    LeftParenthesis,
    RightParenthesis,
    Dot,
    String(String),
    OperatorLiteral(String),
    Operator(String),
    Identifier(String),
    Equals,
    Let,
    True,
    False,
    Unit,
    ///// let x \`\[\` y \`\]\` = ()
    //InfixSurroundOperator(String),
    ///// let \`*\` a = ()
    //PrefixOperator(String),
    ///// let a \`*\` b = ()
    //InfixOperator(String),
    ///// let a \`*\` = ()
    //PostfixOperator(String),
}

/// A span of any source, file, string, etc
#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn span_string(&self, code: &String) -> String {
        let span_text = code.get(self.start..=self.end).unwrap_or_else(|| {
            panic!(
                "could not get span {}->{} for error msg (full string len: {})",
                self.start,
                self.end,
                code.len()
            )
        });

        let line = code
            .get(0..=self.start)
            .map(|x| x.lines().count())
            .unwrap_or_else(|| {
                panic!(
                    "bad token span {}->{} (full string len: {})",
                    self.start,
                    self.end,
                    code.len()
                )
            });

        let col = code
            .get(0..=self.start)
            .and_then(|x| x.lines().last().map(|x| x.len()))
            .unwrap_or_else(|| {
                panic!(
                    "bad token span {}->{} (full string len: {})",
                    self.start,
                    self.end,
                    code.len()
                )
            });

        format!(
            "'{}', line {}:{}, span {}->{}",
            span_text, line, col, self.start, self.end
        )
    }
}

/// Contains a token and the span of the full token
#[derive(Debug, Clone)]
pub struct TokenSpan {
    pub span: Span,
    pub token: Token,
}

impl TokenSpan {
    pub fn new(start: usize, end: usize, token: Token) -> Self {
        Self {
            span: Span { start, end },
            token,
        }
    }

    pub fn token_string(&self, code: &String) -> String {
        let span_text = self.span.span_string(code);
        format!("{:?} ({})", self.token, span_text)
    }
}
