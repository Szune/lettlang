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

use crate::error::LexerError;
use crate::token::{Span, Token, TokenSpan};
use crate::{error, FileTracker, LettlangFile};
use std::collections::{HashSet, VecDeque};
use std::str::Chars;

// Lexer should know nothing about the source of the chars it is lexing
// Errors should be augmented with more information outside of this class
pub struct Lexer<'a> {
    chars: Chars<'a>,
    eof: bool,
    pos: usize,
    buffer: [(usize, char); 2],
    queue: VecDeque<TokenSpan>,
    operators: HashSet<String>,
    peek: Option<TokenSpan>,
    /// use self.newline to ignore any spaces after the first non-space token on a line
    newline: bool,
}

const DEBUG: bool = false;

impl<'a> Lexer<'a> {
    pub fn new(chars: Chars<'a>) -> Lexer<'a> {
        let mut lexer = Lexer {
            chars, //code.chars(),
            eof: false,
            pos: 0,
            buffer: [(0, '\0'), (0, '\0')],
            queue: VecDeque::new(),
            operators: HashSet::new(),
            peek: None,
            newline: false, // first line also counts as a newline
        };
        lexer.fill_buffer();
        lexer
    }

    fn is_eof(&self) -> bool {
        self.eof && self.peek.is_none() && self.queue.is_empty()
    }

    pub fn next(&mut self) -> Result<Option<TokenSpan>, LexerError> {
        // write tests so you can remove the println debug stuff
        let mut was_peek = false;
        let result = if let Some(peek) = &self.peek {
            let tok = peek.clone();
            self.peek = None;
            was_peek = true;
            Some(tok)
        } else if !self.queue.is_empty() {
            self.queue.pop_front()
        } else {
            self.perform_full_match()?
        };

        if !was_peek && DEBUG {
            println!("lexed {:?}", result);
        }
        Ok(result)
    }

    pub fn peek(&mut self) -> Result<Option<TokenSpan>, LexerError> {
        self.peek = self.next()?;
        if DEBUG {
            println!("peeked {:?}", self.peek);
        }

        Ok(self.peek.clone())
    }

    fn skip_spaces(&mut self) {
        while {
            self.consume();
            !self.eof && self.buffer[0].1 == ' '
        } {}
    }

    fn skip_comment(&mut self) {
        while {
            self.consume();
            !self.eof && self.buffer[0].1 != '\n'
        } {}
    }

    fn collect_indentation(&mut self) -> Result<Option<TokenSpan>, LexerError> {
        let start = self.buffer[0].0;
        let mut count = 0u8;
        while !self.eof && self.buffer[0].1 == ' ' {
            self.consume();
            count += 1;
        }
        if self.buffer[0].1 == '\n' || self.buffer[0].1 == '\r' {
            // no reason to generate indentation tokens for otherwise empty lines
            self.collect_newlines()
        } else {
            Ok(if self.is_eof() {
                None // no point in generating indentation for the last otherwise empty line
            } else {
                Some(TokenSpan::new(
                    start,
                    self.buffer[0].0 - 1,
                    Token::Indentation(count),
                ))
            })
        }
    }

    fn collect_newlines(&mut self) -> Result<Option<TokenSpan>, LexerError> {
        let start = self.buffer[0].0;
        let mut count = 0u8;
        while !self.eof {
            match self.buffer {
                [(_, '\n'), _] => {
                    count += 1;
                }
                [(_, '\r'), (_, '\n')] => {
                    count += 1;
                    self.consume(); // consume '\r'
                }
                [(end, '\r'), _] => {
                    self.consume();
                    return Err(error::lexer(
                    "'\\r' is not allowed in code unless followed by '\\n', alone they are only allowed in strings".into(),
                        Span::new(start, end)
                    ));
                }
                _ => break,
            }
            self.consume();
        }

        if self.buffer[0].1 != ' ' {
            Ok(Some(TokenSpan::new(
                start,
                self.buffer[0].0 - 1,
                Token::Newline(count),
            )))
        } else {
            let newline_token = Ok(Some(TokenSpan::new(
                start,
                self.buffer[0].0 - 1,
                Token::Newline(count),
            )));
            let mut tok = None;
            while self.buffer[0].1 == ' ' {
                tok = Some(self.collect_indentation()?);
            }

            if let Some(
                tok @ TokenSpan {
                    token: Token::Indentation(_),
                    ..
                },
            ) = tok.flatten()
            {
                self.queue.push_back(tok);
            }

            newline_token
        }
    }

    fn perform_full_match(&mut self) -> Result<Option<TokenSpan>, LexerError> {
        if self.eof {
            return Ok(None);
        }

        if !self.buffer[0].1.is_whitespace() {
            return self.perform_match();
        }

        match self.buffer {
            [(_, ' '), _] => {
                if self.newline {
                    self.collect_indentation()
                } else {
                    self.skip_spaces();
                    self.perform_full_match()
                }
            }
            [(_, '\r' | '\n'), _] => self.collect_newlines(),
            x => {
                return Err(error::lexer(
                    format!("invalid whitespace {:?}", x[0].1),
                    Span::new(x[0].0, x[0].0),
                ))
            }
        }
    }

    fn perform_match(&mut self) -> Result<Option<TokenSpan>, LexerError> {
        match self.buffer {
            [(_, ';'), (_, ';')] => {
                self.skip_comment();
                self.perform_full_match()
            }
            [(start, '('), (end, ')')] => {
                self.consume();
                self.consume();
                Ok(Some(TokenSpan::new(start, end, Token::Unit)))
            }
            [(start, '('), _] => {
                self.consume();
                Ok(Some(TokenSpan::new(start, start, Token::LeftParenthesis)))
            }
            [(start, ')'), _] => {
                self.consume();
                Ok(Some(TokenSpan::new(start, start, Token::RightParenthesis)))
            }
            //'[' => Ok(Some(Token::LeftBracket)),
            //']' => Ok(Some(Token::RightBracket)),
            //'{' => Ok(Some(Token::LeftBrace)),
            //'}' => Ok(Some(Token::RightBrace)),
            //':' => Ok(Some(Token::Colon)),
            //',' => Ok(Some(Token::Comma)),
            //'@' => Ok(Some(Token::At)),
            [(start, '.'), _] => {
                self.consume();
                Ok(Some(TokenSpan::new(start, start, Token::Dot)))
            }
            [(start, '='), (_, ' ' | '\r' | '\n' | '\t' | '\0')] => {
                self.consume();
                Ok(Some(TokenSpan::new(start, start, Token::Equals)))
            }
            [(_, 'a'..='z' | 'A'..='Z' | '_'), _] => self.get_identifier(),
            [(_, '"'), _] => self.get_string(),
            [(_, '`'), _] => self.get_operator_literal(),
            [(_, '0'..='9'), _] => self.get_number(),
            [(_, '-'), (_, '0'..='9')] => self.get_number(),
            [(_, '\0'), _] => {
                self.consume();
                self.perform_full_match()
            }
            _ => self.get_operator(),
            //_ => Err(format!("Unknown char {}", now)),
        }
    }

    fn get_operator(&mut self) -> Result<Option<TokenSpan>, LexerError> {
        let start = self.buffer[0].0;
        let mut end = self.buffer[0].0;
        let mut t_str = String::with_capacity(10);
        t_str.push(self.buffer[0].1);
        while {
            self.consume();
            !self.eof
        } {
            match self.buffer {
                [(
                    _,
                    '('
                    | ')'
                    | '"'
                    | '`'
                    | '-'
                    | 'a'..='z'
                    | 'A'..='Z'
                    | '0'..='9'
                    | '_'
                    | ' '
                    | '\n'
                    | '\t'
                    | '\r'
                    | '\\'
                    | '\0',
                ), _] => {
                    // char is not allowed in an operator, has to be the end of the operator
                    t_str.shrink_to_fit();
                    break;
                }
                [(c_start, c), _] => {
                    t_str.push(c);
                    end = c_start;
                }
            }
        }

        self.operators
            .get(&t_str)
            .ok_or_else(|| {
                error::lexer(
                    format!("Unknown operator '{}'", t_str),
                    Span::new(start, end),
                )
            })
            .map(|_| Some(TokenSpan::new(start, end, Token::Operator(t_str))))
    }

    fn get_identifier(&mut self) -> Result<Option<TokenSpan>, LexerError> {
        let start = self.buffer[0].0;
        let mut end = self.buffer[0].0;
        let mut t_str = String::with_capacity(10);
        t_str.push(self.buffer[0].1);
        while {
            self.consume();
            !self.eof
        } {
            match self.buffer {
                [(c_start, c @ ('a'..='z' | 'A'..='Z' | '0'..='9' | '_')), _] => {
                    end = c_start;
                    t_str.push(c);
                }
                _ => {
                    t_str.shrink_to_fit();
                    break;
                }
            }
        }

        match t_str.as_str() {
            "let" => Ok(Some(TokenSpan::new(start, end, Token::Let))),
            "true" => Ok(Some(TokenSpan::new(start, end, Token::True))),
            "false" => Ok(Some(TokenSpan::new(start, end, Token::False))),
            _ => Ok(Some(TokenSpan::new(start, end, Token::Identifier(t_str)))),
        }
    }

    fn get_number(&mut self) -> Result<Option<TokenSpan>, LexerError> {
        enum LexNumState {
            Integer,
            HasDecimalPoint,
            HasDecimals,
        }
        let start = self.buffer[0].0;
        let mut end = self.buffer[0].0;
        let mut t_str = String::with_capacity(10);
        let mut num_flags_new = LexNumState::Integer;
        t_str.push(self.buffer[0].1);
        while {
            self.consume();
            !self.eof
        } {
            match self.buffer {
                [(c_start, c @ ('0'..='9' | '_')), _] => {
                    if matches!(num_flags_new, LexNumState::HasDecimalPoint) {
                        num_flags_new = LexNumState::HasDecimals;
                        t_str.push('.');
                    }
                    t_str.push(c);
                    end = c_start;
                }
                // TODO: re-write to match decimals with:
                // TODO: ['.', '0'..='9' | '_'] and an extra consume -> double
                // TODO: ['.', _] -> integer + dot
                [(c_start, '.'), _] => match num_flags_new {
                    LexNumState::Integer => {
                        num_flags_new = LexNumState::HasDecimalPoint;
                    }
                    LexNumState::HasDecimalPoint => {
                        self.queue
                            .push_back(TokenSpan::new(c_start, c_start, Token::Dot));
                        self.consume();
                        break;
                    }
                    LexNumState::HasDecimals => {
                        self.consume();
                        self.queue
                            .push_back(TokenSpan::new(c_start, c_start, Token::Dot));
                        break;
                    }
                },
                _ => {
                    break;
                }
            }
        }

        t_str.shrink_to_fit();
        match num_flags_new {
            LexNumState::Integer => Ok(Some(TokenSpan::new(
                start,
                end,
                Token::Integer(t_str.parse().unwrap()),
            ))),
            LexNumState::HasDecimalPoint => {
                self.queue
                    .push_back(TokenSpan::new(end + 1, end + 1, Token::Dot));
                Ok(Some(TokenSpan::new(
                    start,
                    end,
                    Token::Integer(t_str.parse().unwrap()),
                )))
            }
            LexNumState::HasDecimals => Ok(Some(TokenSpan::new(
                start,
                end,
                Token::Double(t_str.parse().unwrap()),
            ))),
        }
    }

    fn get_operator_literal(&mut self) -> Result<Option<TokenSpan>, LexerError> {
        let start = self.buffer[0].0;
        let mut t_op_lit = String::with_capacity(3);
        while {
            self.consume();
            !self.eof
        } {
            match self.buffer {
                [(
                    c_start,
                    c @ ('('
                    | ')'
                    | '"'
                    | 'a'..='z'
                    | 'A'..='Z'
                    | '0'..='9'
                    | ';'
                    | '_'
                    | ' '
                    | '\n'
                    | '\t'
                    | '\r'
                    | '\\'
                    | '\0'),
                ), _] => {
                    // allowing '`' in operator literals for example would be a bad idea
                    return Err(error::lexer(format!(
                        "Char '{}' not allowed in operator literal: '{}', chars that are not allowed: ()\\\"\\0_; a..=z A..=Z 0..=9",
                        c, t_op_lit
                    ), Span::new(start, c_start)));
                }
                [(c_start, '`'), _] => {
                    t_op_lit.shrink_to_fit();
                    self.consume();
                    self.operators.insert(t_op_lit.clone());
                    return Ok(Some(TokenSpan::new(
                        start,
                        c_start,
                        Token::OperatorLiteral(t_op_lit),
                    )));
                }
                [(_, c), _] => {
                    t_op_lit.push(c);
                }
            }
        }
        Err(error::lexer(
            format!("Unterminated operator literal: {}", t_op_lit),
            Span::new(start, self.buffer[0].0),
        ))
    }

    fn get_string(&mut self) -> Result<Option<TokenSpan>, LexerError> {
        let start = self.buffer[0].0;
        let mut t_str = String::with_capacity(10);
        while {
            self.consume();
            !self.eof
        } {
            match self.buffer {
                [(_, '\\'), (_, 't')] => {
                    t_str.push('\t');
                }
                [(_, '\\'), (_, 'n')] => {
                    t_str.push('\n');
                }
                [(_, '\\'), (_, '\\')] => {
                    t_str.push('\\');
                }
                [(_, '\\'), (_, '"')] => {
                    t_str.push('"');
                }
                [(c_start, '\\'), _] => {
                    return Err(error::lexer(
                        format!("Unescaped string: {}", t_str),
                        Span::new(start, c_start),
                    ))
                }
                [(c_start, '"'), _] => {
                    t_str.shrink_to_fit();
                    self.consume();
                    return Ok(Some(TokenSpan::new(start, c_start, Token::String(t_str))));
                }
                [(_, c), _] => {
                    t_str.push(c);
                }
            }
        }
        Err(error::lexer(
            format!("Unescaped string: {}", t_str),
            Span::new(start, self.buffer[0].0),
        ))
    }

    fn consume(&mut self) {
        if self.buffer[0].1 == '\n' {
            self.newline = true;
        } else if self.buffer[0].1 != ' ' {
            self.newline = false;
        }
        self.buffer[0] = self.buffer[1];

        match self.chars.next() {
            Some(c) => {
                self.buffer[1] = (self.pos, c);
                self.pos += 1;
            }
            None => {
                if self.buffer[0].1 == '\0' {
                    // buffer has been fully consumed
                    self.eof = true;
                } else {
                    self.buffer[1] = (self.pos, '\0');
                    self.pos += 1;
                }
            }
        }
    }

    fn fill_buffer(&mut self) {
        for _ in 0..self.buffer.len() {
            self.consume();
        }
        self.newline = true;
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::*;
    use crate::token::*;
    macro_rules! assert_matches {
        ($actual:expr, $expected:pat) => {
            let val = $actual.token;
            match val {
                $expected => {}
                _ => {
                    assert!(false, "expected {:?}, was {:?}", stringify!($expected), val);
                }
            };
        };
        ($actual:expr, $expected:path = $s:expr) => {
            let val = $actual.token;
            match val {
                $expected(ref a) => {
                    assert_eq!($s, a, "expected {}({:?})", stringify!($expected), $s)
                }
                _ => {
                    assert!(
                        false,
                        "expected {}({:?}), was {:?}",
                        stringify!($expected),
                        $s,
                        val
                    );
                }
            };
        };
    }

    #[test]
    fn numbers() {
        let str = "137517395..138993 38.51 -75 -85.15 73.53..98.73";
        let mut lexer = Lexer::new(str.chars());
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Integer(137517395));
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Dot);
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Dot);
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Integer(138993));
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Double = &38.51);
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Integer(-75));
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Double = &-85.15);
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Double = &73.53);
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Dot);
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Dot);
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Double = &98.73);
        assert!(
            lexer.next().unwrap().is_none(),
            "lexer should have reached end of string"
        );
    }

    #[test]
    fn indentation_on_empty_lines_should_not_generate_indentation_tokens() {
        let str = "x\n    \n  \n     \n    y";
        let mut lexer = Lexer::new(str.chars());
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Identifier = "x");
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Newline(1));
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Indentation(4));
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Identifier = "y");
    }

    #[test]
    fn let_assignment() {
        let str = "let name =";
        let mut lexer = Lexer::new(str.chars());
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Let);
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Identifier = "name");
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Equals);
    }

    #[test]
    fn let_function() {
        let str = "let name arg =";
        let mut lexer = Lexer::new(str.chars());
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Let);
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Identifier = "name");
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Identifier = "arg");
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Equals);
    }

    #[test]
    fn let_operator_literal() {
        let str = "let x `+` y =";
        let mut lexer = Lexer::new(str.chars());
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Let);
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Identifier = "x");
        assert_matches!(lexer.next().unwrap().unwrap(), Token::OperatorLiteral = "+");
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Identifier = "y");
        assert_matches!(lexer.next().unwrap().unwrap(), Token::Equals);
    }

    mod newline_tests {
        use super::*;
        macro_rules! newline_tests {
            ($($name:ident => $value:expr => $expect_count:expr$(,)?)*) => {
                $(
                    #[test]
                    fn $name() {
                        let str = $value;
                        let mut lexer = Lexer::new(str.chars());
                        while let Ok(Some(now)) = lexer.next() {
                            match now.token {
                                Token::Newline(x) =>{
                                    assert_eq!($expect_count, x);
                                    return;
                                },
                                _ => {},
                            }
                        }
                        assert!(false, "lexer generated 0 newlines")
                    }
                )*
            };
        }

        newline_tests! {
            one => "\r\n" => 1,
            three => "\r\n\r\n\r\n" => 3,
            space_between => "\r\n \r\n" => 1,
            start_space => " \r\n" => 1,
            end_space => "\r\n " => 1,
            end_three_spaces => "\r\n   " => 1,
            short_one => "\n" => 1,
            short_three => "\n\n\n" => 3,
            short_space_between => "\n \n" => 1,
            short_start_space => " \n" => 1,
            short_end_space => "\n " => 1,
            short_tab_between => "\n\t\n" => 1,
        }

        #[test]
        #[should_panic]
        fn tab_between() {
            let str = "\r\n\r\t\n";
            let mut lexer = Lexer::new(str.chars());
            assert_matches!(lexer.next().unwrap().unwrap(), Token::Newline(1));
            assert!(
                lexer.next().is_err(),
                "expected single '\\r' to cause an error"
            );
            assert_matches!(lexer.next().unwrap().unwrap(), Token::Newline(1));
        }
    }

    mod indentation_tests {
        use super::*;
        macro_rules! indentation_tests {
            ($($name:ident => $value:expr => $expect_count:expr$(,)?)*) => {
                $(
                    #[test]
                    fn $name() {
                        let str = $value;
                        let mut lexer = Lexer::new(str.chars());
                        while let Ok(Some(now)) = lexer.next() {
                            match now.token {
                                Token::Indentation(x) =>{
                                    if $expect_count > 0 {
                                        assert_eq!($expect_count, x);
                                    } else {
                                        assert!(false, "expected no indentation token");
                                    }
                                    return;
                                },
                                _ => {},
                            }
                        }
                        if $expect_count > 0 {
                            assert!(false, "lexer generated no indentation")
                        }
                    }
                )*
            };
        }

        indentation_tests! {
            one => " f" => 1,
            four => "    e" => 4,
            newline_between => " \r\n d" => 1,
            start_newline => "\r\n c" => 1,
            end_newline => "  x\r\n" => 2,
            space_between_newlines_not_indentation => "\r\n  \r\n" => 0,
            end_three_spaces_before_ident => "\r\n   o" => 3,
            end_three_spaces => "\r\n   " => 0,
            tab_between => " \t " => 1,
            short_newline_between => " \n a" => 1,
            short_start_newline => "\n b" => 1,
            short_end_newline => " x\n" => 1,
            short_space_between_newlines_not_indentation => "\n  \n" => 0,
            trailing_space_not_indentation => "test   " => 0,
            space_between_tokens_not_indentation => "test x y" => 0,
        }
    }
}
