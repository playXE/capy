#![allow(unused_variables)]
use std::{borrow::Cow, cell::Cell};

use r7rs_parser::{
    expr::NoIntern,
    lexer::lexical_error::LexicalError,
    parser::{syntax_error::SyntaxError, ParseError, Parser},
};

use rsgc::thread::Thread;
use rustyline::{
    completion::*,
    config::Configurer,
    error::ReadlineError,
    highlight::Highlighter,
    hint::Hinter,
    history::DefaultHistory,
    validate::{ValidationResult, Validator},
    CompletionType, Helper,
};

use crate::{
    compile::{
        bytecompiler::ByteCompiler, make_cenv, pass1::pass1, r7rs_to_value, ref_count_lvars,
    },
    runtime::module::{scm_search_for_symbols, scm_user_module},
    runtime::string::make_string,
    runtime::value::Value,
    vm::{interpreter::apply, scm_current_module, scm_set_current_module},
};

use r7rs_parser::lexer::scanner::is_subsequent;
#[derive(Default)]
pub struct SchemeHelper {
    highlighter: MatchingBracketHighlighter,
}

impl Completer for SchemeHelper {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        if line.len() == 0 {
            return Ok((0, vec![]));
        }

        let end = pos;
        let mut start = pos;

        while start > 0 {
            let c = line.chars().nth(start - 1).unwrap();
            if !is_subsequent(c) {
                break;
            }
            start -= 1;
        }

        let word = &line[start..end];

        let x = scm_search_for_symbols(scm_current_module().unwrap(), word);

        Ok((start, x))
    }
}

impl Validator for SchemeHelper {
    fn validate(
        &self,
        ctx: &mut rustyline::validate::ValidationContext,
    ) -> rustyline::Result<rustyline::validate::ValidationResult> {
        let str = ctx.input();

        let mut i = NoIntern;
        let mut parser = Parser::new(&mut i, str, false);

        while !parser.finished() {
            if let Err(err) = parser.parse(true) {
                match err {
                    ParseError::Syntax(_, err) => match err {
                        SyntaxError::ClosingParenthesisMissing => {
                            return Ok(ValidationResult::Incomplete);
                        }
                        SyntaxError::Empty => {
                            return Ok(ValidationResult::Incomplete);
                        }

                        SyntaxError::NotAByteValue => {
                            return Ok(ValidationResult::Invalid(Some(
                                "Not a byte value".to_string(),
                            )));
                        }

                        SyntaxError::SyntaxNotYetSupported => {
                            return Ok(ValidationResult::Invalid(Some(
                                "Syntax not yet supported".to_string(),
                            )));
                        }

                        SyntaxError::UnexpectedDot => {
                            return Ok(ValidationResult::Invalid(Some(
                                "Unexpected dot".to_string(),
                            )));
                        }

                        SyntaxError::UnexpectedClosingParenthesis => {
                            return Ok(ValidationResult::Invalid(Some(
                                "Unexpected closing parenthesis".to_string(),
                            )));
                        }
                    },

                    ParseError::Lexical(_, err) => match err {
                        LexicalError::IncompleteCharacterLiteral => {
                            return Ok(ValidationResult::Incomplete);
                        }

                        _ => {
                            return Ok(ValidationResult::Invalid(Some(err.to_string())));
                        }
                    },
                }
            }
        }

        Ok(ValidationResult::Valid(None))
    }
}

/// Highlight matching bracket when typed or cursor moved on.
#[derive(Default)]
pub struct MatchingBracketHighlighter {
    bracket: Cell<Option<(u8, usize)>>, // memorize the character to search...
}

impl MatchingBracketHighlighter {
    /// Constructor
    #[must_use]
    pub fn new() -> Self {
        Self {
            bracket: Cell::new(None),
        }
    }
}

impl Highlighter for MatchingBracketHighlighter {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        if line.len() <= 1 {
            return Cow::Borrowed(line);
        }
        // highlight matching brace/bracket/parenthesis if it exists
        if let Some((bracket, pos)) = self.bracket.get() {
            if let Some((matching, idx)) = find_matching_bracket(line, pos, bracket) {
                let mut copy = line.to_owned();
                copy.replace_range(idx..=idx, &format!("\x1b[1;34m{}\x1b[0m", matching as char));
                return Cow::Owned(copy);
            }
        }

        Cow::Borrowed(line)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        // will highlight matching brace/bracket/parenthesis if it exists
        self.bracket.set(check_bracket(line, pos));
        self.bracket.get().is_some()
    }
}

fn find_matching_bracket(line: &str, pos: usize, bracket: u8) -> Option<(u8, usize)> {
    let matching = matching_bracket(bracket);
    let mut idx;
    let mut unmatched = 1;
    if is_open_bracket(bracket) {
        // forward search
        idx = pos + 1;
        let bytes = &line.as_bytes()[idx..];
        for b in bytes {
            if *b == matching {
                unmatched -= 1;
                if unmatched == 0 {
                    debug_assert_eq!(matching, line.as_bytes()[idx]);
                    return Some((matching, idx));
                }
            } else if *b == bracket {
                unmatched += 1;
            }
            idx += 1;
        }
        debug_assert_eq!(idx, line.len());
    } else {
        // backward search
        idx = pos;
        let bytes = &line.as_bytes()[..idx];
        for b in bytes.iter().rev() {
            if *b == matching {
                unmatched -= 1;
                if unmatched == 0 {
                    debug_assert_eq!(matching, line.as_bytes()[idx - 1]);
                    return Some((matching, idx - 1));
                }
            } else if *b == bracket {
                unmatched += 1;
            }
            idx -= 1;
        }
        debug_assert_eq!(idx, 0);
    }
    None
}

// check under or before the cursor
fn check_bracket(line: &str, pos: usize) -> Option<(u8, usize)> {
    if line.is_empty() {
        return None;
    }
    let mut pos = pos;
    if pos >= line.len() {
        pos = line.len() - 1; // before cursor
        let b = line.as_bytes()[pos]; // previous byte
        if is_close_bracket(b) {
            Some((b, pos))
        } else {
            None
        }
    } else {
        let mut under_cursor = true;
        loop {
            let b = line.as_bytes()[pos];
            if is_close_bracket(b) {
                return if pos == 0 { None } else { Some((b, pos)) };
            } else if is_open_bracket(b) {
                return if pos + 1 == line.len() {
                    None
                } else {
                    Some((b, pos))
                };
            } else if under_cursor && pos > 0 {
                under_cursor = false;
                pos -= 1; // or before cursor
            } else {
                return None;
            }
        }
    }
}

const fn matching_bracket(bracket: u8) -> u8 {
    match bracket {
        b'{' => b'}',
        b'}' => b'{',
        b'[' => b']',
        b']' => b'[',
        b'(' => b')',
        b')' => b'(',
        b => b,
    }
}
const fn is_open_bracket(bracket: u8) -> bool {
    matches!(bracket, b'{' | b'[' | b'(')
}
const fn is_close_bracket(bracket: u8) -> bool {
    matches!(bracket, b'}' | b']' | b')')
}

impl Hinter for SchemeHelper {
    type Hint = String;
    fn hint(&self, line: &str, pos: usize, ctx: &rustyline::Context<'_>) -> Option<Self::Hint> {
        None
    }
}

impl Highlighter for SchemeHelper {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> std::borrow::Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }
}

impl Helper for SchemeHelper {}

pub fn repl() {
    let mut rl = rustyline::Editor::<SchemeHelper, DefaultHistory>::new().unwrap();

    rl.set_helper(Some(SchemeHelper {
        highlighter: MatchingBracketHighlighter::new(),
    }));
    rl.set_completion_type(CompletionType::List);
    rl.set_completion_prompt_limit(10);
    scm_set_current_module(Some(scm_user_module().module()));

    let m = scm_user_module();

    let mut interner = NoIntern;
    let t = Thread::current();
    let cenv = make_cenv(scm_user_module().module(), Value::encode_null_value());
    loop {
        match rl.readline("capy> ") {
            Ok(input) => {
                rl.add_history_entry(input.clone()).unwrap();
                let mut parser = Parser::new(&mut interner, &input, false);
                let proc = ByteCompiler::compile_while(t, |t| {
                    if parser.finished() {
                        return Ok(None);
                    }

                    let expr = parser.parse(true);

                    match expr {
                        Ok(expr) => {
                            let expr = r7rs_to_value(t, &expr);
                            let iform = pass1(
                                expr,
                                make_cenv(scm_user_module().module(), Value::encode_null_value()),
                            )?;
                            ref_count_lvars(iform);
                            Ok(Some(iform))
                        }

                        Err(e) => return Err(make_string(t, &e.to_string()).into()),
                    }
                });

                match proc {
                    Ok(proc) => match apply(proc, &[]) {
                        Ok(v) => {
                            println!("{:?}", v);
                        }
                        Err(e) => {
                            println!("Error: {:?}", e);
                        }
                    },
                    Err(err) => {
                        println!("Error: {:?}", err);
                    }
                }
            }

            Err(ReadlineError::Interrupted) => {
                println!("Input interrupted");
            }

            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }

            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
