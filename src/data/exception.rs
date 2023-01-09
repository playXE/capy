use std::collections::HashSet;

use r7rs_parser::{
    lexer::{lexical_error::LexicalError, scanner::Position},
    parser::syntax_error::SyntaxError,
};
use rsgc::{
    prelude::{Allocation, Object},
    system::{array::Array, object::Handle, string::Str},
};

use crate::{
    prelude::{eval_error::EvalError, Context, Procedure, Type, Value},
    utilities::arraylist::ArrayList,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SourcePosition {
    pub source_id: u32,
    pub position: Position,
}

impl SourcePosition {
    pub fn unknown() -> SourcePosition {
        SourcePosition {
            source_id: u32::MAX,
            position: Position::new(u32::MAX, u32::MAX),
        }
    }

    pub fn is_unknown(&self) -> bool {
        self.source_id == u32::MAX
    }
}

pub struct Exception {
    pub pos: SourcePosition,
    pub irritants: Handle<Array<Value>>,
    pub descriptor: ExceptionDescriptor,
    pub stacktrace: Option<ArrayList<Handle<Procedure>>>,
    pub calltrace: Option<ArrayList<Handle<Str>>>,
}

unsafe impl Send for Exception {}
unsafe impl Sync for Exception {}

impl Object for Exception {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        self.irritants.trace(visitor);
        self.stacktrace.trace(visitor);
        self.calltrace.trace(visitor);
        self.descriptor.trace(visitor);
    }
}

impl Allocation for Exception {}

impl Exception {
    pub(crate) fn attach_ctx(&mut self, ctx: &mut Context, current: Option<Handle<Procedure>>) {
        if self.stacktrace.is_none() {
            self.stacktrace = Some(ctx.get_stack_trace(current));
        }

        if self.calltrace.is_none() {
            //self.calltrace = ctx.get_call_trace(current, None);
        }
    }

    pub fn type_error(
        ctx: &mut Context,
        expected: &[Type],
        got: Value,
        pos: SourcePosition,
    ) -> Handle<Exception> {
        let mut irritants = Array::new(ctx.mutator(), 1, |_, _| Value::nil());
        irritants[0] = got;
        let mut aexpected = ArrayList::with_capacity(ctx.mutator(), expected.len());
        for e in expected.iter().copied() {
            aexpected.push(ctx.mutator(), e);
        }

        let this = ctx.mutator().allocate(Exception {
            pos,
            irritants,
            calltrace: None,
            stacktrace: None,
            descriptor: ExceptionDescriptor::Type(got.typ(), aexpected),
        });

        this
    }

    pub fn lexical(
        ctx: &mut Context,
        err: LexicalError,
        irritants: &[Value],
        pos: SourcePosition,
    ) -> Handle<Exception> {
        let irritants = Array::new(ctx.mutator(), irritants.len(), |_, ix| irritants[ix]);
        let this = ctx.mutator().allocate(Exception {
            pos,
            irritants,
            calltrace: None,
            stacktrace: None,
            descriptor: ExceptionDescriptor::Lexical(err),
        });

        this
    }

    pub fn syntax(
        ctx: &mut Context,
        err: SyntaxError,
        irritants: &[Value],
        pos: SourcePosition,
    ) -> Handle<Exception> {
        let irritants = Array::new(ctx.mutator(), irritants.len(), |_, ix| irritants[ix]);
        let this = ctx.mutator().allocate(Exception {
            pos,
            irritants,
            calltrace: None,
            stacktrace: None,
            descriptor: ExceptionDescriptor::Syntax(err),
        });

        this
    }

    pub fn eval(
        ctx: &mut Context,
        err: EvalError,
        irritants: &[Value],
        pos: SourcePosition,
    ) -> Handle<Exception> {
        let irritants = Array::new(ctx.mutator(), irritants.len(), |_, ix| irritants[ix]);
        let this = ctx.mutator().allocate(Exception {
            pos,
            irritants,
            calltrace: None,
            stacktrace: None,
            descriptor: ExceptionDescriptor::Eval(err),
        });

        this
    }

    pub fn custom(
        ctx: &mut Context,
        kind: &str,
        template: &str,
        irritants: &[Value],
        pos: SourcePosition,
    ) -> Handle<Exception> {
        let irritants = Array::new(ctx.mutator(), irritants.len(), |_, ix| irritants[ix]);
        let kind = Str::new(ctx.mutator(), kind);
        let template = Str::new(ctx.mutator(), template);
        let this = ctx.mutator().allocate(Exception {
            pos,
            irritants,
            calltrace: None,
            stacktrace: None,
            descriptor: ExceptionDescriptor::Custom(kind, template),
        });

        this
    }

    pub fn argument_count(
        ctx: &mut Context,
        of: Option<&str>,
        min: usize,
        max: usize,
        args: Value,
        pos: SourcePosition,
    ) -> Handle<Exception> {
        let mut irritants = Array::new(ctx.mutator(), 2, |_, _| Value::nil());
        irritants[0] = Value::new(args.length() as i32);
        irritants[1] = args;
        let name = of.map(|s| Str::new(ctx.mutator(), s));
        let this = ctx.mutator().allocate(Exception {
            pos,
            irritants,
            descriptor: ExceptionDescriptor::ArgumentCount(
                name,
                if min == usize::MAX {
                    i32::MAX
                } else {
                    min as _
                },
                if max == usize::MAX {
                    i32::MAX
                } else {
                    max as _
                },
            ),
            calltrace: None,
            stacktrace: None,
        });

        this
    }

    pub fn message(&self) -> String {
        Self::replace_placeholders(
            &self.descriptor.message_template(),
            &self.irritants,
            &mut Default::default(),
        )
    }

    pub fn inline_description(&self) -> String {
        let msg = self.message();

        format!("{}: {}", self.descriptor.type_description(), msg)
    }

    fn replace_placeholders(template: &str, values: &[Value], used: &mut HashSet<i32>) -> String {
        let mut res = String::new();
        let mut variable = String::new();
        let mut parsing_variable = false;
        let mut embed_variable = false;

        for ch in template.chars() {
            if parsing_variable {
                match ch {
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                        variable.push(ch);
                        continue;
                    }
                    '~' => {
                        if variable.is_empty() {
                            if embed_variable {
                                res.push_str("$,~");
                            } else {
                                res.push(ch);
                            }
                            parsing_variable = false;
                            embed_variable = false;
                            continue;
                        }
                    }

                    '$' => {
                        if variable.is_empty() {
                            if embed_variable {
                                res.push_str("$,$");
                            } else {
                                res.push(ch);
                            }
                            parsing_variable = false;
                            embed_variable = false;
                            continue;
                        }
                    }
                    ',' => {
                        if variable.is_empty() {
                            if embed_variable {
                                res.push_str("$,,");
                                parsing_variable = false;
                                embed_variable = false;
                            } else {
                                embed_variable = true;
                            }
                            continue;
                        }
                    }
                    _ => {
                        if variable.is_empty() {
                            res.push('$');
                            if embed_variable {
                                res.push_str(",");
                            }
                            res.push(ch);
                            parsing_variable = false;
                            embed_variable = false;
                            continue;
                        }
                    }
                }

                let varnum = variable.parse::<i32>();

                match varnum {
                    Ok(varnum) if varnum >= 0 && varnum < values.len() as i32 => {
                        if embed_variable {
                            res.push_str(&values[varnum as usize].to_string(false))
                        } else {
                            res.push_str(&values[varnum as usize].to_string(true))
                        }

                        used.insert(varnum);

                        variable.clear();
                        parsing_variable = false;
                        embed_variable = false;
                    }
                    _ => {
                        res.push('$');
                        if embed_variable {
                            res.push(',');
                        }

                        res.push_str(&variable);
                        variable.clear();
                        parsing_variable = false;
                        embed_variable = false;
                        if ch == '~' {
                            res.push('~');
                            continue;
                        }
                    }
                }

                if ch == '$' {
                    parsing_variable = true;
                } else if ch != '~' {
                    res.push(ch);
                }
            } else if ch == '$' {
                parsing_variable = true;
            } else {
                res.push(ch);
            }
        }

        if parsing_variable {
            let varnum = variable.parse::<i32>().ok();

            if let Some(varnum) = varnum.filter(|&x| x >= 0 && x < values.len() as i32) {
                let var = values[varnum as usize];

                if embed_variable {
                    res.push_str(&var.to_string(false));
                } else {
                    res.push_str(&var.to_string(true));
                }

                used.insert(varnum as _);
            } else {
                res.push('$');
                if embed_variable {
                    res.push(',');
                }

                res.push_str(&variable);
            }
        }

        res
    }
}

pub enum ExceptionDescriptor {
    Lexical(LexicalError),
    Syntax(SyntaxError),
    Range(Option<Handle<Str>>, Option<isize>, i32, i32),
    ArgumentCount(Option<Handle<Str>>, i32, i32),
    Type(Type, ArrayList<Type>),
    Eval(EvalError),
    OS(Handle<Str>),
    Abortion,
    Uncaught,
    Custom(Handle<Str>, Handle<Str>),
}

impl Object for ExceptionDescriptor {
    fn trace(&self, visitor: &mut dyn rsgc::prelude::Visitor) {
        match self {
            ExceptionDescriptor::Range(file, _, _, _) => {
                if let Some(file) = file {
                    file.trace(visitor);
                }
            }
            ExceptionDescriptor::ArgumentCount(file, _, _) => {
                if let Some(file) = file {
                    file.trace(visitor);
                }
            }
            ExceptionDescriptor::Type(_, types) => types.trace(visitor),
            ExceptionDescriptor::Custom(kind, template) => {
                kind.trace(visitor);
                template.trace(visitor);
            }
            ExceptionDescriptor::OS(msg) => msg.trace(visitor),
            _ => {}
        }
    }
}

impl ExceptionDescriptor {
    pub fn is_file_error(&self) -> bool {
        match self {
            ExceptionDescriptor::Eval(err) => match err {
                EvalError::CannotOpenFile
                | EvalError::CannotOpenAsset
                | EvalError::CannotWriteToPort
                | EvalError::UnknownFile => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_read_error(&self) -> bool {
        match self {
            ExceptionDescriptor::Syntax(_) | ExceptionDescriptor::Lexical(_) => true,
            _ => false,
        }
    }

    pub fn type_description(&self) -> String {
        match self {
            ExceptionDescriptor::Lexical(_) => "lexical error",
            ExceptionDescriptor::Syntax(_) => "syntax error",
            ExceptionDescriptor::Range(_, _, _, _) => "range error",
            ExceptionDescriptor::ArgumentCount(_, _, _) => "argument count error",
            ExceptionDescriptor::Eval(_) => "evaluation error",
            ExceptionDescriptor::OS(_) => "operating system error",
            ExceptionDescriptor::Abortion => "abortion",
            ExceptionDescriptor::Uncaught => "uncaught exception",
            ExceptionDescriptor::Custom(typ, _) => &typ,
            ExceptionDescriptor::Type(_, _) => "type error",
        }
        .to_string()
    }

    pub fn short_type_description(&self) -> String {
        match self {
            ExceptionDescriptor::Lexical(_) => "lexical",
            ExceptionDescriptor::Syntax(_) => "syntax",
            ExceptionDescriptor::Range(_, _, _, _) => "range",
            ExceptionDescriptor::ArgumentCount(_, _, _) => "argument count",
            ExceptionDescriptor::Eval(_) => "evaluation",
            ExceptionDescriptor::OS(_) => "operating system",
            ExceptionDescriptor::Abortion => "abortion",
            ExceptionDescriptor::Uncaught => "uncaught",
            ExceptionDescriptor::Custom(_, _) => "custom",
            ExceptionDescriptor::Type(_, _) => "type",
        }
        .to_string()
    }

    pub fn message_template(&self) -> String {
        match self {
            ExceptionDescriptor::Lexical(error) => error.to_string(),
            ExceptionDescriptor::Syntax(error) => error.to_string(),
            ExceptionDescriptor::Type(found, expected) => {
                if expected.len() == 0 {
                    return "unexpected expression $0".to_string();
                }

                let mut tpe: Option<&Type> = None;

                let mut res = String::new();

                for typ in expected.iter() {
                    if let Some(t) = tpe {
                        res.push_str(
                            &format!("{}{}", if res.is_empty() {
                                ""
                            } else {
                                ", "
                            }, t)
                        );
                    }

                    tpe = Some(typ);
                }

                if res.is_empty() {
                    res = format!("a {}", tpe.unwrap());
                } else {
                    res = format!("either a {} or {}", res, tpe.unwrap())
                }

                format!("$0 is of type {}, but is required to be {} value", found, res)
            }

            ExceptionDescriptor::Range(fun, par, low, high) => {
                let low = *low;
                let high = *high;
                if let (Some(fun), Some(par)) = (fun, par) {
                    if low == i32::MIN {
                        if high == 0 {
                            format!("expected argument {} of function {} to be a negative integer value; is $0 instead", par, fun)
                        } else {
                            format!("expected argument {} of function {} to be a negative integer value in the range {}..=0; is $0 instead", par, fun, high)
                        }
                    } else if low == i32::MAX {
                        if high == 0 {
                            format!("expected argument {} of function {} to be a positive integer value; is $0 instead", par, fun)
                        } else {
                            format!("expected argument {} of function {} to be a positive integer value in the range 0..={}; is $0 instead", par, fun, high)
                        }
                    } else {
                        format!("expected argument {} of function {} to be an integer value in the range {}..={}; is $0 instead", par, fun, low, high)
                    }
                } else if low == i32::MIN {
                    if high == 0 {
                        format!("expected $0 to be a negative integer value")
                    } else {
                        format!("expected $0 to be a negative integer value in the range {}..=0", high)
                    }
                } else if low == i32::MAX {
                    if high == 0 {
                        format!("expected $0 to be a positive integer value")
                    } else {
                        format!("expected $0 to be a positive integer value in the range 0..={}", high)
                    }
                } else {
                    format!("expected $0 to be an integer value in the range {}..={}", low, high)
                }
            },
            ExceptionDescriptor::ArgumentCount(fun, min, max) => {
                let min = *min;
                let max = *max;
                if let Some(fun) = fun {
                    if min == max {
                        format!("{} expects {}, but received $0 arguments: $1", fun, self.arguments(min))
                    } else if max == i32::MAX {
                        format!("{} expects at least {}, but received $0 arguments: $1", fun, self.arguments(min))
                    } else {
                        format!("{} expects between {} and {}, but received $0 arguments: $1", fun, self.arguments(min), self.arguments(max))
                    }
                } else {
                    if min == max {
                        format!("expected {}, but received $0 arguments: $1", self.arguments(min))
                    } else if max == i32::MAX {
                        format!("expected at least {}, but received $0 arguments: $1", self.arguments(min))
                    } else {
                        format!("expected between {} and {}, but received $0 arguments: $1", self.arguments(min), self.arguments(max))
                    }
                }
            }
            ExceptionDescriptor::Eval(error) => error.message().to_string(),
            ExceptionDescriptor::OS(error) => error.to_string(),
            ExceptionDescriptor::Abortion => "abortion".to_string(),
            ExceptionDescriptor::Uncaught => "$0".to_string(),
            ExceptionDescriptor::Custom(_, message) => message.to_string(),
        }.to_string()
    }

    fn arguments(&self, n: i32) -> String {
        if n == 1 {
            "1 argument".to_string()
        } else {
            format!("{} arguments", n)
        }
    }
}
