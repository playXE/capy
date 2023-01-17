use crate::{
    compiler::Compiler,
    data::{equality::NumberPair, exception::{SourcePosition, Exception}},
    prelude::{*, code::Ins},
};

pub(crate) fn math_library() {
    let manager = library_manager();
    let thr = Thread::current();

    let base = manager.scheme_module.get_handle_of::<Library>();

    manager.add_definition(thr, base, Definition::NativeSpecial("+", Implementation::Native0R(plus), compile_plus), true, true);

    manager.add_definition(
        thr,
        base,
        Definition::NativeSpecial("-", Implementation::Native1R(minus), compile_minus),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        Definition::NativeSpecial("*", Implementation::Native0R(multiply), compile_mul),
        true,
        true,
    );
    manager.add_definition(
        thr,
        base,
        Definition::NativeSpecial("/", Implementation::Native0R(divide), compile_div),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        Definition::NativeSpecial(">", Implementation::Native2(greater), compile_greater),
        true,
        true,
    );

    manager.add_definition(thr, base, Definition::NativeSpecial("<", Implementation::Native2(less), compile_less), true, true);

    manager.add_definition(thr, base, ("=", Implementation::Native2(equal)), true, true);

    manager.add_definition(
        thr,
        base,
        Definition::NativeSpecial(">=", Implementation::Native2(greater_or_equal), compile_greater_eq),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        Definition::NativeSpecial("<=", Implementation::Native2(less_or_equal), compile_less_eq),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        (
            "arithmetic-shift",
            Implementation::Native2(arithmetic_shift),
        ),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        (
            "arithmetic-shift-right",
            Implementation::Native2(arithmetic_shift_right),
        ),
        true,
        true,
    );
}

pub fn plus(ctx: &mut Context, args: &Arguments) -> ScmResult {
    let mut acc = Value::new(0i32);

    for expr in args {
        acc = NumberPair::new(ctx, acc, *expr)?.add();
    }

    Ok(acc)
}

// `(+ <expr> <expr> ...)` is compiled to `Add2` or to a call to `+` at runtime if more than 2 arguments is supplied.
pub fn compile_plus(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    tail: bool,
) -> ScmResult<bool> {
    if form.is_pair()
        && form.cdr().is_pair()
        && form.cdr().cdr().is_pair()
        && form.cdr().cdr().cdr().is_null()
    {
        let acc = form.cdr().car();
        let expr = form.cdr().cdr().car();

        cc.compile(ctx, expr, false)?;
        cc.compile(ctx, acc, false)?;
        cc.emit(ctx, Ins::Add2);
        return Ok(false);
    } else if form.is_pair() {
        let sym = ctx.runtime().symbol_table().intern("+");
        let x = cc.make_identifier(ctx, sym);

        cc.emit(ctx, Ins::MakeFrame);
        let ix = cc.add_constant(ctx, x);
        cc.emit(ctx, Ins::PushGlobal(ix as _));
        let nargs = cc.compile_exprs(ctx, form.cdr())?;
        return Ok(cc.call(ctx, nargs, tail));
    } else {
        let exc = Exception::argument_count(ctx, Some("+"), 2, usize::MAX, form, SourcePosition::unknown());

        return Err(exc.into());
    }
}

pub fn compile_minus(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    tail: bool,
) -> ScmResult<bool> {
    if form.is_pair()
        && form.cdr().is_pair()
        && form.cdr().cdr().is_pair()
        && form.cdr().cdr().cdr().is_null()
    {
        let acc = form.cdr().car();
        let expr = form.cdr().cdr().car();

        
        cc.compile(ctx, acc, false)?;
        cc.compile(ctx, expr, false)?;
        cc.emit(ctx, Ins::Sub2);
        return Ok(false);
    } else if form.is_pair() {
        let sym = ctx.runtime().symbol_table().intern("-");
        let x = cc.make_identifier(ctx, sym);

        cc.emit(ctx, Ins::MakeFrame);
        let ix = cc.add_constant(ctx, x);
        cc.emit(ctx, Ins::PushGlobal(ix as _));
        let nargs = cc.compile_exprs(ctx, form.cdr())?;
        return Ok(cc.call(ctx, nargs, tail));
    } else {
        let exc = Exception::argument_count(ctx, Some("-"), 2, usize::MAX, form, SourcePosition::unknown());

        return Err(exc.into());
    }
}

pub fn compile_div(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    tail: bool,
) -> ScmResult<bool> {
    if form.is_pair()
        && form.cdr().is_pair()
        && form.cdr().cdr().is_pair()
        && form.cdr().cdr().cdr().is_null()
    {
        let acc = form.cdr().car();
        let expr = form.cdr().cdr().car();

        cc.compile(ctx, acc, false)?;
        cc.compile(ctx, expr, false)?;
        
        cc.emit(ctx, Ins::Div2);
        return Ok(false);
    } else if form.is_pair() {
        let sym = ctx.runtime().symbol_table().intern("/");
        let x = cc.make_identifier(ctx, sym);

        cc.emit(ctx, Ins::MakeFrame);
        let ix = cc.add_constant(ctx, x);
        cc.emit(ctx, Ins::PushGlobal(ix as _));
        let nargs = cc.compile_exprs(ctx, form.cdr())?;
        return Ok(cc.call(ctx, nargs, tail));
    } else {
        let exc = Exception::argument_count(ctx, Some("/"), 2, usize::MAX, form, SourcePosition::unknown());

        return Err(exc.into());
    }
}

pub fn compile_mul(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    tail: bool,
) -> ScmResult<bool> {
    if form.is_pair()
        && form.cdr().is_pair()
        && form.cdr().cdr().is_pair()
        && form.cdr().cdr().cdr().is_null()
    {
        let acc = form.cdr().car();
        let expr = form.cdr().cdr().car();

        cc.compile(ctx, acc, false)?;
        cc.compile(ctx, expr, false)?;
        
        cc.emit(ctx, Ins::Mul2);
        return Ok(false);
    } else if form.is_pair() {
        let sym = ctx.runtime().symbol_table().intern("*");
        let x = cc.make_identifier(ctx, sym);

        cc.emit(ctx, Ins::MakeFrame);
        let ix = cc.add_constant(ctx, x);
        cc.emit(ctx, Ins::PushGlobal(ix as _));
        let nargs = cc.compile_exprs(ctx, form.cdr())?;
        return Ok(cc.call(ctx, nargs, tail));
    } else {
        let exc = Exception::argument_count(ctx, Some("*"), 2, usize::MAX, form, SourcePosition::unknown());

        return Err(exc.into());
    }
}

pub fn compile_greater(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    tail: bool,
) -> ScmResult<bool> {
    if form.is_pair()
        && form.cdr().is_pair()
        && form.cdr().cdr().is_pair()
        && form.cdr().cdr().cdr().is_null()
    {
        let acc = form.cdr().car();
        let expr = form.cdr().cdr().car();

        cc.compile(ctx, acc, false)?;
        cc.compile(ctx, expr, false)?;
        
        cc.emit(ctx, Ins::Greater2);
        return Ok(false);
    } else if form.is_pair() {
        let sym = ctx.runtime().symbol_table().intern(">");
        let x = cc.make_identifier(ctx, sym);

        cc.emit(ctx, Ins::MakeFrame);
        let ix = cc.add_constant(ctx, x);
        cc.emit(ctx, Ins::PushGlobal(ix as _));
        let nargs = cc.compile_exprs(ctx, form.cdr())?;
        return Ok(cc.call(ctx, nargs, tail));
    } else {
        let exc = Exception::argument_count(ctx, Some(">"), 2, usize::MAX, form, SourcePosition::unknown());

        return Err(exc.into());
    }
}

pub fn compile_less(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    tail: bool,
) -> ScmResult<bool> {
    if form.is_pair()
        && form.cdr().is_pair()
        && form.cdr().cdr().is_pair()
        && form.cdr().cdr().cdr().is_null()
    {
        let acc = form.cdr().car();
        let expr = form.cdr().cdr().car();

        cc.compile(ctx, acc, false)?;
        cc.compile(ctx, expr, false)?;
        
        cc.emit(ctx, Ins::Less2);
        return Ok(false);
    } else if form.is_pair() {
        let sym = ctx.runtime().symbol_table().intern("<");
        let x = cc.make_identifier(ctx, sym);

        cc.emit(ctx, Ins::MakeFrame);
        let ix = cc.add_constant(ctx, x);
        cc.emit(ctx, Ins::PushGlobal(ix as _));
        let nargs = cc.compile_exprs(ctx, form.cdr())?;
        return Ok(cc.call(ctx, nargs, tail));
    } else {
        let exc = Exception::argument_count(ctx, Some("<"), 2, usize::MAX, form, SourcePosition::unknown());

        return Err(exc.into());
    }
}

pub fn compile_greater_eq(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    tail: bool,
) -> ScmResult<bool> {
    if form.is_pair()
        && form.cdr().is_pair()
        && form.cdr().cdr().is_pair()
        && form.cdr().cdr().cdr().is_null()
    {
        let acc = form.cdr().car();
        let expr = form.cdr().cdr().car();

        cc.compile(ctx, expr, false)?;
        cc.compile(ctx, acc, false)?;
        cc.emit(ctx, Ins::GreaterEq2);
        return Ok(false);
    } else if form.is_pair() {
        let sym = ctx.runtime().symbol_table().intern(">=");
        let x = cc.make_identifier(ctx, sym);

        cc.emit(ctx, Ins::MakeFrame);
        let ix = cc.add_constant(ctx, x);
        cc.emit(ctx, Ins::PushGlobal(ix as _));
        let nargs = cc.compile_exprs(ctx, form.cdr())?;
        return Ok(cc.call(ctx, nargs, tail));
    } else {
        let exc = Exception::argument_count(ctx, Some(">="), 2, usize::MAX, form, SourcePosition::unknown());

        return Err(exc.into());
    }
}

pub fn compile_less_eq(
    cc: &mut Compiler,
    ctx: &mut Context,
    form: Value,
    tail: bool,
) -> ScmResult<bool> {
    if form.is_pair()
        && form.cdr().is_pair()
        && form.cdr().cdr().is_pair()
        && form.cdr().cdr().cdr().is_null()
    {
        let acc = form.cdr().car();
        let expr = form.cdr().cdr().car();

        cc.compile(ctx, expr, false)?;
        cc.compile(ctx, acc, false)?;
        cc.emit(ctx, Ins::LessEq2);
        return Ok(false);
    } else if form.is_pair() {
        let sym = ctx.runtime().symbol_table().intern("<=");
        let x = cc.make_identifier(ctx, sym);

        cc.emit(ctx, Ins::MakeFrame);
        let ix = cc.add_constant(ctx, x);
        cc.emit(ctx, Ins::PushGlobal(ix as _));
        let nargs = cc.compile_exprs(ctx, form.cdr())?;
        return Ok(cc.call(ctx, nargs, tail));
    } else {
        let exc = Exception::argument_count(ctx, Some("<="), 2, usize::MAX, form, SourcePosition::unknown());

        return Err(exc.into());
    }
}





pub fn minus(ctx: &mut Context, first: Value, args: &Arguments) -> ScmResult {
    let mut acc = first;

    if args.is_empty() {
        if acc.is_int32() {
            return Ok(Value::new(-acc.get_int32()));
        } else if acc.is_double() {
            return Ok(Value::new(-acc.get_double()));
        } else {
            acc.assert_type(ctx, SourcePosition::unknown(), &[Type::Number])?;
            unreachable!()
        }
    }

    for expr in args {
        acc = NumberPair::new(ctx, acc, *expr)?.sub();
    }
    Ok(acc)
}

pub fn multiply(ctx: &mut Context, args: &Arguments) -> ScmResult {
    let mut acc = Value::new(0i32);

    for expr in args {
        acc = NumberPair::new(ctx, acc, *expr)?.mul();
    }
    Ok(acc)
}

pub fn divide(ctx: &mut Context, args: &Arguments) -> ScmResult {
    let mut acc = Value::new(0i32);

    for expr in args {
        acc = NumberPair::new(ctx, acc, *expr)?.div();
    }
    Ok(acc)
}

pub fn greater(ctx: &mut Context, x: Value, y: Value) -> ScmResult {
    Ok(match NumberPair::new(ctx, x, y)?.cmp() {
        std::cmp::Ordering::Greater => Value::new(true),
        _ => Value::new(false),
    })
}

pub fn less(ctx: &mut Context, x: Value, y: Value) -> ScmResult {
    Ok(match NumberPair::new(ctx, x, y)?.cmp() {
        std::cmp::Ordering::Less => Value::new(true),
        _ => Value::new(false),
    })
}

pub fn equal(ctx: &mut Context, x: Value, y: Value) -> ScmResult {
    Ok(match NumberPair::new(ctx, x, y)?.cmp() {
        std::cmp::Ordering::Equal => Value::new(true),
        _ => Value::new(false),
    })
}

pub fn greater_or_equal(ctx: &mut Context, x: Value, y: Value) -> ScmResult {
    Ok(match NumberPair::new(ctx, x, y)?.cmp() {
        std::cmp::Ordering::Greater | std::cmp::Ordering::Equal => Value::new(true),
        _ => Value::new(false),
    })
}

pub fn less_or_equal(ctx: &mut Context, x: Value, y: Value) -> ScmResult {
    Ok(match NumberPair::new(ctx, x, y)?.cmp() {
        std::cmp::Ordering::Less | std::cmp::Ordering::Equal => Value::new(true),
        _ => Value::new(false),
    })
}

pub fn arithmetic_shift(ctx: &mut Context, num: Value, n: Value) -> ScmResult {
    n.assert_type(ctx, SourcePosition::unknown(), &[Type::Integer])?;
    num.assert_type(ctx, SourcePosition::unknown(), &[Type::Integer])?;

    let y = n.get_int32();
    let x = num.get_int32();

    if y <= 0 {
        return Ok(Value::new(x.wrapping_shr((-y) as u32)));
    } else if x == 0 {
        return Ok(Value::new(0));
    } else if x > 0 {
        if y < x.leading_zeros() as i32 - 1 {
            return Ok(Value::new(x << y));
        } else {
            todo!("overflow to bigint")
        }
    } else {
        if y < ((!x).leading_zeros() as i32 - 1) {
            return Ok(Value::new(x << y));
        } else {
            todo!("overflow to bigint")
        }
    }
}

pub fn arithmetic_shift_right(ctx: &mut Context, num: Value, n: Value) -> ScmResult {
    n.assert_type(ctx, SourcePosition::unknown(), &[Type::Integer])?;
    num.assert_type(ctx, SourcePosition::unknown(), &[Type::Integer])?;

    let y = n.get_int32();
    let x = num.get_int32();

    return Ok(Value::new(x >> y));
}
