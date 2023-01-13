use crate::{data::{equality::NumberPair, exception::SourcePosition}, prelude::*};

pub(crate) fn math_library() {
    let manager = library_manager();
    let thr = Thread::current();

    let base = manager.scheme_module.get_handle_of::<Library>();


    manager.add_definition(thr, base, ("+", Implementation::Native0R(plus)), true, true);

    manager.add_definition(
        thr,
        base,
        ("-", Implementation::Native1R(minus)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        ("*", Implementation::Native0R(multiply)),
        true,
        true 
    );
    manager.add_definition(
        thr,
        base,
        ("/", Implementation::Native0R(divide)),
        true,
        true,
    );

    manager.add_definition(
        thr,
        base,
        (">", Implementation::Native2(greater)),
        true,
        true 
    );

    manager.add_definition(
        thr,
        base,
        ("<", Implementation::Native2(less)),
        true,
        true
    );

    manager.add_definition(
        thr,
        base,
        ("=", Implementation::Native2(equal)),
        true,
        true
    );

    manager.add_definition(
        thr,
        base,
        (">=", Implementation::Native2(greater_or_equal)),
        true,
        true
    );

    manager.add_definition(
        thr,
        base,
        ("<=", Implementation::Native2(less_or_equal)),
        true,
        true
    );

    manager.add_definition(
        thr,
        base,
        ("arithmetic-shift", Implementation::Native2(arithmetic_shift)),
        true,
        true
    );

    manager.add_definition(
        thr,
        base,
        ("arithmetic-shift-right", Implementation::Native2(arithmetic_shift_right)),
        true,
        true
    );
}

pub fn plus(ctx: &mut Context, args: &Arguments) -> ScmResult {
    let mut acc = Value::new(0i32);
    
    for expr in args {
        acc = NumberPair::new(ctx, acc, *expr)?.add();
    }

    Ok(acc)
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
        _ => Value::new(false)
    })
}

pub fn less(ctx: &mut Context, x: Value, y: Value) -> ScmResult {
    Ok(match NumberPair::new(ctx, x, y)?.cmp() {
        std::cmp::Ordering::Less => Value::new(true),
        _ => Value::new(false)
    })
}

pub fn equal(ctx: &mut Context, x: Value, y: Value) -> ScmResult {
    Ok(match NumberPair::new(ctx, x, y)?.cmp() {
        std::cmp::Ordering::Equal => Value::new(true),
        _ => Value::new(false)
    })
}

pub fn greater_or_equal(ctx: &mut Context, x: Value, y: Value) -> ScmResult {
    Ok(match NumberPair::new(ctx, x, y)?.cmp() {
        std::cmp::Ordering::Greater | std::cmp::Ordering::Equal => Value::new(true),
        _ => Value::new(false)
    })
}

pub fn less_or_equal(ctx: &mut Context, x: Value, y: Value) -> ScmResult {
    Ok(match NumberPair::new(ctx, x, y)?.cmp() {
        std::cmp::Ordering::Less | std::cmp::Ordering::Equal => Value::new(true),
        _ => Value::new(false)
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