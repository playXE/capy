use super::{tree_il::IForm, P};

/// Pass 3.  Optimization stage 2
///

/// Closure optimization can introduce superfluous $LET, which can
/// be optimized further.  (In fact, pass2 and pass3 can be repeated
/// until no further optimization can be possible.  However, compilation
/// speed is also important for Capy, so we just run this pass once.)


pub fn pass3_rec(iform: P<IForm>, changed: &mut bool) -> P<IForm> {
    let _ = changed;
    iform
}

pub fn pass3(iform: P<IForm>) -> P<IForm> {
    let mut changed = false;
    while changed {
        pass3_rec(iform.clone(), &mut changed);
    }
    iform
}