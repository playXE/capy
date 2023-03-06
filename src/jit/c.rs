use std::sync::atomic::AtomicUsize;

static C_NAME: AtomicUsize = AtomicUsize::new(0);
static V_NAME: AtomicUsize = AtomicUsize::new(0);

pub fn lambda_name() -> String {
    format!("k_{}", C_NAME.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
}

pub fn var_name() -> String {
    format!("v{}", V_NAME.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
}


pub struct Compiler {
    buffer: String,
}

impl Compiler {
    pub fn new() -> Self {
        let mut this = Self {
            buffer: String::with_capacity(1024)
        };

        this.buffer.push_str(include_str!("base.h"));

        this 
    }


    fn generate_prelude(&mut self)
    
}