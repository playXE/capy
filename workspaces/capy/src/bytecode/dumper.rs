use super::virtual_register::VirtualRegister;

pub struct BytecodeDumper {
    out: Box<dyn std::io::Write>,
    current_location: usize,
}

impl BytecodeDumper {
    pub fn new(out: Box<dyn std::io::Write>) -> Self {
        Self {
            out,
            current_location: 0,
        }
    }

    pub fn print_location_and_op(&mut self, location: usize, op: &str) {
        self.current_location = location;
        write!(self.out, "[{:04}]  {: <18}", location, op).unwrap();
    }

    pub fn write_str(&mut self, s: &str) {
        self.out.write_all(s.as_bytes()).unwrap();
    }
}

impl std::io::Write for BytecodeDumper {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.out.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.out.flush()
    }
}

pub trait DumpOperand<T>: std::io::Write {
    fn dump_operand(&mut self, operand_name: &str, operand: T, is_first: bool) {
        if !is_first {
            self.write_all(", ".as_bytes()).unwrap();
        }
        self.write_fmt(format_args!("{}:", operand_name)).unwrap();
        self.dump_value(operand);
    }

    fn dump_value(&mut self, value: T);
}

impl DumpOperand<VirtualRegister> for BytecodeDumper {
    fn dump_value(&mut self, value: VirtualRegister) {
        if value.is_constant() {
            self.out
                .write_fmt(format_args!("const{}", value.to_constant_index()))
        } else if value.is_argument() {
            self.out
                .write_fmt(format_args!("arg{}", value.to_argument()))
        } else {
            self.out
                .write_fmt(format_args!("local{}", value.to_local()))
        }
        .unwrap();
    }
}
impl<T: std::fmt::Display> DumpOperand<T> for BytecodeDumper {
    fn dump_value(&mut self, value: T) {
        self.out.write_fmt(format_args!("{}", value)).unwrap();
    }
}
