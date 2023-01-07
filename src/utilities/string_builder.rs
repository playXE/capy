use core::fmt;
use std::borrow::Cow;

pub struct StringBuilder<'a> {
    buffer: String,
    postfix: &'a str,
    separator: Option<&'a str>,
    initial: Option<&'a str>,
    requires_separator: bool,
}

impl<'a> StringBuilder<'a> {
    pub fn new(
        prefix: &str,
        postfix: &'a str,
        separator: Option<&'a str>,
        initial: Option<&'a str>,
    ) -> Self {
        Self {
            buffer: prefix.to_string(),
            postfix,
            separator,
            initial,
            requires_separator: false,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty() && self.postfix.is_empty()
    }

    pub fn append(&mut self, value: impl ToString) {
        if self.requires_separator {
            self.buffer.push_str(self.separator.unwrap());
        } else {
            self.requires_separator = self.separator.is_some();

            if let Some(init) = self.initial.take() {
                self.buffer.push_str(init);
            }
        }

        self.buffer.push_str(value.to_string().as_str())
    }

   
    pub fn append_align(&mut self, value: impl ToString, width: usize, align_right: bool) {
        let value = value.to_string();
        let pad = width.saturating_sub(value.len());

        if align_right {
            self.append(format!("{}{}", Self::padding(pad), value))
        } else {
            self.append(format!("{}{}", value, Self::padding(pad)))
        }
    }

    pub fn padding(n: usize) -> Cow<'static, str> {
        match n {
            0 => Cow::Borrowed(""),
            1 => Cow::Borrowed(" "),
            2 => Cow::Borrowed("  "),
            3 => Cow::Borrowed("   "),
            4 => Cow::Borrowed("    "),
            5 => Cow::Borrowed("     "),
            6 => Cow::Borrowed("      "),
            7 => Cow::Borrowed("       "),
            8 => Cow::Borrowed("        "),
            9 => Cow::Borrowed("         "),
            _ => Cow::Owned(" ".repeat(n)),
        }
    }
}



impl fmt::Display for StringBuilder<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.buffer, self.postfix)
    }
}