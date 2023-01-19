use crate::{prelude::Value, ScmResult};

pub fn format(form: &str, values: &[Value]) -> ScmResult<String> {
    let mut output = String::with_capacity(form.len());

    let mut chars = form.chars().peekable();

    let mut current = 0;

    while let Some(ch) = chars.next() {
        match ch {
            '~' => match chars.peek() {
                None => {}

                Some(p) => match p {
                    c if c.is_whitespace() => {
                        while let Some(_) = chars
                            .next()
                            .filter(|&x| x == '\r' || x == '\n' || x.is_whitespace())
                        {
                        }
                        continue;
                    }

                    '~' => {
                        chars.next().unwrap();
                        output.push('~');
                    }

                    'a' | 'A' => {
                        chars.next().unwrap();
                        output.push_str(&values[current].to_string(false));
                        current += 1;
                    }

                    

                    _ => todo!(),
                },
            },

            _ => output.push(ch),
        }
    }

    Ok(output)
}
