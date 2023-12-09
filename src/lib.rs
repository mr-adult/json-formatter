mod parser;
mod tokenizer;
use parser::{JsonParser, JsonValue};
use tokenizer::{JsonParseErr, JsonTokenKind, JsonTokenizer};

use std::error::Error;

pub fn parse(json: &str) -> Result<JsonValue<'_>, Vec<Box<dyn Error>>> {
    match JsonParser::parse(json) {
        Ok(value) => return Ok(value),
        Err(errs) => {
            return Err(errs
                .into_iter()
                .map(|err| Box::new(err) as Box<dyn Error>)
                .collect())
        }
    }
}

pub fn format(json: &str) -> (String, Option<Vec<Box<dyn Error>>>) {
    let mut result = String::new();
    let tokenizer = JsonTokenizer::new(json);
    let mut errs = Vec::new();
    let mut indent = 0;
    let mut previous = None;

    for token in tokenizer.into_iter() {
        match token {
            Err(err) => match err {
                JsonParseErr::UnexpectedCharacters(span) => {
                    result.push_str(&json[span.as_range()]);
                    errs.push(err);
                }
                err => {
                    errs.push(err);
                }
            },
            Ok(token) => {
                match token.kind {
                    JsonTokenKind::ObjectStart => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        indent += 1;
                        result.push('{');
                    }
                    JsonTokenKind::ObjectEnd => {
                        indent -= 1;
                        if let Some(JsonTokenKind::ObjectStart) = previous {
                            result.push('}');
                        } else {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                            result.push('}');
                        }
                    }
                    JsonTokenKind::ArrayStart => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        indent += 1;
                        result.push('[');
                    }
                    JsonTokenKind::ArrayEnd => {
                        indent -= 1;
                        if let Some(JsonTokenKind::ArrayStart) = previous {
                            result.push(']');
                        } else {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                            result.push(']');
                        }
                    }
                    JsonTokenKind::Colon => {
                        result.push(':');
                        result.push(' ');
                    }
                    JsonTokenKind::Comma => {
                        result.push('\n');
                        for _ in 0..indent {
                            result.push('\t');
                        }
                        result.push(',');
                        result.push(' ');
                    }
                    JsonTokenKind::String => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str(&json[token.span.as_range()])
                    }
                    JsonTokenKind::Number => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str(&json[token.span.as_range()])
                    }
                    JsonTokenKind::True => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str("true")
                    }
                    JsonTokenKind::False => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str("false")
                    }
                    JsonTokenKind::Null => {
                        if let Some(JsonTokenKind::ObjectStart | JsonTokenKind::ArrayStart) =
                            previous
                        {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str("null");
                    }
                }
                previous = Some(token.kind);
            }
        }
    }

    (
        result,
        if !errs.is_empty() {
            Some(
                errs.into_iter()
                    .map(|err| Box::new(err) as Box<dyn Error>)
                    .collect(),
            )
        } else {
            None
        },
    )
}

#[cfg(test)]
mod tests {
    use std::io::{BufRead, Write};

    #[test]
    fn test_bench() {
        loop {
            println!("Enter your JSON:");
            let mut stdin = std::io::stdin().lock();
            let mut input = String::new();
            stdin
                .read_line(&mut input)
                .expect("Failed to read from stdin.");

            if input.trim().is_empty() {
                break;
            }

            let output = super::format(&input);
            let mut stdout = std::io::stdout();
            stdout
                .write_all(output.0.as_bytes())
                .expect("Failed to write to stdout.");
            stdout
                .write(&['\n' as u8])
                .expect("Failed to write to stdout");
            match output.1 {
                None => {}
                Some(errs) => {
                    let mut stdout = std::io::stdout();
                    for err in errs {
                        write!(stdout, "{}", err).expect("Failed to write toe stdout.");
                        write!(stdout, "\n").expect("Failed to write toe stdout.");
                    }
                    stdout.flush().expect("Failed to flush to stdout.");
                }
            }

            let values = super::parse(&input);
            println!("{:?}", values);
        }
    }
}
