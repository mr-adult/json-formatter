use std::{collections::HashMap, iter::Peekable, vec::IntoIter};

use crate::{
    tokenizer::{JsonParseErr, JsonToken, JsonTokenKind, JsonTokenizer, UnexpectedToken},
    JsonNumber, JsonString, JsonValue,
};

pub(crate) struct JsonParser<'i> {
    source: &'i str,
    tokens: Peekable<IntoIter<JsonToken>>,
}

impl<'i> JsonParser<'i> {
    pub(crate) fn parse(json: &'i str) -> Result<JsonValue<'i>, Vec<JsonParseErr>> {
        let tokenizer = JsonTokenizer::new(json);

        let mut tokens = Vec::new();
        let mut errs = Vec::new();
        for result in tokenizer {
            match result {
                Ok(token) => {
                    tokens.push(token);
                }
                Err(err) => {
                    errs.push(err);
                }
            }
        }
        if !errs.is_empty() {
            return Err(errs);
        }

        let mut parser = Self {
            source: json,
            tokens: tokens.into_iter().peekable(),
        };

        match parser.match_value() {
            Err(err) => Err(vec![err]),
            Ok(value) => Ok(value),
        }
    }

    fn match_value(&mut self) -> Result<JsonValue<'i>, JsonParseErr> {
        match self.tokens.peek() {
            None => {
                return Err(JsonParseErr::UnexpectedToken(UnexpectedToken {
                    expected: JsonTokenKind::Null,
                    actual: None,
                }));
            }
            Some(token) => match token.kind {
                JsonTokenKind::Null => {
                    self.tokens.next();
                    return Ok(JsonValue::Null)
                }
                JsonTokenKind::False => {
                    self.tokens.next();
                    return Ok(JsonValue::Boolean(false))
                }
                JsonTokenKind::True => {
                    self.tokens.next();
                    return Ok(JsonValue::Boolean(true))
                }
                JsonTokenKind::Number => {
                    return Ok(JsonValue::Number(Box::new(JsonNumber::new(
                        &self.source[self
                            .tokens
                            .next()
                            .expect("Token to be populated since we just matched a peeked value")
                            .span
                            .as_range()],
                    ))));
                }
                JsonTokenKind::String => {
                    return Ok(JsonValue::String(Box::new(JsonString::new(
                        &self.source[self
                            .tokens
                            .next()
                            .expect("Token to be populated since we just matched a peeked value")
                            .span
                            .as_range()],
                    ))));
                }
                _ => {}
            },
        }

        if let Some(obj) = self.match_object() {
            return obj;
        }

        if let Some(arr) = self.match_array() {
            return arr;
        }

        return Err(JsonParseErr::UnexpectedToken(UnexpectedToken {
            expected: JsonTokenKind::Null,
            actual: self.tokens.next(),
        }));
    }

    fn match_object(&mut self) -> Option<Result<JsonValue<'i>, JsonParseErr>> {
        if self.match_token(JsonTokenKind::ObjectStart).is_none() {
            return None;
        }

        let mut kvps = HashMap::new();
        if let Some(_) = self.match_token(JsonTokenKind::ObjectEnd) {
            return Some(Ok(JsonValue::Object(kvps)));
        }

        loop {
            let key = self.match_token_or_err(JsonTokenKind::String);
            let colon = self.match_token_or_err(JsonTokenKind::Colon);
            let value = self.match_value();

            let key = match key {
                Err(err) => return Some(Err(err)),
                Ok(token) => token,
            };
            let key_str = &self.source[key.span.as_range()];
            if kvps.contains_key(key_str) {
                return Some(Err(JsonParseErr::DuplicateKey(key)));
            }

            if let Err(err) = colon {
                return Some(Err(err));
            }

            let value = match value {
                Err(err) => return Some(Err(err)),
                Ok(token) => token,
            };

            kvps.insert(key_str, value);

            if self.match_token(JsonTokenKind::Comma).is_none() {
                break;
            }
        }

        if let Err(err) = self.match_token_or_err(JsonTokenKind::ObjectEnd) {
            return Some(Err(err));
        }
        Some(Ok(JsonValue::Object(kvps)))
    }

    fn match_array(&mut self) -> Option<Result<JsonValue<'i>, JsonParseErr>> {
        if self.match_token(JsonTokenKind::ArrayStart).is_none() {
            return None;
        }

        let mut values = Vec::new();
        if let Some(_) = self.match_token(JsonTokenKind::ArrayEnd) {
            return Some(Ok(JsonValue::Array(values)));
        }

        loop {
            let value = self.match_value();
            let value = match value {
                Err(err) => return Some(Err(err)),
                Ok(value) => value,
            };
            values.push(value);

            if self.match_token(JsonTokenKind::Comma).is_none() {
                break;
            }
        }

        if let Err(err) = self.match_token_or_err(JsonTokenKind::ArrayEnd) {
            return Some(Err(err));
        }
        Some(Ok(JsonValue::Array(values)))
    }

    fn match_token(&mut self, kind: JsonTokenKind) -> Option<JsonToken> {
        if let Some(peeked) = self.tokens.peek() {
            if peeked.kind == kind {
                println!("Matched {:?}", kind);
                return self.tokens.next();
            }
        }
        return None;
    }

    fn match_token_or_err(&mut self, expected: JsonTokenKind) -> Result<JsonToken, JsonParseErr> {
        match self.match_token(expected) {
            None => Err(JsonParseErr::UnexpectedToken(UnexpectedToken {
                expected, 
                actual: self.tokens.next(),
            })),
            Some(token) => {
                println!("Matched {:?}", token.kind);
                Ok(token)
            }
        }
    }
}
