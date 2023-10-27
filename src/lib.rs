use std::{str::CharIndices, iter::Peekable, ops::Range};

use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub fn format(json: &str) -> String {
    let mut result = String::new();
    let tokenizer = JsonTokenizer::new(&json).peekable();
    let mut errs = Vec::new();
    let mut indent = 0;
    let mut previous = None;

    for token in tokenizer.into_iter() {
        match token {
            Err(err) => { errs.push(err); }
            Ok(token) => {
                match token.kind {
                    JsonTokenKind::ObjectStart => {
                        if let Some(JsonTokenKind::ObjectStart 
                            | JsonTokenKind::ArrayStart
                            | JsonTokenKind::Comma
                        ) = previous {
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
                    },
                    JsonTokenKind::ArrayStart => {
                        if let Some(JsonTokenKind::ObjectStart 
                            | JsonTokenKind::ArrayStart
                            | JsonTokenKind::Comma
                        ) = previous {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        indent += 1;
                        result.push('[');
                    },
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
                    },
                    JsonTokenKind::Colon => {
                        result.push(':');
                        result.push(' ');
                    },
                    JsonTokenKind::Comma => {
                        result.push(',');
                    },
                    JsonTokenKind::String => {
                        if let Some(JsonTokenKind::ObjectStart 
                            | JsonTokenKind::ArrayStart
                            | JsonTokenKind::Comma
                        ) = previous {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str(&json[token.span.as_range()])
                    },
                    JsonTokenKind::Number => {
                        if let Some(JsonTokenKind::ObjectStart 
                            | JsonTokenKind::ArrayStart 
                            | JsonTokenKind::Comma
                        ) = previous {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str(&json[token.span.as_range()])
                    },
                    JsonTokenKind::True => {
                        if let Some(JsonTokenKind::ObjectStart 
                            | JsonTokenKind::ArrayStart 
                            | JsonTokenKind::Comma
                        ) = previous {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str("true")
                    },
                    JsonTokenKind::False => {
                        if let Some(JsonTokenKind::ObjectStart 
                            | JsonTokenKind::ArrayStart 
                            | JsonTokenKind::Comma
                        ) = previous {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str("false")
                    },
                    JsonTokenKind::Null => {
                        if let Some(JsonTokenKind::ObjectStart 
                            | JsonTokenKind::ArrayStart 
                            | JsonTokenKind::Comma
                        ) = previous {
                            result.push('\n');
                            for _ in 0..indent {
                                result.push('\t');
                            }
                        }
                        result.push_str("null");
                    },
                }
                previous = Some(token.kind);
            }
        }
    }

    result
}

struct JsonTokenizer<'i> {
    source_len: usize,
    chars: Peekable<CharIndices<'i>>,
    states: Vec<JsonTokenizerState>,
    yielded_state: JsonValueYieldState,
    lookahead: Option<JsonToken>,
    current_position: Position,
}

impl<'i> JsonTokenizer<'i> {
    fn new(source: &'i str) -> Self {
        let mut states = Vec::new();
        states.push(JsonTokenizerState::Value);
        Self {
            source_len: source.len(),
            chars: source.char_indices().peekable(),
            states,
            yielded_state: JsonValueYieldState::NotYielded,
            lookahead: None,
            current_position: Position { 
                line: 1, 
                col: 0, 
                raw: 0 
            }
        }
    }

    fn match_whitespace(&mut self) {
        loop {
            let matched = self.match_char_if(|ch| {
                match ch {
                    ' '
                    | '\n'
                    | '\r'
                    | '\t' => true,
                    _ => false,
                }
            });

            if !matched { break; }
        }   
    }

    fn match_string(&mut self) -> Result<JsonToken, TokenizerError> {
        let quote = self.next_char();
        let quote = quote.expect("quote to be Some(\")");
        assert!(quote.1 == '"');
        let start = self.current_position;
        loop {
            match self.next_char() {
                None => {
                    let mut new_unclosed = Vec::with_capacity(0);
                    std::mem::swap(&mut new_unclosed, &mut self.states);
                    return Err(TokenizerError::UnexpectedEOF(new_unclosed));
                }
                Some(ch) => {
                    match ch.1 {
                        '"' => {
                            return Ok(JsonToken { 
                                span: Span {
                                    start: start,
                                    end: self.peek_position(),
                                },
                                kind: JsonTokenKind::String 
                            });
                        }
                        '\\' => {
                            // we're just tokenizing, not interpreting the value's escape sequences. 
                            // Quote is the only character that matters to us.
                            self.match_char('"');
                        }
                        _ => {} // just continue
                    }    
                }
            }
        }
    }

    fn match_number(&mut self) -> (JsonToken, Option<TokenizerError>) {
        self.next_char().expect("Start of number to be Some(0..=9 | '-')");
        let start = self.current_position;
        self.match_char('-');
        let index_of_leading_0 = if let Some(ch_index) = self.chars.peek() {
            ch_index.0
        } else { 0 };

        let mut leading_0_err = None;
        if self.match_char('0') && self.match_char_if(|ch| ch.is_ascii_digit()) {
            leading_0_err = Some(TokenizerError::IllegalLeading0(index_of_leading_0));
        }
        self.match_char_while(|ch| ch.is_ascii_digit());
        if self.match_char('.') {
            self.match_char_while(|ch| ch.is_ascii_digit());
        }
        
        if self.match_char_if(|ch| ch == 'e' || ch == 'E') {
            self.match_char_if(|ch| ch == '-' || ch == '+');
            self.match_char_while(|ch| ch.is_ascii_digit());
        }

        return (
            JsonToken {
                span: Span {
                    start: start,
                    end: self.peek_position(),
                },
                kind: JsonTokenKind::Number,
            },
            leading_0_err,
        )
    }

    fn match_literal(&mut self, str: &str) -> bool {
        for char in str.chars() {
            if !self.match_char(char) { return false; }
        }
        return true;
    }

    fn match_char(&mut self, char: char) -> bool {
        self.match_char_if(|ch| ch == char)
    }

    fn match_char_while<P: FnMut(char) -> bool>(&mut self, mut predicate: P) {
        loop {
            match self.chars.peek() {
                None => break,
                Some(char) => {
                    if predicate(char.1) { self.next_char(); }
                    else { break; }
                }
            }
        }
    }

    fn match_char_if<P: FnMut(char) -> bool>(&mut self, mut predicate: P) -> bool {
        match self.chars.peek() {
            None => false,
            Some(char) => {
                if predicate(char.1) {
                    self.next_char();
                    true
                } else {
                    false
                }
            }
        }
    }

    fn next_char(&mut self) -> Option<(usize, char)> {
        if let Some(ch_index) = self.chars.next() {
            if ch_index.1 == '\n' {
                self.current_position.line += 1;
                self.current_position.col = 0;
            } else {
                self.current_position.col += 1;
            }
            self.current_position.raw = ch_index.0;
            Some(ch_index)
        } else {
            None
        }
    }

    fn peek_position(&mut self) -> Position {
        let mut result = self.current_position;
        if let Some(ch_index) = self.chars.peek() {
            if ch_index.1 == '\n' {
                result.line += 1;
                result.col = 0;
            } else {
                result.col += 1;
            }
            result.raw = ch_index.0;
            result
        } else {
            result.raw = self.source_len;
            result
        }
    }
}

impl<'i> Iterator for JsonTokenizer<'i> {
    type Item = Result<JsonToken, TokenizerError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(lookahead) = std::mem::take(&mut self.lookahead) {
            return Some(Ok(lookahead));
        }

        loop {
            match self.states.pop() {
                None => {
                    if let JsonValueYieldState::YieldedErrorMultipleValue = self.yielded_state {
                        return None;
                    }
                    if self.chars.peek().is_some() {
                        self.yielded_state = JsonValueYieldState::YieldedErrorMultipleValue;
                        return Some(Err(TokenizerError::MultipleValues));
                    }
                    return None;
                }
                Some(state) => {
                    match state {
                        JsonTokenizerState::Value => {
                            self.match_whitespace();
                            match self.chars.peek() {
                                None => {
                                    if self.states.is_empty() {
                                        return None;
                                    } else {
                                        let mut states = Vec::with_capacity(0);
                                        std::mem::swap(&mut states, &mut self.states);
                                        return Some(Err(TokenizerError::UnexpectedEOF(states)))
                                    }
                                }
                                Some(ch) => {
                                    if self.states.len() > 0 {
                                        self.states.push(JsonTokenizerState::AfterValue);
                                    }
                                    match ch.1 {
                                        '{' => {
                                            self.states.push(JsonTokenizerState::Object);
                                            self.next_char();
                                            let span = Span {
                                                start: self.current_position,
                                                end: self.peek_position(),
                                            };
                                            return Some(Ok(JsonToken { 
                                                span,
                                                kind: JsonTokenKind::ObjectStart 
                                            }));
                                        }
                                        '[' => {
                                            self.states.push(JsonTokenizerState::Array);
                                            self.next_char();
                                            let start = self.current_position;
                                            let span = Span {
                                                start,
                                                end: self.peek_position(),
                                            };
                                            return Some(Ok(JsonToken { 
                                                span,
                                                kind: JsonTokenKind::ArrayStart 
                                            }));
                                        }
                                        '"' => {
                                            return Some(self.match_string());
                                        }
                                        '-' | '0'..='9' => {
                                            let num_value = self.match_number();
                                            match num_value {
                                                (token, Some(err)) => {
                                                    self.lookahead = Some(token);
                                                    return Some(Err(err));
                                                }
                                                (token, None) => return Some(Ok(token)),
                                            }
                                        }
                                        _ => {
                                            let current_position = self.current_position;
                                            if self.match_literal("true") {
                                                return Some(Ok(JsonToken { 
                                                    span: Span {
                                                        start: current_position,
                                                        end: self.peek_position(),
                                                    }, 
                                                    kind: JsonTokenKind::True 
                                                }));
                                            }

                                            if self.match_literal("false") {
                                                return Some(Ok(JsonToken { 
                                                    span: Span {
                                                        start: current_position,
                                                        end: self.peek_position(),
                                                    }, 
                                                    kind: JsonTokenKind::False 
                                                }));
                                            }

                                            if self.match_literal("null") {
                                                return Some(Ok(JsonToken { 
                                                    span: Span {
                                                        start: current_position,
                                                        end: self.peek_position(),
                                                    }, 
                                                    kind: JsonTokenKind::Null 
                                                }));
                                            }

                                            self.next_char();
                                            // re-push value back onto the stack, removing AfterValue
                                            self.states.pop();
                                            self.states.push(JsonTokenizerState::Value);
                                            return Some(Err(TokenizerError::UnexpectedCharacter(JsonTokenizerState::Value, current_position)));
                                        }
                                    }
                                }
                            }
                        }
                        JsonTokenizerState::Object => {
                            self.match_whitespace();
                            let start = self.current_position;
                            if self.match_char('}') {
                                return Some(Ok(JsonToken {
                                    span: Span {
                                        start,
                                        end: self.current_position,
                                    },
                                    kind: JsonTokenKind::ObjectEnd
                                }))
                            }

                            // need to find the colon + value next. Put them on the stack!
                            self.states.push(JsonTokenizerState::Object);
                            self.states.push(JsonTokenizerState::Value);
                            self.states.push(JsonTokenizerState::KeyValuePairColon);

                            return Some(self.match_string());
                        }
                        JsonTokenizerState::KeyValuePairColon => {
                            self.match_whitespace();
                            if self.match_char(':') {
                                return Some(Ok(JsonToken {
                                    span: Span { 
                                        start: self.current_position, 
                                        end: self.peek_position() 
                                    },
                                    kind: JsonTokenKind::Colon,
                                }));
                            } else {
                                return Some(Err(
                                    TokenizerError::UnexpectedCharacter(
                                        JsonTokenizerState::KeyValuePairColon, 
                                        self.current_position
                                    )
                                ));
                            }
                        }
                        JsonTokenizerState::KeyValuePairKey => {
                            self.match_whitespace();
                            return Some(self.match_string());
                        }
                        JsonTokenizerState::AfterValue => {
                            self.match_whitespace();
                            let start = self.current_position;
                            if self.match_char(',') {
                                assert!(self.states.len() > 0);

                                match self.states.get(self.states.len() - 1).expect("states to include at least 1 value") {
                                    JsonTokenizerState::Object => {
                                        self.states.push(JsonTokenizerState::Value);
                                        self.states.push(JsonTokenizerState::KeyValuePairColon);
                                        self.states.push(JsonTokenizerState::KeyValuePairKey);
                                        return Some(Ok(JsonToken {
                                            span: Span {
                                                start: self.current_position,
                                                end: self.peek_position(),
                                            },
                                            kind: JsonTokenKind::Comma,
                                        }))
                                    }
                                    JsonTokenizerState::Array => {
                                        self.states.push(JsonTokenizerState::Value);
                                        return Some(Ok(JsonToken {
                                            span: Span {
                                                start,
                                                end: self.current_position,
                                            },
                                            kind: JsonTokenKind::Comma,
                                        }))
                                    }
                                    _ => {
                                        return Some(Err(
                                            TokenizerError::UnexpectedCharacter(
                                                JsonTokenizerState::AfterValue, 
                                                start
                                            )
                                        ));
                                    }
                                } 
                            }
                        }
                        JsonTokenizerState::Array => {
                            self.match_whitespace();
                            let start = self.current_position;
                            if self.match_char(']') {
                                return Some(Ok(JsonToken { 
                                    span: Span {
                                        start,
                                        end: self.current_position,
                                    }, 
                                    kind: JsonTokenKind::ArrayEnd,
                                }))
                            }

                            self.states.push(JsonTokenizerState::Array);
                            self.states.push(JsonTokenizerState::Value);
                        }
                    }
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
struct JsonToken {
    span: Span,
    kind: JsonTokenKind,
}

#[derive(Clone, Copy, Debug, Default)]
struct Span {
    start: Position,
    end: Position,
}

impl Span {
    fn as_range(&self) -> Range<usize> {
        self.start.raw..self.end.raw
    }
}

#[derive(Clone, Copy, Debug, Default)]
struct Position {
    line: usize,
    col: usize,
    raw: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum JsonTokenKind {
    ObjectStart,
    ObjectEnd,
    ArrayStart,
    ArrayEnd,
    Colon,
    Comma,
    String,
    Number,
    True,
    False,
    Null,
}

#[derive(Debug, Clone, Copy)]
enum JsonTokenizerState {
    Object,
    Array,
    KeyValuePairColon,
    KeyValuePairKey,
    Value,
    AfterValue,
}

#[derive(Clone, Copy)]
enum JsonValueYieldState {
    NotYielded,
    YieldedErrorMultipleValue,
}

#[derive(Debug, Clone)]
enum TokenizerError {
    UnexpectedEOF(Vec<JsonTokenizerState>),
    IllegalLeading0(usize),
    UnexpectedCharacter(JsonTokenizerState, Position),
    MultipleValues,
}