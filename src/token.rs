use std::{borrow::Cow, str::Chars};

use crate::utils::{offset_str, CompError, MAX_SAFE_INTEGER, MIN_SAFE_INTEGER};

/// um iterador construido a partir de um `&str` que retorna `Token`
///
/// note que o iterador funciona com referências, ele recebe uma referência para o código e retorna referências para o código, nada nunca é alocado
///
/// exceto erros que são sim alocados em um Vec
#[derive(Clone)]
pub struct TokenIter<'a> {
    code: &'a str,
    remaining: &'a str,
    current: Option<Token<'a>>,
    /// guarda se o ultimo token era o fim de um Statement (space = Space::Line ou text = ";" ou text = "}")
    prev_is_eos: bool,
    errors: Vec<CompError<'a>>,
}

/// uma palavra, numero, ou string do código
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub text: &'a str,
    pub space: TokenSpace,
    pub ty: TokenType,
}

/// indica qual o tipo desse token
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    /// um identifiador como foo ou bar
    Identifier,
    /// uma palavra chave como function ou let
    Keyword,
    /// um caractere como ( ou ,
    Punct,
    /// um valor literal como 1.3
    Number,
    /// um valor literal como ""
    String,
}

/// indica se tem espaço após um token, e se espaço é um espaço normal, ou uma nova linha
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenSpace {
    /// não há espaço após o token
    Joint,
    /// há espaço após o token
    Space,
    /// há uma quebra de linha após o token
    Line,
}

impl<'a> TokenIter<'a> {
    pub fn new(code: &'a str) -> Self {
        let mut remaining = code;
        let mut errors = Vec::new();
        parse_space(&mut remaining, &mut errors);
        let current = parse_token(&mut remaining, &mut errors);
        Self {
            code,
            remaining,
            current,
            prev_is_eos: false,
            errors,
        }
    }
    pub fn peek(&self) -> Option<&Token<'a>> {
        self.current.as_ref()
    }
    /// retorna true e chama next apenas se o token atual for igual a expect
    pub fn consume(&mut self, expect: &str) -> bool {
        if self.text() == expect {
            self.next();
            true
        } else {
            false
        }
    }
    /// retorna true e chama next apenas se o token atual for igual a expect
    pub fn consume_text(&mut self, expect: &str) -> Option<&'a str> {
        if self.text() == expect {
            Some(self.take_text())
        } else {
            None
        }
    }
    /// se o próximo token for adjacente a token e também for um token Punct, e o texto dele for expect
    ///
    /// então retorna Ok(token + expect) e consome o Punct, senão retorna Err(token)
    pub fn join(&mut self, token: &'a str, expect: &str) -> Result<&'a str, &'a str> {
        if let Some(Token {
            text,
            space: _,
            ty: TokenType::Punct,
        }) = self.current
        {
            if text == expect {
                if let Some(joined) = concat_str(self.code, token, text) {
                    self.next();
                    return Ok(joined);
                }
            }
        }
        Err(token)
    }
    /// retorna o texto do token atual, então vai para o próximo token, se todos os tokens já tiverem sido consumidos, retorna uma string vazia
    pub fn take_text(&mut self) -> &'a str {
        match self.next() {
            Some(previous) => previous.text,
            None => &self.remaining[self.remaining.len()..self.remaining.len()],
        }
    }
    /// retorna o texto do token atual, se todos os tokens já tiverem sido consumidos, retorna uma string vazia
    pub fn text(&self) -> &'a str {
        match &self.current {
            Some(current) => current.text,
            None => &self.remaining[self.remaining.len()..self.remaining.len()],
        }
    }
    /// retorna o tipo de spaço do token atual
    pub fn space(&self) -> Option<TokenSpace> {
        self.current.as_ref().map(|x| x.space)
    }
    /// retorna o tipo do token atual
    pub fn ty(&self) -> Option<TokenType> {
        self.current.as_ref().map(|x| x.ty)
    }
    /// retorna true se todos os tokens já foram consumidos
    pub fn eof(&self) -> bool {
        self.current.is_none()
    }
    /// retorna true se estamos no fim de um Statement
    pub fn eos(&self) -> bool {
        self.prev_is_eos || matches!(&self.current, Some(current) if current.text == ";")
    }
    /// caso o token atual for ";", consome ele, senão não faz nada
    ///
    /// isso serve para consumir o ";" no caso de `let a = 2; const b = 3;` e ir para o próximo Statement
    ///
    /// mas não consumir o "let" no caso de `if (...) {...} let a = 2;` que tambem é fim de comando, já que o token anterior é "}"
    ///
    /// ou não consumir o 'const" no caso de
    /// ```ignore
    /// let b = 2
    /// const a = 2
    /// ```
    /// que tambem é fim de comando, já que o token anterior termina com uma nova linha
    pub fn next_eos(&mut self) {
        if matches!(&self.current, Some(current) if current.text == ";") {
            self.next();
        } else if !self.prev_is_eos {
            self.error("Esperado ;");
        }
    }
    /// adiciona um erro a lista interna de erros, e como src, usa o token atual
    pub fn error(&mut self, desc: impl Into<Cow<'static, str>>) {
        self.error_at(self.text(), desc);
    }
    /// adiciona um erro a lista interna de erros, e como src, usa o argumento passado
    pub fn error_at(&mut self, src: &'a str, desc: impl Into<Cow<'static, str>>) {
        let error = CompError {
            src,
            desc: desc.into(),
        };
        #[cfg(debug_assertions)]
        let _ = println!("{:#?}", error);
        self.errors.push(error);
    }
    /// adiciona um erro a lista interna de erros, e como src, usa o token atual
    pub fn errors(&self) -> &Vec<CompError> {
        &self.errors
    }
    /// retorna uma nova string que contém tanto left como right
    ///
    /// os dois tem que ter vindo do slice que este iterador está lendo, se não haverá um panic
    pub fn cover(&self, left: &'a str, right: &'a str) -> &'a str {
        let (left, right) = if left.as_ptr() <= right.as_ptr() {
            (left, right)
        } else {
            (right, left)
        };
        if let Some(cover) = cover_str(self.code, left, right) {
            cover
        } else {
            panic!(
                "TokenIter::cover foi chamado com strings que não estão contidas no código fonte"
            )
        }
    }
    /// retorna todo o código fonte
    pub fn code(&self) -> &'a str {
        self.code
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token<'a>;
    /// retorna o token atualmente selecionado, e seleciona o próximo
    fn next(&mut self) -> Option<Self::Item> {
        let previous = self.current.take();
        self.current = parse_token(&mut self.remaining, &mut self.errors);
        if let Some(previous) = &previous {
            self.prev_is_eos = previous.space == TokenSpace::Line
                || previous.text == ";"
                || previous.text == "}"
                || previous.text == ":";
        } else {
            self.prev_is_eos = false;
        }
        previous
    }
}

pub enum ParsedNumber {
    Integer(i64),
    Float(f64),
}

pub fn parse_number(code: &str) -> ParsedNumber {
    fn parse_int(code: &str) -> i64 {
        let (code, negative) = if code.starts_with('-') {
            (&code[1..], true)
        } else {
            (code, false)
        };
        let value = code
            .bytes()
            .filter(|x| matches!(x, b'0'..=b'9'))
            .map(|x| (x - b'0') as i64)
            .reduce(|a, b| a * 10 + b)
            .unwrap_or(0);
        if negative {
            -value
        } else {
            value
        }
    }
    fn parse_float(code: &str) -> f64 {
        if !code.contains('_') {
            code.parse().unwrap_or(0.0)
        } else {
            let mut temp = String::with_capacity(code.len());
            for char in code.chars() {
                if char != '_' {
                    temp.push(char);
                }
            }
            temp.parse().unwrap_or(0.0)
        }
    }
    if code.is_empty() {
        // houve um erro na tokenização do inteiro, um erro já joi anteriormente emitido
        ParsedNumber::Integer(0)
    } else if code == "0" {
        ParsedNumber::Integer(0)
    } else if code == "0n" {
        todo!("BigInt")
    } else if code.starts_with('0') {
        let (code, is_bigint) = if code.ends_with('n') {
            (&code[0..code.len() - 1], true)
        } else {
            (code, false)
        };
        let bytes = &code.as_bytes()[2..];
        if !is_bigint {
            ParsedNumber::Integer(match code.as_bytes()[1] {
                b'0'..=b'9' => {
                    if !code.as_bytes().iter().any(|x| matches!(x, b'8'..=b'9')) {
                        code.as_bytes()
                            .iter()
                            .filter(|x| matches!(x, b'0'..=b'7'))
                            .map(|x| (*x - b'0') as i64)
                            .reduce(|a, b| a * 0o10 + b)
                            .unwrap_or(0)
                    } else {
                        parse_int(code)
                    }
                }
                b'b' => bytes
                    .iter()
                    .filter(|x| matches!(x, b'0'..=b'1'))
                    .map(|x| (*x - b'0') as i64)
                    .reduce(|a, b| a * 0b10 + b)
                    .unwrap_or(0),
                b'o' => bytes
                    .iter()
                    .filter(|x| matches!(x, b'0'..=b'7'))
                    .map(|x| (*x - b'0') as i64)
                    .reduce(|a, b| a * 0o10 + b)
                    .unwrap_or(0),
                b'x' => bytes
                    .iter()
                    .filter(|x| matches!(x, b'0'..=b'9' | b'A'..=b'F' | b'a'..=b'f'))
                    .map(|x| (*x - b'0') as i64)
                    .reduce(|a, b| a * 0x10 + b)
                    .unwrap_or(0),
                _ => 0,
            })
        } else {
            todo!("BigInt")
        }
    } else if code.ends_with('n') {
        todo!("BigInt")
    } else {
        let (number, mut exponent) =
            if let Some((number, exponent)) = code.split_once(|x| matches!(x, 'e' | 'E')) {
                (number, parse_int(exponent))
            } else {
                (code, 0)
            };
        if exponent < 0 || number.contains('.') {
            let mut acc = parse_float(code);
            while exponent < 0 {
                exponent += 1;
                acc *= 0.1;
            }
            ParsedNumber::Float(acc)
        } else {
            let mut acc_int = parse_int(number);
            while MIN_SAFE_INTEGER <= acc_int && acc_int <= MAX_SAFE_INTEGER {
                if exponent == 0 {
                    return ParsedNumber::Integer(acc_int);
                }
                exponent -= 1;
                acc_int *= 10;
            }
            let mut acc_float = acc_int as f64;
            while exponent > 0 {
                exponent -= 1;
                acc_float *= 10.0;
            }
            ParsedNumber::Float(acc_float)
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedString<'a> {
    remaining: Chars<'a>,
    /// indica que o ultimo caractere foi um que precisa de dois u16 para ser mostrado
    ///
    /// o primeiro código já foi retornado, aqui esta o segundo para ser retornado na proxima chamada de next
    second_half: Option<u16>,
}

pub fn parse_string<'a>(code: &'a str) -> ParsedString<'a> {
    let contents = if code.is_empty() {
        // a string não foi parsada corretamente
        // bota uma string vazia
        code
    } else {
        // remover aspas
        &code[1..code.len() - 1]
    };
    ParsedString {
        remaining: contents.chars(),
        second_half: None,
    }
}

impl<'a> Iterator for ParsedString<'a> {
    type Item = u16;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(second_half) = self.second_half.take() {
            return Some(second_half);
        }
        let character = match self.remaining.next()? {
            '\\' => match self.remaining.next()? {
                digit @ '0'..='3' => {
                    let mut unicode = digit as u32 - b'0' as u32;
                    unicode = unicode * 0x10 + self.remaining.next()? as u32 - b'0' as u32;
                    unicode = unicode * 0x10 + self.remaining.next()? as u32 - b'0' as u32;
                    unicode.try_into().expect("esse escape inválido deveria ter sido eliminado durante a analise da sintaxe")
                }
                digit @ '4'..='7' => {
                    let mut unicode = digit as u32 - b'0' as u32;
                    unicode = unicode * 0x10 + self.remaining.next()? as u32 - b'0' as u32;
                    unicode.try_into().expect("esse escape inválido deveria ter sido eliminado durante a analise da sintaxe")
                }
                'x' => {
                    let mut unicode = hex_char_to_int(self.remaining.next()?) as u32;
                    unicode = unicode * 0x10 + hex_char_to_int(self.remaining.next()?) as u32;
                    unicode.try_into().expect("esse escape inválido deveria ter sido eliminado durante a analise da sintaxe")
                }
                'u' => {
                    let mut unicode: u32 = 0;
                    let first = self.remaining.next()?;
                    if first == '{' {
                        loop {
                            let digit = self.remaining.next()?;
                            if digit == '}' {
                                break;
                            }
                            unicode = unicode * 0x10 + hex_char_to_int(digit) as u32;
                        }
                    } else {
                        for _ in 0..4 {
                            unicode =
                                unicode * 0x10 + hex_char_to_int(self.remaining.next()?) as u32;
                        }
                    }
                    unicode.try_into().expect("esse escape inválido deveria ter sido eliminado durante a analise da sintaxe")
                }
                'b' => 8 as char,
                't' => 9 as char,
                'n' => 10 as char,
                'v' => 11 as char,
                'f' => 12 as char,
                'r' => 13 as char,
                escape => escape,
            },
            character => character,
        };
        if (character as u32) < 0x10000 {
            return Some(character as u16);
        }
        let high_half = 0xD800 + ((character as u32) / 0x10000) as u16;
        let low_half = 0xDC00 + ((character as u32) % 0x10000) as u16;
        self.second_half = Some(low_half);
        Some(high_half)
    }
}

/// obtem o token no começo de code,
/// o começo de code é movido para o começo do próximo token (se não hover, code ficará vazio),
/// e o token obtido é retornado
///
/// se code for vazio, retorna None
fn parse_token<'a>(code: &mut &'a str, errors: &mut Vec<CompError<'a>>) -> Option<Token<'a>> {
    if code.is_empty() {
        return None;
    }
    let bytes = code.as_bytes();
    match bytes[0] {
        b' ' | b'\t' | b'\n' | b'\r' => {
            panic!("parse_next foi chamado e code começava com espaço em branco")
        }
        0..=31 | b'#' | b'@' | b'`' | b'\x7f' => {
            errors.push(CompError::new(
                parse_slice(code, 1),
                "caractere inválido".into(),
            ));
            parse_token(code, errors)
        }
        b'"' | b'\'' => Some(parse_token_string(code, errors)),
        b'0'..=b'9' => Some(parse_token_number(code, errors)),
        b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'$' | 128.. => Some(parse_token_word(code, errors)),
        b'.' => {
            if bytes.len() >= 2 && matches!(bytes[1], b'0'..=b'9') {
                Some(parse_token_number(code, errors))
            } else {
                Some(parse_token_punct(code, errors))
            }
        }
        b'!' | b'%' | b'&' | b'(' | b')' | b'*' | b'+' | b',' | b'-' | b'/' | b':' | b';'
        | b'<' | b'=' | b'>' | b'?' | b'[' | b'\\' | b']' | b'^' | b'{' | b'|' | b'}' | b'~' => {
            Some(parse_token_punct(code, errors))
        }
    }
}

fn parse_token_number<'a>(code: &mut &'a str, errors: &mut Vec<CompError<'a>>) -> Token<'a> {
    /// returna true se text contem apneas digitos ascii ou _, e não começa nem termina com _
    fn validate(text: &str) -> bool {
        if text.starts_with('_') || text.ends_with('_') {
            return false;
        }
        for byte in text.as_bytes() {
            if !matches!(*byte, b'0'..=b'9' | b'_') {
                return false;
            }
        }
        true
    }
    let text = parse_slice(
        code,
        find(code, |x| !(x == b'.' || x == b'-' || is_word_byte(x))),
    );
    let space = parse_space(code, errors);
    let valid = if text == "0" || text == "0n" {
        true
    } else if text.starts_with('0') {
        let (text, is_bigint) = if text.ends_with('n') {
            (&text[0..text.len() - 1], true)
        } else {
            (text, false)
        };
        let bytes = &text.as_bytes()[2..];
        let mut iter = bytes.iter();
        let valid = !bytes.is_empty()
            && bytes.first() != Some(&b'_')
            && bytes.last() != Some(&b'_')
            && match text.as_bytes()[1] {
                b'0'..=b'9' => !is_bigint && iter.all(|x| matches!(x, b'0'..=b'9')),
                b'b' => iter.all(|x| matches!(x, b'0'..=b'1')),
                b'o' => iter.all(|x| matches!(x, b'0'..=b'7')),
                b'x' => iter.all(|x| matches!(x, b'0'..=b'9' | b'A'..=b'F' | b'a'..=b'f')),
                _ => false,
            };
        valid
    } else if text.ends_with('n') {
        validate(&text[0..text.len() - 1])
    } else {
        let (number, exponent) =
            if let Some((number, mut exponent)) = text.split_once(|x| matches!(x, 'e' | 'E')) {
                if exponent.starts_with('-') {
                    exponent = &exponent[1..];
                }
                (number, Some(exponent))
            } else {
                (text, None)
            };
        let (integer, fraction) = if let Some((integer, fraction)) = number.split_once('.') {
            (integer, Some(fraction))
        } else {
            (number, None)
        };
        validate(integer)
            && fraction.map(validate).unwrap_or(true)
            && exponent.map(validate).unwrap_or(true)
    };
    if !valid {
        errors.push(CompError::new(text, "literal numérico inválido".into()));
        return Token {
            text: &text[0..0],
            space,
            ty: TokenType::Number,
        };
    }
    return Token {
        text,
        space,
        ty: TokenType::Number,
    };
}

fn parse_token_string<'a>(code: &mut &'a str, errors: &mut Vec<CompError<'a>>) -> Token<'a> {
    let delimiter = code.as_bytes()[0];
    let bytes = code.as_bytes();
    let mut has_errors = false;
    let mut iter = 1..bytes.len();
    'outer: while let Some(index) = iter.next() {
        match bytes[index] {
            b'\\' => {
                let Some(index) = iter.next() else {
                    break;
                };
                match bytes[index] {
                    b'0'..=b'3' => {
                        // obter até mais 2 caracteres octais
                        for offset in 1..=2 {
                            if index + offset < bytes.len()
                                && matches!(bytes[index + offset], b'0'..=b'7')
                            {
                                iter.next();
                            } else {
                                break;
                            }
                        }
                    }
                    b'4'..=b'7' => {
                        // obter até mais 1 caracteres octais
                        if index + 1 < bytes.len() && matches!(bytes[index + 1], b'0'..=b'7') {
                            iter.next();
                        }
                    }
                    b'u' => {
                        if index + 1 < bytes.len() && bytes[index + 1] == b'{' {
                            iter.next();
                            const LARGEST_UNICODE_CODE_POINT: u32 = 0x10FFFF;
                            let mut unicode = 0;
                            let mut is_empty = true;
                            // obter de 1 a 6 caracteres hexadecimais
                            loop {
                                let Some(index) = iter.next() else {
                                    break 'outer;
                                };
                                if matches!(bytes[index], b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F') {
                                    is_empty = false;
                                    if unicode != u32::MAX {
                                        unicode =
                                            unicode * 0x10 + hex_byte_to_int(bytes[index]) as u32;
                                        if unicode > LARGEST_UNICODE_CODE_POINT {
                                            unicode = u32::MAX;
                                        }
                                    }
                                } else if bytes[index] == b'}' {
                                    if is_empty {
                                        has_errors = true;
                                        errors.push(CompError::new(
                                            &code[index..index + 1],
                                            "esperado caractere hexadecimal".into(),
                                        ));
                                    }
                                    break;
                                } else {
                                    has_errors = true;
                                    errors.push(CompError::new(
                                        &code[index..index + 1],
                                        "esperado caractere hexadecimal".into(),
                                    ));
                                    break;
                                }
                            }
                            if unicode == u32::MAX || TryInto::<char>::try_into(unicode).is_err() {
                                has_errors = true;
                                errors.push(CompError::new(
                                    &code[index..index + 1],
                                    "código unicode inválido".into(),
                                ));
                            }
                        } else {
                            // obter exatamente 4 caracteres hexadecimais
                            for _ in 0..4 {
                                let Some(index) = iter.next() else {
                                    break 'outer;
                                };
                                if !matches!(bytes[index], b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F')
                                {
                                    has_errors = true;
                                    errors.push(CompError::new(
                                        &code[index..index + 1],
                                        "esperado caractere hexadecimal".into(),
                                    ))
                                }
                            }
                        }
                    }
                    b'x' => {
                        // obter exatamente 2 caracteres hexadecimais
                        for _ in 0..2 {
                            let Some(index) = iter.next() else {
                                break 'outer;
                            };
                            if !matches!(bytes[index], b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F') {
                                has_errors = true;
                                errors.push(CompError::new(
                                    &code[index..index + 1],
                                    "esperado caractere hexadecimal".into(),
                                ))
                            }
                        }
                    }
                    0..=31 | 127 => {
                        has_errors = true;
                        errors.push(CompError::new(
                            &code[index..index + 1],
                            "caractere invalido".into(),
                        ))
                    }
                    _ => {} // os outros caracteres, ou são escapes inválidos,
                            // ou são válidos e ocupam apenas um caractere e não requerem validação
                }
            }
            b'\t' | 32..=126 | 128.. => {
                if bytes[index] == delimiter {
                    let text = if has_errors {
                        &parse_slice(code, index + 1)[0..0]
                    } else {
                        parse_slice(code, index + 1)
                    };
                    return Token {
                        text,
                        space: parse_space(code, errors),
                        ty: TokenType::String,
                    };
                }
            }
            b'\r' | b'\n' => {
                has_errors = true;
                errors.push(CompError::new(
                    &code[index - 1..index],
                    "string não terminada".into(),
                ))
            }
            0..=31 | 127 => {
                has_errors = true;
                errors.push(CompError::new(
                    &code[index..index + 1],
                    "caractere invalido".into(),
                ))
            }
        }
    }
    errors.push(CompError::new(
        &code[code.len() - 1..],
        "string não terminada".into(),
    ));
    Token {
        text: &parse_slice(code, code.len())[0..0],
        space: parse_space(code, errors),
        ty: TokenType::String,
    }
}

fn parse_token_word<'a>(code: &mut &'a str, errors: &mut Vec<CompError<'a>>) -> Token<'a> {
    const KEYWORDS: [&str; 41] = [
        "do",
        "if",
        "in",
        "for",
        "let",
        "new",
        "try",
        "var",
        "case",
        "else",
        "enum",
        "eval",
        "null",
        "this",
        "true",
        "void",
        "with",
        "await",
        "break",
        "catch",
        "class",
        "const",
        "false",
        "super",
        "throw",
        "while",
        "yield",
        "delete",
        "export",
        "import",
        "return",
        "switch",
        "typeof",
        "default",
        "extends",
        "finally",
        "continue",
        "debugger",
        "function",
        "arguments",
        "instanceof",
    ];
    let text = parse_slice(code, find(code, |x| !is_word_byte(x)));
    let space = parse_space(code, errors);
    let ty = if KEYWORDS.iter().any(|keyword| *keyword == text) {
        TokenType::Keyword
    } else {
        TokenType::Identifier
    };
    Token { text, space, ty }
}

fn parse_token_punct<'a>(code: &mut &'a str, errors: &mut Vec<CompError<'a>>) -> Token<'a> {
    let text = parse_slice(code, 1);
    let space = parse_space(code, errors);
    Token {
        text,
        space,
        ty: TokenType::Punct,
    }
}

/// remove o espaço, cometários de linha e comentários de bloco no começo de code,
///
/// se code é vazio retorna Line
///
/// se nada foi removido retorna Joint,
/// se no espaço removido havia '\n' ou '\r' retorna Space::Line,
/// senão retorna Space::Space
fn parse_space<'a>(code: &mut &'a str, errors: &mut Vec<CompError<'a>>) -> TokenSpace {
    if code.is_empty() {
        return TokenSpace::Line;
    }
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Comment {
        None,
        Line,
        Block,
    }
    let bytes = code.as_bytes();
    let mut found_new_line = false;
    let mut comment = Comment::None;
    for index in 0..bytes.len() {
        match bytes[index] {
            b' ' | b'\t' => {}
            // todo!() 0x2028 (0xE2 0x80 0xA8) e 0x2029 (0xE2 0x80 0xA9) também são quebras de linha
            b'\n' | b'\r' => {
                if comment == Comment::Line {
                    comment = Comment::None;
                }
                found_new_line = true;
            }
            // começo de comentário de linha
            b'/' if comment == Comment::None
                && index + 1 < bytes.len()
                && bytes[index + 1] == b'/' =>
            {
                comment = Comment::Line
            }
            // começo de comentário de bloco
            b'/' if comment == Comment::None
                && index + 1 < bytes.len()
                && bytes[index + 1] == b'*' =>
            {
                comment = Comment::Block
            }
            // fim de comentário de bloco
            b'/' if comment == Comment::Block && index > 0 && bytes[index - 1] == b'*' => {
                comment = Comment::None
            }
            _ if comment == Comment::None => {
                // move o começo de code em index caracteres
                parse_slice(code, index);
                // um caractere não branco foi encontrado
                if index == 0 {
                    // ele é o primeiro significa que nenhum espaço foi encontrado
                    return TokenSpace::Joint;
                } else if found_new_line {
                    return TokenSpace::Line;
                } else {
                    return TokenSpace::Space;
                }
            }
            _ => {}
        }
    }
    parse_slice(code, code.len());
    if comment == Comment::Block {
        errors.push(CompError {
            src: code,
            desc: "Esperado */".into(),
        })
    }
    if found_new_line {
        TokenSpace::Line
    } else {
        TokenSpace::Space
    }
}

/// remove os primeiros len bytes de code e retorna eles
fn parse_slice<'a>(code: &mut &'a str, len: usize) -> &'a str {
    let slice = &code[0..len];
    *code = &code[len..];
    slice
}

/// retorna o index do primeiro byte que quando chamada com predicate, ele retorna true
fn find(code: &str, mut predicate: impl FnMut(u8) -> bool) -> usize {
    let bytes = code.as_bytes();
    for index in 0..bytes.len() {
        if predicate(bytes[index]) {
            return index;
        }
    }
    return bytes.len();
}

/// retorna true se o byte pode fazer de um identificador, palavra-chave ou literal numérico (exceto ".")
fn is_word_byte(byte: u8) -> bool {
    matches!(byte, b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'$' | 128..)
}

#[cfg(test)]
mod tests {
    use super::TokenSpace::*;
    use super::TokenType::*;
    use super::*;
    impl<'a> TokenIter<'a> {
        fn test_next(&mut self, text: &str, space: super::TokenSpace, ty: TokenType) {
            assert_eq!(self.next(), Some(Token { text, space, ty }));
        }
    }
    #[test]
    fn teste_geral_ok() {
        let mut iter =
            TokenIter::new("function add(a, b) {\nreturn a + b;\n}\nconsole.log(add(1, 2));");
        iter.test_next("function", Space, Keyword);
        iter.test_next("add", Joint, Identifier);
        iter.test_next("(", Joint, Punct);
        iter.test_next("a", Joint, Identifier);
        iter.test_next(",", Space, Punct);
        iter.test_next("b", Joint, Identifier);
        iter.test_next(")", Space, Punct);
        iter.test_next("{", Line, Punct);
        iter.test_next("return", Space, Keyword);
        iter.test_next("a", Space, Identifier);
        iter.test_next("+", Space, Punct);
        iter.test_next("b", Joint, Identifier);
        iter.test_next(";", Line, Punct);
        iter.test_next("}", Line, Punct);
        iter.test_next("console", Joint, Identifier);
        iter.test_next(".", Joint, Punct);
        iter.test_next("log", Joint, Identifier);
        iter.test_next("(", Joint, Punct);
        iter.test_next("add", Joint, Identifier);
        iter.test_next("(", Joint, Punct);
        iter.test_next("1", Joint, Number);
        iter.test_next(",", Space, Punct);
        iter.test_next("2", Joint, Number);
        iter.test_next(")", Joint, Punct);
        iter.test_next(")", Joint, Punct);
        iter.test_next(";", Line, Punct);
        assert_eq!(iter.next(), None);
    }
    #[test]
    fn teste_literais() {
        assert_valid(Number, "1");
        assert_valid(Number, "1.1");
        assert_valid(Number, "1.");
        assert_valid(Number, ".1");
        assert_valid(Number, "1_1");
        assert_valid(Number, "1_1.1_1");
        assert_valid(Number, "1_1.");
        assert_valid(Number, ".1_1");
        assert_valid(Number, "1e1");
        assert_valid(Number, "1.1e-1");
        assert_valid(Number, "1.E1");
        assert_valid(Number, ".1E-1");
        assert_valid(Number, "1_1e1");
        assert_valid(Number, "1_1.1_1e-1");
        assert_valid(Number, "1_1.E1");
        assert_valid(Number, ".1_1E-1");
        assert_valid(Number, "0b011010");
        assert_valid(Number, "0o013716");
        assert_valid(Number, "0x23FAcD");

        assert_valid(String, "\"\\0a\"");
        assert_valid(String, "\"\\01a\"");
        assert_valid(String, "\"\\012a\"");
        assert_valid(String, "'\\xFF'");
        assert_valid(String, "'\\uABCD'");
        assert_valid(String, "\"\\u{0}\"");
        assert_valid(String, "\"\\u{10FFFF}\"");
        assert_valid(String, "\"\\u{000000}\"");
        assert_valid(String, "\"\\u{00000000}\"");

        assert_invalid(Number, "1_");
        assert_invalid(Number, "1_.1");
        assert_invalid(Number, "1_.");
        assert_invalid(Number, ".1_");
        assert_invalid(Number, "1_e1");
        assert_invalid(Number, "1.1e_-1");
        assert_invalid(Number, "1.E1_");
        assert_invalid(Number, ".1_E-1");
        assert_invalid(Number, "1_1e_1");
        assert_invalid(Number, "1_1.1_1e-1_");
        assert_invalid(Number, "1_1.E1_");
        assert_invalid(Number, ".1_1_E-1");
        assert_invalid(Number, "0b");
        assert_invalid(Number, "0o");
        assert_invalid(Number, "0x");
        assert_invalid(Number, "0b2");
        assert_invalid(Number, "0o8");
        assert_invalid(Number, "0xH");

        assert_invalid(String, "\"\\0a");
        assert_invalid(String, "\"\\x");
        assert_invalid(String, "\"\\x\"");
        assert_invalid(String, "\"\\xA");
        assert_invalid(String, "\"\\xAH");
        assert_invalid(String, "\"\\xA\"");
        assert_invalid(String, "\"\\u");
        assert_invalid(String, "\"\\uH");
        assert_invalid(String, "\"\\uA");
        assert_invalid(String, "\"\\uAA");
        assert_invalid(String, "\"\\uAAA");
        assert_invalid(String, "\"\\uAAAG");
        assert_invalid(String, "\"\\u{");
        assert_invalid(String, "\"\\u{}");
        assert_invalid(String, "\"\\u{G");
        assert_invalid(String, "\"\\u{\"");
        assert_invalid(String, "\"\\u{}\"");
        assert_invalid(String, "\"\\u{110000}\"");
        assert_invalid(String, "\"\\u{0000000}\"");
        assert_invalid(String, "\"\\u{\"");
        assert_invalid(String, "\"\\u{G\"");
    }
    fn assert_valid(ty: super::TokenType, text: &str) {
        let mut iter = TokenIter::new(text);
        assert_eq!(
            iter.next(),
            Some(Token {
                text,
                space: Line,
                ty
            }),
            "token não foi parsado corretamente token: \"{text}\""
        );
        assert_eq!(iter.next(), None);
        if !iter.errors.is_empty() {
            panic!(
                "Houveram erros ao tentar parsar ({text}), que deveria ser correta, erros: {:#?}",
                iter.errors
            );
        }
    }
    fn assert_invalid(ty: super::TokenType, text: &str) {
        let mut iter = TokenIter::new(text);
        assert!(
            iter.next().map(|x| x.ty == ty).unwrap_or(false),
            "token não foi parsado corretamente token: \"{text}\""
        );
        assert_eq!(iter.next(), None);
        if iter.errors.is_empty() {
            panic!("Não houveram erros ao tentar parsar ({text}), que deveria dar um erro");
        }
    }
}

/// cria um novo &str que representa todo o left e right, juntos
///
/// se left e right não estão adjacentes na memória, retorna None
fn concat_str<'a>(all: &'a str, left: &'a str, right: &'a str) -> Option<&'a str> {
    let left_index = offset_str(all, left)?;
    let right_index = offset_str(all, right)?;
    if left_index + left.len() == right_index {
        Some(&all[left_index..right_index + right.len()])
    } else {
        None
    }
}

/// cria um novo &str que começa onde o left começa, e termina onde o right termina
///
/// no entanto, se left conter right, ou seja right se encontrar dentro de left, left será retornado
///
/// isso siginifica que a string final sempre será igual ou maior que a soma das outras duas
///
/// mas é garantido que a string retornada sempre existe dentro de all
fn cover_str<'a>(all: &'a str, left: &'a str, right: &'a str) -> Option<&'a str> {
    let left_index = offset_str(all, left)?;
    let right_index = offset_str(all, right)?;
    if left_index > right_index {
        None
    } else if left_index + left.len() >= right_index + right.len() {
        // o fim de left está alem do fim de right, left contem right, retorna left
        Some(left)
    } else {
        Some(&all[left_index..right_index + right.len()])
    }
}

fn hex_byte_to_int(digit: u8) -> u8 {
    match digit {
        b'0'..=b'9' => digit - b'0',
        b'a'..=b'f' => digit - b'a' + 10,
        b'A'..=b'F' => digit - b'A' + 10,
        _ => panic!("hex_byte_to_int foi chamado com um digito que não é um hex"),
    }
}

fn hex_char_to_int(digit: char) -> u8 {
    match digit {
        '0'..='9' => digit as u8 - b'0',
        'a'..='f' => digit as u8 - b'a' + 10,
        'A'..='F' => digit as u8 - b'A' + 10,
        _ => panic!("hex_char_to_int foi chamado com um digito que não é um hex"),
    }
}
