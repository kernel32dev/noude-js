/// um iterador construido a partir de um `&str` que retorna `Token`
#[derive(Clone)]
pub struct TokenIter<'a> {
    remaining: &'a str,
    current: Option<Token<'a>>,
}

/// uma palavra, numero, ou string do código
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub text: &'a str,
    pub space: Space,
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
    /// um valor literal como "" ou 1.3
    Literal(TokenLiteralType),
}

/// indica que tipo de literal esse token é
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenLiteralType {
    /// um numero no formato 1, 0b1, 0o7, 0xF, 1.2, .2, 1., 3.5e1, .7e-2, 8.e0,
    Number,
    /// uma string "", ou '', ou ``
    String,
}

/// indica se tem espaço após um token, e se espaço é um espaço normal, ou uma nova linha
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Space {
    /// não há espaço após o token
    Joint,
    /// há espaço após o token
    Space,
    /// há uma quebra de linha após o token
    Line,
}

impl<'a> TokenIter<'a> {
    pub fn new(mut code: &'a str) -> Self {
        parse_space(&mut code);
        let current = parse_token(&mut code);
        Self {
            remaining: code,
            current,
        }
    }
    pub fn peek(&self) -> Option<&Token<'a>> {
        self.current.as_ref()
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token<'a>;
    /// retorna o token atualmente selecionado, e seleciona o próximo
    fn next(&mut self) -> Option<Self::Item> {
        let previous = self.current.take();
        self.current = parse_token(&mut self.remaining);
        previous
    }
}

/// obtem o token no começo de code,
/// o começo de code é movido para o começo do próximo token (se não hover, code ficará vazio),
/// e o token obtido é retornado
///
/// se code for vazio, retorna None
fn parse_token<'a>(code: &mut &'a str) -> Option<Token<'a>> {
    if code.is_empty() {
        return None;
    }
    let bytes = code.as_bytes();
    match bytes[0] {
        b' ' | b'\t' | b'\n' | b'\r' => {
            panic!("parse_next foi chamado e code começava com espaço em branco")
        }
        0..=31 | b'#' | b'\x7f' => todo!("Retornar erro: caractere inválido"),
        b'"' | b'\'' | b'`' => Some(parse_token_string(code)),
        b'0'..=b'9' => Some(parse_token_number(code)),
        b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'$' | 128.. => Some(parse_token_word(code)),
        b'.' => {
            if bytes.len() >= 2 && matches!(bytes[1], b'0'..=b'9') {
                Some(parse_token_number(code))
            } else {
                Some(parse_token_punct(code))
            }
        }
        b'!' | b'%' | b'&' | b'(' | b')' | b'*' | b'+' | b',' | b'-' | b'/' | b':' | b';'
        | b'<' | b'=' | b'>' | b'?' | b'@' | b'[' | b'\\' | b']' | b'^' | b'{' | b'|' | b'}'
        | b'~' => Some(parse_token_punct(code)),
    }
}

fn parse_token_number<'a>(code: &mut &'a str) -> Token<'a> {
    /// returna true se text contem apneas digitos ascii ou _, e não começa nem termina com _
    fn validate(text: &str) -> bool {
        if !text.starts_with('_') && !text.ends_with('_') {
            for byte in text.as_bytes() {
                if !matches!(*byte, b'0'..=b'9' | b'_') {
                    return false;
                }
            }
        }
        true
    }
    let text = parse_slice(
        code,
        find(code, |x| !(x == b'.' || x == b'-' || is_word_byte(x))),
    );
    let number = if let Some((number, exponent)) = text.split_once(|x| matches!(x, 'e' | 'E')) {
        let exponent_valid = if exponent.starts_with('-') {
            validate(&exponent[1..])
        } else {
            validate(exponent)
        };
        if !exponent_valid {
            todo!("Retornar erro: literal numérico inválido");
        }
        number
    } else {
        text
    };
    let number_valid = if let Some((integer, fraction)) = number.split_once('.') {
        validate(integer) && validate(fraction)
    } else {
        validate(number)
    };
    if !number_valid {
        todo!("Retornar erro: literal numérico inválido");
    }
    let space = parse_space(code);
    Token {
        text,
        space,
        ty: TokenType::Literal(TokenLiteralType::Number),
    }
}

fn parse_token_string<'a>(code: &mut &'a str) -> Token<'a> {
    let delimiter = code.as_bytes()[0];
    let bytes = code.as_bytes();
    let mut iter = 1..bytes.len();
    'outer: while let Some(index) = iter.next() {
        match bytes[index] {
            b'\\' => {
                let Some(index) = iter.next() else {
                    break;
                };
                match bytes[index] {
                    b'0'..=b'7' => {
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
                    b'u' => {
                        // obter exatamente 4 caracteres hexadecimais
                        for _ in 0..4 {
                            let Some(index) = iter.next() else {
                                break 'outer;
                            };
                            if !matches!(bytes[index], b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F') {
                                todo!("Retornar erro: esperado caractere hexadecimal")
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
                                todo!("Retornar erro: esperado caractere hexadecimal")
                            }
                        }
                    }
                    0..=31 | 127 => todo!("Retornar erro: caractere invalido"),
                    _ => {} // todos os outros caracteres, ou não são escapes inválidos,
                            // ou são válidos e ocupam apenas um caractere e não requerem validação
                }
            }
            b'\t' | 32..=126 | 128.. => {
                if bytes[index] == delimiter {
                    return Token {
                        text: parse_slice(code, index + 1),
                        space: parse_space(code),
                        ty: TokenType::Literal(TokenLiteralType::String),
                    };
                }
            }
            b'\r' | b'\n' => todo!("Retornar erro: string não terminada"),
            0..=31 | 127 => todo!("Retornar erro: caractere invalido"),
        }
    }
    todo!("Retornar erro: string não terminada")
}

fn parse_token_word<'a>(code: &mut &'a str) -> Token<'a> {
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
    let space = parse_space(code);
    let ty = if KEYWORDS.iter().any(|keyword| *keyword == text) {
        TokenType::Keyword
    } else {
        TokenType::Identifier
    };
    Token { text, space, ty }
}

fn parse_token_punct<'a>(code: &mut &'a str) -> Token<'a> {
    let text = parse_slice(code, 1);
    let space = parse_space(code);
    Token {
        text,
        space,
        ty: TokenType::Punct,
    }
}

/// remove o espaço no começo de code,
///
/// se code é vazio retorna Space
///
/// se nada foi removido retorna Joint,
/// se no espaço removido havia '\n' ou '\r' retorna Space::Line,
/// senão retorna Space::Space
fn parse_space(code: &mut &str) -> Space {
    if code.is_empty() {
        return Space::Space;
    }
    let bytes = code.as_bytes();
    let mut found_new_line = false;
    for index in 0..bytes.len() {
        match bytes[index] {
            b' ' | b'\t' => {}
            b'\n' | b'\r' => {
                found_new_line = true;
            }
            _ => {
                // move o começo de code em index caracteres
                parse_slice(code, index);
                // um caractere não branco foi encontrado
                if index == 0 {
                    // ele é o primeiro significa que nenhum espaço foi encontrado
                    return Space::Joint;
                } else if found_new_line {
                    return Space::Line;
                } else {
                    return Space::Space;
                }
            }
        }
    }
    if found_new_line {
        Space::Line
    } else {
        Space::Space
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
    use super::*;
    use super::Space::*;
    use super::TokenLiteralType::*;
    use super::TokenType::*;
    #[test]
    fn teste_geral_ok() {
        let mut iter = TokenIter::new("function add(a, b) {\nreturn a + b;\n}\nconsole.log(add(1, 2));");
        assert_eq!(iter.next(), Some(Token {text: "function", space: Space, ty: Keyword }));
        assert_eq!(iter.next(), Some(Token {text: "add", space: Joint, ty: Identifier }));
        assert_eq!(iter.next(), Some(Token {text: "(", space: Joint, ty: Punct }));
        assert_eq!(iter.next(), Some(Token {text: "a", space: Joint, ty: Identifier }));
        assert_eq!(iter.next(), Some(Token {text: ",", space: Space, ty: Punct }));
        assert_eq!(iter.next(), Some(Token {text: "b", space: Joint, ty: Identifier }));
        assert_eq!(iter.next(), Some(Token {text: ")", space: Space, ty: Punct }));
        assert_eq!(iter.next(), Some(Token {text: "{", space: Line, ty: Punct }));
        assert_eq!(iter.next(), Some(Token {text: "return", space: Space, ty: Keyword }));
        assert_eq!(iter.next(), Some(Token {text: "a", space: Space, ty: Identifier }));
        assert_eq!(iter.next(), Some(Token {text: "+", space: Space, ty: Punct }));
        assert_eq!(iter.next(), Some(Token {text: "b", space: Joint, ty: Identifier }));
        assert_eq!(iter.next(), Some(Token {text: ";", space: Line, ty: Punct }));
        assert_eq!(iter.next(), Some(Token {text: "}", space: Line, ty: Punct }));
        assert_eq!(iter.next(), Some(Token {text: "console", space: Joint, ty: Identifier }));
        assert_eq!(iter.next(), Some(Token {text: ".", space: Joint, ty: Punct }));
        assert_eq!(iter.next(), Some(Token {text: "log", space: Joint, ty: Identifier }));
        assert_eq!(iter.next(), Some(Token {text: "(", space: Joint, ty: Punct }));
        assert_eq!(iter.next(), Some(Token {text: "add", space: Joint, ty: Identifier }));
        assert_eq!(iter.next(), Some(Token {text: "(", space: Joint, ty: Punct }));
        assert_eq!(iter.next(), Some(Token {text: "1", space: Joint, ty: Literal(Number) }));
        assert_eq!(iter.next(), Some(Token {text: ",", space: Space, ty: Punct }));
        assert_eq!(iter.next(), Some(Token {text: "2", space: Joint, ty: Literal(Number) }));
        assert_eq!(iter.next(), Some(Token {text: ")", space: Joint, ty: Punct }));
        assert_eq!(iter.next(), Some(Token {text: ")", space: Joint, ty: Punct }));
        assert_eq!(iter.next(),
            Some(Token {
                text: ";",
                space: Space,
                ty: Punct
            })
        );
        assert_eq!(iter.next(), None);
    }
    #[test]
    fn teste_literais_ok() {
        assert_valid_type(Literal(Number), "1");
        assert_valid_type(Literal(Number), "1.1");
        assert_valid_type(Literal(Number), "1.");
        assert_valid_type(Literal(Number), ".1");
        assert_valid_type(Literal(Number), "1_1");
        assert_valid_type(Literal(Number), "1_1.1_1");
        assert_valid_type(Literal(Number), "1_1.");
        assert_valid_type(Literal(Number), ".1_1");
        assert_valid_type(Literal(Number), "1e1");
        assert_valid_type(Literal(Number), "1.1e-1");
        assert_valid_type(Literal(Number), "1.E1");
        assert_valid_type(Literal(Number), ".1E-1");
        assert_valid_type(Literal(Number), "1_1e1");
        assert_valid_type(Literal(Number), "1_1.1_1e-1");
        assert_valid_type(Literal(Number), "1_1.E1");
        assert_valid_type(Literal(Number), ".1_1E-1");
        assert_valid_type(Literal(String), "\"\\0a\"");
        assert_valid_type(Literal(String), "\"\\01a\"");
        assert_valid_type(Literal(String), "\"\\012a\"");
        assert_valid_type(Literal(String), "'\\xFF'");
        assert_valid_type(Literal(String), "`\\uABCD`");
    }
    fn assert_valid_type(ty: super::TokenType, text: &str) {
        let mut iter = TokenIter::new(text);
        assert_eq!(
            iter.next(),
            Some(Token {
                text,
                space: Space,
                ty
            }),
            "token não foi analizado corretamente token: \"{text}\""
        );
        assert_eq!(iter.next(), None);
    }
}
