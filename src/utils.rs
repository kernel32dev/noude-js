
use std::borrow::Cow;

pub const MAX_SAFE_INTEGER: i64 = (1 << 53) - 1;
pub const MIN_SAFE_INTEGER: i64 = -((1 << 53) - 1);

/// um erro de compilção do js
#[derive(Debug, Clone)]
pub struct CompError<'a> {
    /// a fonte do erro, uma referência ao código
    pub src: &'a str,
    /// a descrição do erro, como muitos errps tem mensagens fixas, usamos um Cow
    pub desc: Cow<'static, str>,
}

impl<'a> CompError<'a> {
    pub fn new(src: &'a str, desc: Cow<'static, str>) -> Self {
        Self { src, desc }
    }
}

/// se substr estiver contido dentro de all, retorna o index de substr dentro de all
///
/// ```ignore
/// let all = " ABC ABC ";
/// let substr = &all[5..8];
/// 
/// assert_eq!(substr, "ABC");
/// 
/// let offset = offset_from(all, substr).unwrap();
///
/// assert_eq!(offset, 5);
/// 
/// let substr_2 = &all[offset..offset + substr.len()];
/// 
/// assert_eq!(substr, substr_2);
/// 
/// let other_str = "somewhere else";
/// 
/// let offset = offset_from(all, other_str);
///
/// assert_eq!(offset, None);
/// ```
pub fn offset_str<'a>(all: &'a str, substr: &'a str) -> Option<usize> {
    unsafe {
        let index = substr.as_ptr().offset_from(all.as_ptr()).try_into().ok()?;
        if index + substr.len() <= all.len() {
            Some(index)
        } else {
            None
        }
    }
}

pub const fn is_space(char: u32) -> bool {
    match char {
        0x09 |
        0x0B |
        0x0C |
        0x20 |
        0x00A0 |
        0x1680 |
        0x2000..=0x200A |
        0x202F |
        0x205F |
        0x3000 |
        0xFEFF => true,
        _ => false,
    }
}
