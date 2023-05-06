
use std::borrow::Cow;

/// um erro de compilção do js
#[derive(Debug, Clone)]
pub struct CompError<'a> {
    /// a fonte do erro, uma referência ao código
    pub src: Option<&'a str>,
    /// a descrição do erro, como muitos errps tem mensagens fixas, usamos um Cow
    pub desc: Cow<'static, str>,
}

impl<'a> CompError<'a> {
    pub fn new(desc: Cow<'static, str>, src: Option<&'a str>) -> Self {
        Self { src, desc }
    }
}
