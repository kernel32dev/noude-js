use core::panic;
use std::borrow::Cow;
use std::ops::Deref;

use crate::token::{Token, TokenIter, TokenSpace, TokenType};

/// TODO: function* yield yield* import class super

/// um alias para str, quando usado, indica que esse valor está aqui apenas para podermos encontrar sua fonte no código
///
/// caso o valor tenha algum uso fora isso, use str
#[allow(non_camel_case_types)]
pub type src = str;

/// uma lista de Statement, na raiz do arquivo
#[derive(Debug, Clone)]
pub struct Program<'a>(pub Vec<Statement<'a>>);

/// uma lista de Statement, representa código dentro de chaves
#[derive(Debug, Clone)]
pub struct Statements<'a>(pub Vec<Statement<'a>>);

/// uma "linha" de código, por assim dizer, é uma parte do código que não retorna um valor
///
/// mais especificamente, qualquer das variantes abaixo
#[derive(Debug, Clone)]
pub enum Statement<'a> {
    /// function name(args = expr, ...) {}
    Function {
        is_async: Option<&'a src>,
        name: &'a str,
        args: Patterns<'a>,
        body: Statements<'a>,
    },
    /// var, let ou const
    VarDecl(VariableDeclaration<'a>),
    Expr(Expr<'a>),
    /// return expr;
    Return(Option<Expr<'a>>),
    Break(&'a src),
    Continue(&'a src),
    If {
        src: &'a src,
        condition: Expr<'a>,
        body: Statements<'a>,
        branch: Option<Box<Statement<'a>>>,
    },
    While {
        src: &'a src,
        condition: Expr<'a>,
        body: Statements<'a>,
    },
    Do {
        src: &'a src,
        body: Statements<'a>,
        condition: Expr<'a>,
    },
    ForExpr {
        src: &'a src,
        init: Expr<'a>,
        condition: Expr<'a>,
        increment: Expr<'a>,
        body: Statements<'a>,
    },
    ForVarDecl {
        src: &'a src,
        init: VariableDeclaration<'a>,
        condition: Expr<'a>,
        increment: Expr<'a>,
        body: Statements<'a>,
    },
    ForEach {
        src: &'a src,
        forty: ForEachType<'a>,
        init: ForEachInit<'a>,
        iterator: Expr<'a>,
        body: Statements<'a>,
    },
    Switch {
        src: &'a src,
        expr: Expr<'a>,
        body: Statements<'a>,
    },
    Case(&'a src, Expr<'a>),
    Throw(&'a src, Expr<'a>),
    Try {
        src: &'a src,
        body: Statements<'a>,
        catch: Option<(Option<Pattern<'a>>, Statements<'a>)>,
        finally: Option<Statements<'a>>,
    },
    Debugger(&'a src),
    Scope(Statements<'a>),
}

/// representa um var, let ou const
#[derive(Debug, Clone)]
pub struct VariableDeclaration<'a> {
    pub decltype: VariableDeclarationType<'a>,
    pub decls: Patterns<'a>,
}

/// qual o tipo de inicialização da variável / constante
#[derive(Debug, Clone)]
pub enum VariableDeclarationType<'a> {
    Var(&'a src),
    Let(&'a src),
    Const(&'a src),
}

/// qual o tipo de for each, tem o "in" e o "of"
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForEachType<'a> {
    In(&'a src),
    Of(&'a src),
}

/// um enum que representa uma inicialização de um for loop qualquer
///
/// ao invés de todos os outros structs e enums, este não é usado diretamente por Program,
///
/// serve apenas como variável temporaria para criar uma das três variações de For
#[derive(Debug, Clone)]
pub enum ForInitStatement<'a> {
    VarDecl(VariableDeclaration<'a>),
    Expr(Expr<'a>),
}

///
#[derive(Debug, Clone)]
pub enum ForEachInit<'a> {
    VarDecl {
        decltype: VariableDeclarationType<'a>,
        decls: Pattern<'a>,
    },
    Assignee(Assignee<'a>),
}

/// uma lista de Pattern, separada por virgula, representa as declarações de um let, var, const ou parâmetros de uma função
#[derive(Debug, Clone)]
pub struct Patterns<'a>(pub Vec<Pattern<'a>>);

/// uma declaração de um let, var, const, ou um paramêtro
///
/// essa enum encapsula o fato que `let foo`, `let [a,{b,c}]`, `let {a:c,b:[x,y,z]}` são todos válidos
#[derive(Debug, Clone)]
pub enum Pattern<'a> {
    Identifier(&'a str, Option<Expr<'a>>),
    Array(&'a src, Vec<Pattern<'a>>, Option<Expr<'a>>),
    Object(&'a src, Vec<(&'a str, Pattern<'a>)>, Option<Expr<'a>>),
    Rest(&'a src, Box<Pattern<'a>>),
}

/// uma expressão ao qual podemos usar o Assign (=)
#[derive(Debug, Clone)]
pub enum Assignee<'a> {
    /// `foo = ...`
    Identifier(&'a str),
    /// `[a, b, c] = ...`
    Array(&'a src, Vec<Assignee<'a>>),
    /// `{a, b, c} = ...`
    Object(&'a src, Vec<(&'a str, Assignee<'a>)>),
    /// `obj.member = ...`
    Member(&'a src, Expr<'a>, &'a str),
    /// `obj[0] = ...`
    Index(&'a src, Expr<'a>, Expr<'a>),
    /// válido apenas quando entro de um Assignee::Array ou Assignee::Object
    ///
    /// `[...rest] = ...`
    ///
    /// `{...rest} = ...`
    Rest(&'a src, Box<Assignee<'a>>),
}

/// uma expressão de javascript, pode ser qualquer
/// coisa que quando executada resulta em um valor
///
/// QUALQUER COISA
#[derive(Debug, Clone)]
pub enum Expr<'a> {
    // expressões primarias (ExprPart::Value)
    Undefined(&'a src),
    Null(&'a src),
    True(&'a src),
    False(&'a src),
    This(&'a src),
    Parameters(&'a src),
    NewTarget(&'a src),
    /// um identificador solitário
    Identifier(&'a str),
    /// um numero em qualquer formato possível
    Number(&'a str),
    /// uma string em qualquer formato possível
    String(&'a str),
    /// uma função no contexto de uma expressão
    Function {
        src: &'a src,
        is_async: Option<&'a src>,
        name: Option<&'a str>,
        args: Patterns<'a>,
        body: Statements<'a>,
    },
    FunctionShort {
        src: &'a src,
        is_async: Option<&'a src>,
        args: Patterns<'a>,
        body: Statements<'a>,
    },
    FunctionExpr {
        src: &'a src,
        is_async: Option<&'a src>,
        args: Patterns<'a>,
        body: Box<Expr<'a>>,
    },
    /// colchetes, ex: []
    Array(&'a src, Vec<Expr<'a>>),
    /// chaves, ex: {}
    Object(&'a src, Vec<(&'a str, Expr<'a>)>),
    /// uma chamada de new, com argumentos
    NewCall(&'a src, Box<(Expr<'a>, Vec<Expr<'a>>)>),

    // expressões com operadores
    /// expressão com um operador que fica depois do valor, ex: i++
    Postfix(Box<(Expr<'a>, ExprPostfix<'a>)>),
    /// expressão com um operador que fica entre dois valores, ex: 1 + 2
    Infix(Box<(Expr<'a>, ExprInfix<'a>, Expr<'a>)>),
    /// expressão com um operador que fica antes de um valor, ex: -1
    Prefix(Box<(ExprPrefix<'a>, Expr<'a>)>),
    /// expressão com o operador ternário, ex: condicao ? "sim" : "não"
    Ternary(Box<(Expr<'a>, Expr<'a>, Expr<'a>)>),
    /// expressão onde hà escrita de um valor
    Assign(Box<(Assignee<'a>, ExprInfixAssign<'a>, Expr<'a>)>),
    /// uma expressão com ... antes dela, válido apenas em certas circunstâncias, ex: ...iterador
    Spread(&'a src, Box<Expr<'a>>),
}

/// parte de uma Expr em construção
#[derive(Debug, Clone)]
pub enum ExprPart<'a> {
    Taken,
    Value(Expr<'a>),
    Prefix(ExprPrefix<'a>),
    Postfix(ExprPostfix<'a>),
    Infix(ExprInfix<'a>),
}

/// representa um operador que vem antes de um valor
#[derive(Debug, Clone)]
pub enum ExprPrefix<'a> {
    Delete(&'a src),
    Typeof(&'a src),
    Void(&'a src),
    New(&'a src),
    BitwiseNot(&'a src),
    Not(&'a src),
    Await(&'a src),
    UnaryPlus(&'a src),
    UnaryMinus(&'a src),
    PreIncrement(&'a src),
    PreDecrement(&'a src),
}

/// representa um operador que vem depois de um valor
#[derive(Debug, Clone)]
pub enum ExprPostfix<'a> {
    Call(&'a src, Vec<Expr<'a>>),
    Index(&'a src, Expr<'a>),
    Member(&'a src, &'a str),
    MemberOpt(&'a src, &'a str),
    PostIncrement(&'a src),
    PostDecrement(&'a src),
}

/// representa um operador que vem entre dois valores
#[derive(Debug, Clone)]
pub enum ExprInfix<'a> {
    Power(&'a src),
    Multiply(&'a src),
    Divide(&'a src),
    Remainder(&'a src),
    Add(&'a src),
    Subtract(&'a src),
    BitwiseLeft(&'a src),
    BitwiseRight(&'a src),
    BitwiseLeftUnsigned(&'a src),
    Less(&'a src),
    LessEqual(&'a src),
    Greater(&'a src),
    GreaterEqual(&'a src),
    In(&'a src),
    InstanceOf(&'a src),
    Equal(&'a src),
    NotEqual(&'a src),
    Identical(&'a src),
    NotIdentical(&'a src),
    BitwiseAnd(&'a src),
    BitwiseXor(&'a src),
    BitwiseOr(&'a src),
    And(&'a src),
    Or(&'a src),
    Coalesce(&'a src),
    TernaryQuestion(&'a src),
    TernaryAnswer(&'a src),
    Assign(ExprInfixAssign<'a>),
    Comma(&'a src),
}

/// representa um operador que vem entre um Assignee e um Expr
#[derive(Debug, Clone)]
pub enum ExprInfixAssign<'a> {
    Assign(&'a src),
    Power(&'a src),
    Add(&'a src),
    Subtract(&'a src),
    Multiply(&'a src),
    Divide(&'a src),
    Remainder(&'a src),
    BitwiseLeft(&'a src),
    BitwiseRight(&'a src),
    BitwiseLeftUnsigned(&'a src),
    BitwiseAnd(&'a src),
    BitwiseXor(&'a src),
    BitwiseOr(&'a src),
    Coalesce(&'a src),
}

impl<'a> Program<'a> {
    pub fn parse(iter: &mut TokenIter<'a>) -> Self {
        let mut statements = Vec::new();
        while !iter.eof() {
            statements.push(Statement::parse(iter));
            iter.next_eos();
        }
        Self(statements)
    }
}
impl<'a> Statements<'a> {
    fn parse(iter: &mut TokenIter<'a>) -> Self {
        if iter.consume("{") {
            let mut statements = Vec::new();
            while !iter.consume("}") {
                statements.push(Statement::parse(iter));
                iter.next_eos();
            }
            Self(statements)
        } else {
            let statement = Statement::parse(iter);
            iter.next_eos();
            Self(vec![statement])
        }
    }
}
impl<'a> Statement<'a> {
    fn parse(iter: &mut TokenIter<'a>) -> Self {
        match iter.text() {
            "async" | "function" => {
                let is_async = if let Some(is_async) = iter.consume_text("async") {
                    parse_token(iter, "function", "Esperado function após async");
                    Some(is_async)
                } else {
                    iter.next();
                    None
                };
                let name = parse_identifier(iter);
                parse_token(iter, "(", "Esperado (");
                let args = Patterns::parse_opt(iter, false);
                parse_token(iter, ")", "Esperado )");
                let body = Statements::parse(iter);
                Self::Function {
                    is_async,
                    name,
                    args,
                    body,
                }
            }
            "var" | "let" | "const" => Self::VarDecl(VariableDeclaration::parse(iter)),
            "return" => {
                iter.next();
                if iter.eos() {
                    Self::Return(None)
                } else {
                    Self::Return(Some(Expr::parse_one(iter)))
                }
            }
            "break" => Self::Break(iter.take_text()),
            "continue" => Self::Continue(iter.take_text()),
            "if" => {
                let src = iter.take_text();
                let condition = parse_parethesis_expr(iter);
                let body = Statements::parse(iter);
                let branch = if iter.consume("else") {
                    Some(Box::new(Statement::parse(iter)))
                } else {
                    None
                };
                Self::If {
                    src,
                    condition,
                    body,
                    branch,
                }
            }
            "while" => {
                let src = iter.take_text();
                let condition = parse_parethesis_expr(iter);
                let body = Statements::parse(iter);
                Self::While {
                    src,
                    condition,
                    body,
                }
            }
            "do" => {
                let src = iter.take_text();
                let body = Statements::parse(iter);
                parse_token(iter, "while", "Esperado while, pois há um loop do");
                let condition = parse_parethesis_expr(iter);
                Self::Do {
                    src,
                    body,
                    condition,
                }
            }
            "for" => {
                let src = iter.take_text();
                parse_token(iter, "(", "Esperado ( após o for");
                let init = ForInitStatement::parse(iter);
                let forty = if let Some(src) = iter.consume_text("in") {
                    Some(ForEachType::In(src))
                } else if let Some(src) = iter.consume_text("of") {
                    Some(ForEachType::Of(src))
                } else {
                    parse_token(iter, ";", "Esperado in, of ou ;");
                    None
                };
                if let Some(forty) = forty {
                    let iterator = Expr::parse_one(iter);
                    parse_token(iter, ")", "Esperado ) do for");
                    let body = Statements::parse(iter);
                    match init {
                        ForInitStatement::VarDecl(vardecl) => {
                            let pat = match forty {
                                ForEachType::In(_) => vardecl
                                    .decls
                                    .ensure_one(iter, "Apenas uma variável é permitida no for in"),
                                ForEachType::Of(_) => vardecl
                                    .decls
                                    .ensure_one(iter, "Apenas uma variável é permitida no for of"),
                            };
                            match forty {
                                ForEachType::In(_) => pat.ensure_no_defaults(iter, "Um valor padrão não pode ser especificado na variável do for in"),
                                ForEachType::Of(_) => pat.ensure_no_defaults(iter, "Um valor padrão não pode ser especificado na variável do for of"),
                            }
                            Statement::ForEach {
                                src,
                                forty,
                                init: ForEachInit::VarDecl {
                                    decltype: vardecl.decltype,
                                    decls: pat,
                                },
                                iterator,
                                body,
                            }
                        }
                        ForInitStatement::Expr(expr) => Statement::ForEach {
                            src,
                            forty,
                            init: ForEachInit::Assignee(expr.to_assignee(iter)),
                            iterator,
                            body,
                        },
                    }
                } else {
                    let condition = Expr::parse_one(iter);
                    parse_token(iter, ";", "Esperado ; do for");
                    let increment = Expr::parse_one(iter);
                    parse_token(iter, ")", "Esperado ) do for");
                    let body = Statements::parse(iter);
                    match init {
                        ForInitStatement::VarDecl(init) => Statement::ForVarDecl {
                            src,
                            init,
                            condition,
                            increment,
                            body,
                        },
                        ForInitStatement::Expr(init) => Statement::ForExpr {
                            src,
                            init,
                            condition,
                            increment,
                            body,
                        },
                    }
                }
            }
            "switch" => {
                let src = iter.take_text();
                let expr = parse_parethesis_expr(iter);
                if iter.text() != "{" {
                    iter.error("Esperado {")
                }
                let body = Statements::parse(iter);
                Self::Switch { src, expr, body }
            }
            "case" => {
                let src = iter.take_text();
                let expr = Expr::parse_one(iter);
                parse_token(iter, ":", "Esperado um : após a expressão do case");
                Self::Case(src, expr)
            }
            "throw" => {
                let src = iter.take_text();
                Self::Throw(src, Expr::parse_one(iter))
            }
            "try" => {
                let src = iter.take_text();
                if iter.text() != "{" {
                    iter.error("Esperado {")
                }
                let body = Statements::parse(iter);
                let catch = if iter.consume("catch") {
                    let pattern = if iter.consume("(") {
                        let pat = Pattern::parse(iter, false);
                        pat.ensure_no_defaults(
                            iter,
                            "Um valor padrão não pode ser especificado aqui",
                        );
                        Some(pat)
                    } else {
                        None
                    };
                    if iter.text() != "{" {
                        iter.error("Esperado {")
                    }
                    let catch_body = Statements::parse(iter);
                    Some((pattern, catch_body))
                } else {
                    None
                };
                let finally = if iter.consume("finally") {
                    if iter.text() != "{" {
                        iter.error("Esperado {")
                    }
                    Some(Statements::parse(iter))
                } else {
                    None
                };
                Self::Try {
                    src,
                    body,
                    catch,
                    finally,
                }
            }
            "debugger" => Self::Debugger(iter.take_text()),
            "{" => Self::Scope(Statements::parse(iter)),
            _ => Self::Expr(Expr::parse_one(iter)),
        }
    }
}
impl<'a> VariableDeclaration<'a> {
    fn parse(iter: &mut TokenIter<'a>) -> Self {
        let decltype = VariableDeclarationType::parse(iter);
        let decls = Patterns::parse(iter, true);
        Self { decltype, decls }
    }
}
impl<'a> VariableDeclarationType<'a> {
    fn parse(iter: &mut TokenIter<'a>) -> Self {
        match iter.text() {
            "var" => Self::Var(iter.take_text()),
            "let" => Self::Let(iter.take_text()),
            "const" => Self::Const(iter.take_text()),
            _ => {
                iter.error("Esperado var, let ou const");
                Self::Var("")
            }
        }
    }
}
impl<'a> ForInitStatement<'a> {
    fn parse(iter: &mut TokenIter<'a>) -> Self {
        match iter.text() {
            "var" | "let" | "const" => Self::VarDecl(VariableDeclaration::parse(iter)),
            _ => Self::Expr(Expr::parse_one(iter)),
        }
    }
}
impl<'a> Patterns<'a> {
    /// nunca retorna um Patterns vazio
    fn parse(iter: &mut TokenIter<'a>, require_default: bool) -> Self {
        let mut args = Vec::new();
        loop {
            args.push(Pattern::parse(iter, require_default));
            if !iter.consume(",") {
                break;
            }
            if iter.ty() != Some(TokenType::Identifier) && !matches!(iter.text(), "{" | "[") {
                break;
            }
        }
        Self(args)
    }
    /// mesmo que parse, mas se não houver patters, retorna um Patterns vazio
    fn parse_opt(iter: &mut TokenIter<'a>, require_default: bool) -> Self {
        if iter.ty() != Some(TokenType::Identifier) && !matches!(iter.text(), "{" | "[") {
            Self(Vec::new())
        } else {
            Self::parse(iter, require_default)
        }
    }
    /// emite um erro não houver exatamente um pattern neste Vec, retorna o primeiro pattern
    fn ensure_one(
        self,
        iter: &mut TokenIter<'a>,
        message: impl Into<Cow<'static, str>>,
    ) -> Pattern<'a> {
        if self.len() != 1 {
            iter.error(message);
        }
        self.0
            .into_iter()
            .next()
            .unwrap_or(Pattern::Identifier("", None))
    }
}
impl<'a> Pattern<'a> {
    fn parse(iter: &mut TokenIter<'a>, require_default: bool) -> Self {
        let rest_allowed = !require_default;
        let pat = if iter.ty() == Some(TokenType::Identifier) {
            let text = iter.text();
            iter.next();
            Self::Identifier(text, None)
        } else if iter.text() == "[" {
            let mut src = iter.take_text();
            let mut items = Vec::new();
            loop {
                if let Some(src_end) = iter.consume_text("]") {
                    src = iter.cover(src, src_end);
                }
                items.push(Pattern::parse(iter, false));
                if !iter.consume(",") {
                    if let Some(src_end) = iter.consume_text("]") {
                        src = iter.cover(src, src_end);
                    } else {
                        iter.error("Esperado o ] da desestruturação");
                    }
                    break;
                }
            }
            Self::Array(src, items, None)
        } else if iter.text() == "{" {
            let mut src = iter.take_text();
            let mut pairs = Vec::new();
            loop {
                if let Some(src_end) = iter.consume_text("}") {
                    src = iter.cover(src, src_end);
                }
                let key = parse_identifier(iter);
                if iter.consume(":") {
                    pairs.push((key, Pattern::parse(iter, false)));
                } else {
                    pairs.push((key, Pattern::Identifier(key, None)));
                }
                if !iter.consume(",") {
                    if let Some(src_end) = iter.consume_text("}") {
                        src = iter.cover(src, src_end);
                    } else {
                        iter.error("Esperado o } da desestruturação");
                    }
                    break;
                }
            }
            Self::Object(src, pairs, None)
        } else if rest_allowed && iter.text() == "." && iter.space() == Some(TokenSpace::Joint) {
            let token = iter.take_text();
            if let Ok(token) = iter.join(token, ".") {
                if let Ok(token) = iter.join(token, ".") {
                    Self::Rest(token, Box::new(Pattern::parse(iter, false)))
                } else {
                    iter.error("Esperado ...");
                    Self::Rest(token, Box::new(Pattern::parse(iter, false)))
                }
            } else {
                iter.error("Esperado ...");
                Self::Rest(token, Box::new(Pattern::parse(iter, false)))
            }
        } else {
            iter.error("Esperado um identificador ou uma desestruturação");
            return Self::Identifier("", None);
        };
        if let Self::Rest(_, pat) = &pat {
            match &**pat {
                Self::Identifier(_, Some(expr))
                | Self::Array(_, _, Some(expr))
                | Self::Object(_, _, Some(expr)) => {
                    iter.error_at(
                        expr.src(iter),
                        "Um valor padrão não pode ser especificado para essa desestruturação",
                    );
                }
                _ => {}
            }
        }
        if iter.consume("=") {
            let expr = Expr::parse(iter);
            match pat {
                Self::Identifier(pat, _) => Self::Identifier(pat, Some(expr)),
                Self::Array(src, pat, _) => Self::Array(src, pat, Some(expr)),
                Self::Object(src, pat, _) => Self::Object(src, pat, Some(expr)),
                Self::Rest(src, pat) => Self::Rest(src, pat),
            }
        } else {
            if require_default && matches!(pat, Self::Array(..) | Self::Object(..)) {
                iter.error("Esperado um valor");
            }
            pat
        }
    }
    /// emite erros, se hover valores padrão neste Pattern
    fn ensure_no_defaults(
        &self,
        iter: &mut TokenIter<'a>,
        message: impl Into<Cow<'static, str>> + Clone,
    ) {
        let default = match self {
            Pattern::Identifier(_, default) => default,
            Pattern::Array(_, items, default) => {
                for item in items {
                    item.ensure_no_defaults(iter, message.clone());
                }
                default
            }
            Pattern::Object(_, pairs, default) => {
                for (_, value) in pairs {
                    value.ensure_no_defaults(iter, message.clone());
                }
                default
            }
            Pattern::Rest(..) => &None,
        };
        if let Some(default) = default {
            iter.error_at(default.src(iter), message);
        }
    }
}
impl<'a> Assignee<'a> {
    fn src(&self) -> &'a src {
        match self {
            Assignee::Identifier(src)
            | Assignee::Array(src, _)
            | Assignee::Object(src, _)
            | Assignee::Member(src, _, _)
            | Assignee::Index(src, _, _)
            | Assignee::Rest(src, _) => src,
        }
    }
}
impl<'a> Expr<'a> {
    fn parse(iter: &mut TokenIter<'a>) -> Self {
        let mut parts = Vec::with_capacity(8);
        let mut ternary_depth: usize = 0;
        loop {
            let value = loop {
                let prefix = match iter.ty() {
                    Some(TokenType::Identifier) => break Expr::Identifier(iter.take_text()),
                    Some(TokenType::Number) => break Expr::Number(iter.take_text()),
                    Some(TokenType::String) => break Expr::String(iter.take_text()),
                    Some(TokenType::Keyword) => match iter.text() {
                        "new" => {
                            let src = iter.take_text();
                            if iter.consume(".") {
                                let member = parse_identifier(iter);
                                if member != "target" {
                                    iter.error_at(member, "Esperado target");
                                }
                                break Expr::NewTarget(iter.cover(src, member));
                            } else {
                                ExprPrefix::New(src)
                            }
                        }
                        // PREFIX
                        "delete" => ExprPrefix::Delete(iter.take_text()),
                        "typeof" => ExprPrefix::Typeof(iter.take_text()),
                        "void" => ExprPrefix::Void(iter.take_text()),
                        "await" => ExprPrefix::Await(iter.take_text()),
                        // VALUE
                        "undefined" => break Expr::Undefined(iter.take_text()),
                        "null" => break Expr::Null(iter.take_text()),
                        "true" => break Expr::True(iter.take_text()),
                        "false" => break Expr::False(iter.take_text()),
                        "this" => break Expr::This(iter.take_text()),
                        "parameters" => break Expr::Parameters(iter.take_text()),
                        "async" | "function" => {
                            let mut src = iter.text();
                            let is_async = iter.consume_text("async");
                            if iter.consume("function") {
                                let name = if iter.ty() == Some(TokenType::Identifier) {
                                    Some(iter.take_text())
                                } else {
                                    None
                                };
                                parse_token(iter, "(", "Esperado (");
                                let args = Patterns::parse_opt(iter, false);
                                if let Some(src_end) = iter.consume_text(")") {
                                    src = iter.cover(src, src_end);
                                } else {
                                    iter.error("Esperado )");
                                }
                                parse_token(iter, "{", "Esperado {");
                                let body = Statements::parse(iter);
                                break Expr::Function {
                                    src,
                                    is_async,
                                    name,
                                    args,
                                    body,
                                };
                            } else if iter.consume("(") {
                                let args = Patterns::parse_opt(iter, false);
                                if let Some(src_end) = iter.consume_text(")") {
                                    src = iter.cover(src, src_end);
                                } else {
                                    iter.error("Esperado )");
                                }
                                parse_arrow(iter);
                                if iter.text() == "{" {
                                    let body = Statements::parse(iter);
                                    break Expr::FunctionShort {
                                        src,
                                        is_async,
                                        args,
                                        body,
                                    };
                                } else {
                                    let body = Expr::parse(iter);
                                    break Expr::FunctionExpr {
                                        src,
                                        is_async,
                                        args,
                                        body: Box::new(body),
                                    };
                                }
                            } else if iter.ty() == Some(TokenType::Identifier) {
                                let args = Patterns(vec![Pattern::Identifier(iter.text(), None)]);
                                iter.next();
                                if let Some(src_end) = iter.consume_text(")") {
                                    src = iter.cover(src, src_end);
                                } else {
                                    iter.error("Esperado )");
                                }
                                parse_arrow(iter);
                                if iter.text() == "{" {
                                    let body = Statements::parse(iter);
                                    break Expr::FunctionShort {
                                        src,
                                        is_async,
                                        args,
                                        body,
                                    };
                                } else {
                                    let body = Expr::parse(iter);
                                    break Expr::FunctionExpr {
                                        src,
                                        is_async,
                                        args,
                                        body: Box::new(body),
                                    };
                                }
                            } else {
                                iter.error("Esperado function ou () => {}");
                                break Expr::Undefined("");
                            }
                        }
                        _ => {
                            iter.error("Esperado um valor");
                            break Expr::Undefined("");
                        }
                    },
                    Some(TokenType::Punct) => match iter.text() {
                        // PREFIX
                        "~" => ExprPrefix::BitwiseNot(iter.take_text()),
                        "!" => ExprPrefix::Not(iter.take_text()),
                        "+" => {
                            let token = iter.take_text();
                            match iter.join(token, "+") {
                                Ok(token) => ExprPrefix::PreIncrement(token),
                                Err(token) => ExprPrefix::UnaryPlus(token),
                            }
                        }
                        "-" => {
                            let token = iter.take_text();
                            match iter.join(token, "-") {
                                Ok(token) => ExprPrefix::PreDecrement(token),
                                Err(token) => ExprPrefix::UnaryMinus(token),
                            }
                        }
                        // VALUE
                        "(" => {
                            iter.next();
                            let expr = Expr::parse_one(iter);
                            parse_token(iter, ")", "Esperado )");
                            break expr;
                        }
                        "[" => {
                            let mut src = iter.take_text();
                            let exprs = Expr::parse_many_opt(iter);
                            if let Some(src_end) = iter.consume_text("]") {
                                src = iter.cover(src, src_end);
                            } else {
                                iter.error("Esperado ]");
                            }
                            break Expr::Array(src, exprs);
                        }
                        "{" => {
                            let mut src = iter.take_text();
                            let mut pairs = Vec::new();
                            loop {
                                if let Some(src_end) = iter.consume_text("}") {
                                    src = iter.cover(src, src_end);
                                    break;
                                }
                                // TODO! add more property types (string literal as key, get property, set property, computed property, prototype, and spread)
                                let key = parse_identifier(iter);
                                let value = if iter.consume(":") {
                                    Expr::parse(iter)
                                } else if iter.consume("(") {
                                    let args = Patterns::parse_opt(iter, false);
                                    parse_token(iter, ")", "Esperado )");
                                    if iter.text() != "{" {
                                        iter.error("Esperado {");
                                    }
                                    let body = Statements::parse(iter);
                                    Expr::Function {
                                        src: key,
                                        is_async: None,
                                        name: Some(key),
                                        args,
                                        body,
                                    }
                                } else {
                                    Expr::Identifier(key)
                                };
                                pairs.push((key, value));
                                if !iter.consume(",") {
                                    if let Some(src_end) = iter.consume_text("}") {
                                        src = iter.cover(src, src_end);
                                    } else {
                                        iter.error("Esperado }");
                                    }
                                    break;
                                }
                            }
                            break Expr::Object(src, pairs);
                        }
                        _ => {
                            iter.error("Esperado um valor");
                            break Expr::Undefined("");
                        }
                    },
                    None => {
                        iter.error("Esperado um valor");
                        break Expr::Undefined("");
                    }
                };
                parts.push(ExprPart::Prefix(prefix));
            };
            parts.push(ExprPart::Value(value));
            let infix = loop {
                let postfix = match iter.text() {
                    "(" => {
                        let mut src = iter.take_text();
                        let exprs = Expr::parse_many_opt(iter);
                        if let Some(src_end) = iter.consume_text(")") {
                            src = iter.cover(src, src_end);
                        } else {
                            iter.error("Esperado )")
                        }
                        ExprPostfix::Call(src, exprs)
                    }
                    "[" => {
                        let mut src = iter.take_text();
                        let expr = Expr::parse_one(iter);
                        if let Some(src_end) = iter.consume_text("]") {
                            src = iter.cover(src, src_end);
                        } else {
                            iter.error("Esperado ]")
                        }
                        ExprPostfix::Index(src, expr)
                    }
                    "." => {
                        let src = iter.take_text();
                        let member = parse_identifier(iter);
                        ExprPostfix::Member(iter.cover(src, member), member)
                    }
                    "+" => {
                        let token = iter.take_text();
                        if let Ok(token) = iter.join(token, "+") {
                            ExprPostfix::PostIncrement(token)
                        } else if let Ok(token) = iter.join(token, "=") {
                            break ExprInfix::Assign(ExprInfixAssign::Add(token));
                        } else {
                            break ExprInfix::Add(token);
                        }
                    }
                    "-" => {
                        let token = iter.take_text();
                        if let Ok(token) = iter.join(token, "-") {
                            ExprPostfix::PostDecrement(token)
                        } else if let Ok(token) = iter.join(token, "=") {
                            break ExprInfix::Assign(ExprInfixAssign::Multiply(token));
                        } else {
                            break ExprInfix::Multiply(token);
                        }
                    }
                    "*" => {
                        let token = iter.take_text();
                        if let Ok(token) = iter.join(token, "*") {
                            if let Ok(token) = iter.join(token, "*") {
                                break ExprInfix::Assign(ExprInfixAssign::Power(token));
                            } else {
                                break ExprInfix::Power(token);
                            }
                        } else if let Ok(token) = iter.join(token, "=") {
                            break ExprInfix::Assign(ExprInfixAssign::Subtract(token));
                        } else {
                            break ExprInfix::Subtract(token);
                        }
                    }
                    "/" => {
                        let token = iter.take_text();
                        if let Ok(token) = iter.join(token, "=") {
                            break ExprInfix::Assign(ExprInfixAssign::Divide(token));
                        } else {
                            break ExprInfix::Divide(token);
                        }
                    }
                    "%" => {
                        let token = iter.take_text();
                        if let Ok(token) = iter.join(token, "=") {
                            break ExprInfix::Assign(ExprInfixAssign::Remainder(token));
                        } else {
                            break ExprInfix::Remainder(token);
                        }
                    }
                    "=" => {
                        let token = iter.take_text();
                        if let Ok(token) = iter.join(token, "=") {
                            if let Ok(token) = iter.join(token, "=") {
                                break ExprInfix::Identical(token);
                            } else {
                                break ExprInfix::Equal(token);
                            }
                        } else {
                            break ExprInfix::Assign(ExprInfixAssign::Assign(token));
                        }
                    }
                    "!" => {
                        let token = iter.take_text();
                        if let Ok(token) = iter.join(token, "=") {
                            if let Ok(token) = iter.join(token, "=") {
                                break ExprInfix::NotIdentical(token);
                            } else {
                                break ExprInfix::NotEqual(token);
                            }
                        } else {
                            iter.error("Esperado !=");
                            break ExprInfix::NotEqual(token);
                        }
                    }
                    ">" => {
                        let token = iter.take_text();
                        if let Ok(token) = iter.join(token, ">") {
                            if let Ok(token) = iter.join(token, ">") {
                                if let Ok(token) = iter.join(token, "=") {
                                    break ExprInfix::Assign(ExprInfixAssign::BitwiseLeftUnsigned(
                                        token,
                                    ));
                                } else {
                                    break ExprInfix::BitwiseLeftUnsigned(token);
                                }
                            } else if let Ok(token) = iter.join(token, "=") {
                                break ExprInfix::Assign(ExprInfixAssign::BitwiseLeft(token));
                            } else {
                                break ExprInfix::BitwiseLeft(token);
                            }
                        } else if let Ok(token) = iter.join(token, "=") {
                            break ExprInfix::GreaterEqual(token);
                        } else {
                            break ExprInfix::Greater(token);
                        }
                    }
                    "<" => {
                        let token = iter.take_text();
                        if let Ok(token) = iter.join(token, "<") {
                            if let Ok(token) = iter.join(token, "=") {
                                break ExprInfix::Assign(ExprInfixAssign::BitwiseRight(token));
                            } else {
                                break ExprInfix::BitwiseRight(token);
                            }
                        } else if let Ok(token) = iter.join(token, "=") {
                            break ExprInfix::LessEqual(token);
                        } else {
                            break ExprInfix::Less(token);
                        }
                    }
                    "&" => {
                        let token = iter.take_text();
                        if let Ok(token) = iter.join(token, "&") {
                            break ExprInfix::And(token);
                        } else if let Ok(token) = iter.join(token, "=") {
                            break ExprInfix::Assign(ExprInfixAssign::BitwiseAnd(token));
                        } else {
                            break ExprInfix::BitwiseAnd(token);
                        }
                    }
                    "|" => {
                        let token = iter.take_text();
                        if let Ok(token) = iter.join(token, "|") {
                            break ExprInfix::Or(token);
                        } else if let Ok(token) = iter.join(token, "=") {
                            break ExprInfix::Assign(ExprInfixAssign::BitwiseOr(token));
                        } else {
                            break ExprInfix::BitwiseOr(token);
                        }
                    }
                    "^" => {
                        let token = iter.take_text();
                        if let Ok(token) = iter.join(token, "^") {
                            break ExprInfix::Assign(ExprInfixAssign::BitwiseXor(token));
                        } else {
                            break ExprInfix::BitwiseXor(token);
                        }
                    }
                    "?" => {
                        let token = iter.take_text();
                        match iter.join(token, ".") {
                            Ok(token) => {
                                let member = parse_identifier(iter);
                                ExprPostfix::MemberOpt(iter.cover(token, member), member)
                                // ?.
                            }
                            Err(token) => match iter.join(token, "?") {
                                Ok(token) => match iter.join(token, "=") {
                                    Ok(token) => {
                                        break ExprInfix::Assign(ExprInfixAssign::Coalesce(token))
                                    }
                                    Err(token) => break ExprInfix::Coalesce(token),
                                },
                                Err(token) => {
                                    ternary_depth += 1;
                                    break ExprInfix::TernaryQuestion(token);
                                }
                            },
                        }
                    }
                    // se o dois pontos for encontrada, mas ternary_depth for 0, significa que a expressão acabou
                    ":" if ternary_depth > 0 => {
                        ternary_depth -= 1;
                        break ExprInfix::TernaryAnswer(iter.take_text());
                    }
                    // se a virgula for encontrada, mas ternary_depth for 0, significa que a expressão acabou
                    "," if ternary_depth > 0 => {
                        // vírgula não é permitido no operador ternario
                        // nada quebra no noude.js, mas pelo visto não é permitido no padrão
                        iter.error("Vírgula não pode ser usada dentro do operador ternário");
                        break ExprInfix::Comma(iter.take_text());
                    }
                    "in" => {
                        break ExprInfix::In(iter.take_text());
                    }
                    "instanceof" => {
                        break ExprInfix::InstanceOf(iter.take_text());
                    }
                    _ => {
                        if ternary_depth > 0 {
                            iter.error("Esperado :");
                            parts.reserve(ternary_depth * 2);
                            for _ in 0..ternary_depth {
                                parts.push(ExprPart::Infix(ExprInfix::TernaryAnswer("")));
                                parts.push(ExprPart::Value(Expr::Undefined("")));
                            }
                        }
                        // um operador postfix não foi encontrado, nem um infix, termina a expressão
                        return Self::from_parts(iter, parts.as_mut_slice());
                    }
                };
                parts.push(ExprPart::Postfix(postfix));
            };
            // um operador infix foi encontrado
            parts.push(ExprPart::Infix(infix));
            // agora repete tudo, para obter a expressão que vem depois do operador infix
        }
    }
    /// parsar apenas uma expressão, isso significa que a vírgula é interpretada como um operador
    ///
    /// e o operador spread é inválido nesse contexto
    fn parse_one(iter: &mut TokenIter<'a>) -> Self {
        let mut expr = Expr::parse(iter);
        while let Some(src) = iter.consume_text(",") {
            expr = Expr::Infix(Box::new((expr, ExprInfix::Comma(src), Expr::parse(iter))));
        }
        expr
    }
    /// parsar várias expressões (uma ou mais), isso significa que a vírgula é interpretada como um separador
    ///
    /// e o operador spread é válido nesse contexto
    fn parse_many(iter: &mut TokenIter<'a>) -> Vec<Self> {
        let mut exprs = Vec::new();
        loop {
            let expr = if iter.peek()
                != Some(&Token {
                    text: ".",
                    space: TokenSpace::Joint,
                    ty: TokenType::Punct,
                }) {
                Self::parse(iter)
            } else {
                let token = iter.take_text();
                if let Ok(token) = iter.join(token, ".") {
                    if let Ok(token) = iter.join(token, ".") {
                        Expr::Spread(token, Box::new(Expr::parse(iter)))
                    } else {
                        iter.error("Esperado ...");
                        Expr::Spread(token, Box::new(Expr::Undefined("")))
                    }
                } else {
                    iter.error("Esperado ...");
                    Expr::Spread(token, Box::new(Expr::Undefined("")))
                }
            };
            exprs.push(expr);
            if !iter.consume(",") {
                return exprs;
            }
            if Self::is_expr(iter.peek())
                || iter.peek()
                    == Some(&Token {
                        text: ".",
                        space: TokenSpace::Joint,
                        ty: TokenType::Punct,
                    })
            {
                continue;
            }
            return exprs;
        }
    }
    /// identico a parse_many, mas se o primeiro token não for de uma expressão, retorna um vetor vazio
    fn parse_many_opt(iter: &mut TokenIter<'a>) -> Vec<Self> {
        if !Self::is_expr(iter.peek())
            && iter.peek()
                != Some(&Token {
                    text: ".",
                    space: TokenSpace::Joint,
                    ty: TokenType::Punct,
                })
        {
            Vec::new()
        } else {
            Self::parse_many(iter)
        }
    }
    /// retorna true se esse token poderia ser o começo de uma Expr
    fn is_expr(token: Option<&Token<'_>>) -> bool {
        if let Some(token) = token {
            match token.ty {
                TokenType::Number | TokenType::String | TokenType::Identifier => true,
                TokenType::Keyword => match token.text {
                    "undefined" | "null" | "true" | "false" | "this" | "parameters" | "new"
                    | "async" | "delete" | "typeof" | "void" | "~" | "!" | "await" => true,
                    _ => false,
                },
                TokenType::Punct => match token.text {
                    "+" | "-" | "(" | "[" | "{" => true,
                    _ => false,
                },
            }
        } else {
            false
        }
    }
    /// constroi uma nova expressão a partir de uma lista de ExprPart
    fn from_parts(iter: &mut TokenIter<'a>, parts: &mut [ExprPart<'a>]) -> Self {
        match parts.len() {
            0 => panic!("Expr::from_parts chamado com um array vazio"),
            1 => match parts[0].take() {
                ExprPart::Value(value) => return value,
                _ => panic!("Expr::from_parts chamado com um array de tamanho 1 inválido"),
            },
            2 => match (parts[0].take(), parts[1].take()) {
                (ExprPart::Prefix(prefix), ExprPart::Value(value)) => {
                    return Expr::Prefix(Box::new((prefix, value)))
                }
                (ExprPart::Value(value), ExprPart::Postfix(postfix)) => {
                    return Expr::Postfix(Box::new((value, postfix)))
                }
                _ => panic!("Expr::from_parts chamado com um array de tamanho 2 inválido"),
            },
            3 => match (parts[0].take(), parts[1].take(), parts[2].take()) {
                (ExprPart::Value(left), ExprPart::Infix(ExprInfix::Assign(infix)), ExprPart::Value(right)) => {
                    return Expr::Assign(Box::new((left.to_assignee(iter), infix, right)))
                }
                (ExprPart::Value(left), ExprPart::Infix(infix), ExprPart::Value(right)) => {
                    return Expr::Infix(Box::new((left, infix, right)))
                }
                (
                    ExprPart::Value(value),
                    ExprPart::Postfix(postfix1),
                    ExprPart::Postfix(postfix2),
                ) => {
                    let expr = Expr::Postfix(Box::new((value, postfix1)));
                    return Expr::Postfix(Box::new((expr, postfix2)));
                }
                (ExprPart::Prefix(prefix2), ExprPart::Prefix(prefix1), ExprPart::Value(value)) => {
                    let expr = Expr::Prefix(Box::new((prefix1, value)));
                    return Expr::Prefix(Box::new((prefix2, expr)));
                }
                (ExprPart::Prefix(ExprPrefix::New(new_src)), ExprPart::Value(value), ExprPart::Postfix(ExprPostfix::Call(call_src, args))) => {
                    let src = iter.cover(new_src, call_src);
                    return Expr::NewCall(src, Box::new((value, args)));
                }
                (ExprPart::Prefix(prefix), ExprPart::Value(value), ExprPart::Postfix(postfix)) => {
                    use std::cmp::Ordering::*;
                    match u8::cmp(&prefix.precedence(), &postfix.precedence()) {
                        Less => {
                            let expr = Expr::Postfix(Box::new((value, postfix)));
                            return Expr::Prefix(Box::new((prefix, expr)));
                        }
                        Equal => panic!("Nenhum operador tem a mesma precedencia de um operador postfix"),
                        Greater => {
                            let expr = Expr::Prefix(Box::new((prefix, value)));
                            return Expr::Postfix(Box::new((expr, postfix)));
                        }
                    }
                }
                _ => panic!("Expr::from_parts chamado com um array de tamanho 3 inválido"),
            },
            _ => {}
        }
        let mut precedence = u8::MAX;
        let mut left_index = 0;
        let mut right_index = 0;
        let mut depth: usize = 0;
        for i in 0..parts.len() {
            let part_precedence = parts[i].precedence();
            if let ExprPart::Infix(ExprInfix::TernaryQuestion(_)) = parts[i] {
                depth += 1;
            } else if let ExprPart::Infix(ExprInfix::TernaryAnswer(_)) = parts[i] {
                if depth > 0 {
                    depth += 1;
                } else {
                    panic!("Expr::from_parts chamado com um array inválido, tem um TernaryAnswer sem um TernaryQuestion correspondente");
                }
            } else if let ExprPart::Infix(ExprInfix::Comma(_)) = parts[i] {
                // é para isso que depth serve, fora validação
                // vírgulas dentro de um ternary não são considerados como candidatos
                if depth > 0 {
                    continue;
                }
            }
            if part_precedence != u8::MAX {
                if part_precedence < precedence {
                    precedence = part_precedence;
                    left_index = i;
                }
                if part_precedence <= precedence {
                    right_index = i;
                }
            }
        }
        if depth > 0 {
            panic!("Expr::from_parts chamado com um array inválido, tem um TernaryQuestion sem um TernaryAnswer correspondente");
        }
        if precedence == u8::MAX {
            panic!("Expr::from_parts chamado com um array contendo apenas ExprPart::Value");
        }
        let index = if precedence == 2 || precedence == 13 {
            // precedence == 2 são os operadores do tipo = e suas variantes e mais alguns
            // precedence == 13 é o operador ** (potência)
            // os dois acima tem "right-associativity", como queremos encontrar o lugar
            // de menor precedência, dividimos na esquerda
            left_index
        } else {
            right_index
        };
        match parts[index].take() {
            ExprPart::Infix(ExprInfix::TernaryQuestion(_)) => {
                let question_index = index;
                let mut depth: usize = 0;
                for answer_index in question_index + 1..parts.len() {
                    match &parts[answer_index] {
                        ExprPart::Infix(ExprInfix::TernaryQuestion(_)) => {
                            depth += 1;
                        }
                        ExprPart::Infix(ExprInfix::TernaryAnswer(_)) => {
                            if depth > 0 {
                                depth -= 1;
                            } else {
                                let condition =
                                    Self::from_parts(iter, &mut parts[..question_index]);
                                let case_true = Self::from_parts(
                                    iter,
                                    &mut parts[question_index + 1..answer_index],
                                );
                                let case_false =
                                    Self::from_parts(iter, &mut parts[answer_index + 1..]);
                                return Self::Ternary(Box::new((condition, case_true, case_false)));
                            }
                        }
                        _ => {}
                    }
                }
                panic!("Expr::from_parts chamado com um array inválido, tem um TernaryAnswer sem um TernaryQuestion correspondente");
            }
            ExprPart::Infix(ExprInfix::TernaryAnswer(_)) => {
                let answer_index = index;
                let mut depth: usize = 0;
                for question_index in (0..answer_index - 1).rev() {
                    match &parts[answer_index] {
                        ExprPart::Infix(ExprInfix::TernaryAnswer(_)) => {
                            depth += 1;
                        }
                        ExprPart::Infix(ExprInfix::TernaryQuestion(_)) => {
                            if depth > 0 {
                                depth -= 1;
                            } else {
                                let condition =
                                    Self::from_parts(iter, &mut parts[..question_index]);
                                let case_true = Self::from_parts(
                                    iter,
                                    &mut parts[question_index + 1..answer_index],
                                );
                                let case_false =
                                    Self::from_parts(iter, &mut parts[answer_index + 1..]);
                                return Self::Ternary(Box::new((condition, case_true, case_false)));
                            }
                        }
                        _ => {}
                    }
                }
                panic!("Expr::from_parts chamado com um array inválido, tem um TernaryQuestion sem um TernaryAnswer correspondente");
            }
            ExprPart::Infix(ExprInfix::Assign(infix)) => {
                let left = Self::from_parts(iter, &mut parts[..index]);
                let right = Self::from_parts(iter, &mut parts[index + 1..]);
                Self::Assign(Box::new((left.to_assignee(iter), infix, right)))
            }
            ExprPart::Infix(infix) => {
                let left = Self::from_parts(iter, &mut parts[..index]);
                let right = Self::from_parts(iter, &mut parts[index + 1..]);
                Self::Infix(Box::new((left, infix, right)))
            }
            ExprPart::Prefix(prefix) => {
                let value = Self::from_parts(iter, &mut parts[index + 1..]);
                parts[index] = ExprPart::Value(Self::Prefix(Box::new((prefix, value))));
                Self::from_parts(iter, &mut parts[..=index])
            }
            ExprPart::Postfix(ExprPostfix::Call(call_src, args)) => {
                if let ExprPart::Prefix(ExprPrefix::New(_)) = parts[0] {
                    let ExprPart::Prefix(ExprPrefix::New(new_src)) = parts[0].take() else {
                        unreachable!()
                    };
                    let src = iter.cover(new_src, call_src);
                    let value = Self::from_parts(iter, &mut parts[1..index]);
                    parts[index] = ExprPart::Value(Self::NewCall(src, Box::new((value, args))));
                    Self::from_parts(iter, &mut parts[index..])
                } else {
                    let value = Self::from_parts(iter, &mut parts[..index]);
                    parts[index] = ExprPart::Value(Self::Postfix(Box::new((value, ExprPostfix::Call(call_src, args)))));
                    Self::from_parts(iter, &mut parts[index..])
                }
            }
            ExprPart::Postfix(postfix) => {
                let value = Self::from_parts(iter, &mut parts[..index]);
                parts[index] = ExprPart::Value(Self::Postfix(Box::new((value, postfix))));
                Self::from_parts(iter, &mut parts[index..])
            }
            ExprPart::Value(_) => {
                panic!("Expr::from_parts chamado com um array inválido")
            }
            ExprPart::Taken => {
                panic!("Expr::from_parts chamado com um array contendo ExprPart::Taken")
            }
        }
    }
    /// retona um str que cobre todo o str, serve para dar erros mostrando a expressão correta
    fn src(&self, iter: &TokenIter<'a>) -> &'a src {
        match self {
            Expr::Undefined(src)
            | Expr::Null(src)
            | Expr::True(src)
            | Expr::False(src)
            | Expr::This(src)
            | Expr::Parameters(src)
            | Expr::NewTarget(src)
            | Expr::Identifier(src)
            | Expr::Number(src)
            | Expr::String(src)
            | Expr::Function { src, .. }
            | Expr::FunctionShort { src, .. }
            | Expr::FunctionExpr { src, .. }
            | Expr::Array(src, _)
            | Expr::Object(src, _)
            | Expr::NewCall(src, _) => src,
            Expr::Postfix(postfix) => iter.cover(postfix.0.src(iter), postfix.1.src()),
            Expr::Infix(infix) => iter.cover(infix.0.src(iter), infix.2.src(iter)),
            Expr::Prefix(prefix) => iter.cover(prefix.0.src(), prefix.1.src(iter)),
            Expr::Ternary(ternary) => iter.cover(ternary.0.src(iter), ternary.2.src(iter)),
            Expr::Assign(assign) => iter.cover(assign.0.src(), assign.2.src(iter)),
            Expr::Spread(src, spread) => iter.cover(src, spread.src(iter)),
        }
    }
    /// reinterpretar essa expressão como um Assignee, ao qual vamos setar um valor
    ///
    /// essa função não consome tokens, apenas adiciona novos erros, se necessário
    fn to_assignee(self, iter: &mut TokenIter<'a>) -> Assignee<'a> {
        match self {
            Expr::Identifier(identifier) => Assignee::Identifier(identifier),
            Expr::Array(src, array) => {
                let mut items = Vec::with_capacity(array.len());
                for item in array {
                    items.push(item.to_assignee(iter));
                }
                Assignee::Array(src, items)
            }
            Expr::Object(src, object) => {
                let mut pairs = Vec::with_capacity(object.len());
                for (key, value) in object {
                    pairs.push((key, value.to_assignee(iter)));
                }
                Assignee::Object(src, pairs)
            }
            Expr::Postfix(postfix) if matches!(postfix.1, ExprPostfix::Member(_, _)) => {
                let ExprPostfix::Member(src, member) = postfix.1 else {
                    unreachable!();
                };
                Assignee::Member(src, postfix.0, member)
            }
            Expr::Postfix(postfix) if matches!(postfix.1, ExprPostfix::Index(_, _)) => {
                let ExprPostfix::Index(src, expr) = postfix.1 else {
                    unreachable!();
                };
                Assignee::Index(src, postfix.0, expr)
            }
            Expr::Spread(src, spread) => Assignee::Rest(src, Box::new(spread.to_assignee(iter))),
            _ => {
                iter.error_at(
                    self.src(iter),
                    "Você não pode escrever um valor para esta expressão",
                );
                Assignee::Identifier("")
            }
        }
    }
}
impl<'a> ExprPart<'a> {
    fn take(&mut self) -> Self {
        std::mem::replace(self, ExprPart::Taken)
    }
    fn precedence(&self) -> u8 {
        match self {
            ExprPart::Taken => panic!("ExprPart::precedence foi chamado em um ExprPart::Taken"),
            ExprPart::Value(_) => u8::MAX,
            ExprPart::Prefix(prefix) => prefix.precedence(),
            ExprPart::Postfix(postfix) => postfix.precedence(),
            ExprPart::Infix(infix) => infix.precedence(),
        }
    }
}
impl<'a> ExprPrefix<'a> {
    fn precedence(&self) -> u8 {
        match self {
            ExprPrefix::Delete(_) => 14,
            ExprPrefix::Typeof(_) => 14,
            ExprPrefix::Void(_) => 14,
            ExprPrefix::New(_) => 16,
            ExprPrefix::BitwiseNot(_) => 14,
            ExprPrefix::Not(_) => 14,
            ExprPrefix::Await(_) => 14,
            ExprPrefix::UnaryPlus(_) => 14,
            ExprPrefix::UnaryMinus(_) => 14,
            ExprPrefix::PreIncrement(_) => 14,
            ExprPrefix::PreDecrement(_) => 14,
        }
    }
    fn src(&self) -> &'a src {
        match self {
            ExprPrefix::Delete(src)
            | ExprPrefix::Typeof(src)
            | ExprPrefix::Void(src)
            | ExprPrefix::New(src)
            | ExprPrefix::BitwiseNot(src)
            | ExprPrefix::Not(src)
            | ExprPrefix::Await(src)
            | ExprPrefix::UnaryPlus(src)
            | ExprPrefix::UnaryMinus(src)
            | ExprPrefix::PreIncrement(src)
            | ExprPrefix::PreDecrement(src) => src,
        }
    }
}
impl<'a> ExprPostfix<'a> {
    fn precedence(&self) -> u8 {
        match self {
            ExprPostfix::Call(_, _) => 17,
            ExprPostfix::Index(_, _) => 17,
            ExprPostfix::Member(_, _) => 17,
            ExprPostfix::MemberOpt(_, _) => 17,
            ExprPostfix::PostIncrement(_) => 15,
            ExprPostfix::PostDecrement(_) => 15,
        }
    }
    fn src(&self) -> &'a src {
        match self {
            ExprPostfix::Call(src, _)
            | ExprPostfix::Index(src, _)
            | ExprPostfix::Member(src, _)
            | ExprPostfix::MemberOpt(src, _)
            | ExprPostfix::PostIncrement(src)
            | ExprPostfix::PostDecrement(src) => src,
        }
    }
}
impl<'a> ExprInfix<'a> {
    fn precedence(&self) -> u8 {
        match self {
            ExprInfix::Power(_) => 13,
            ExprInfix::Multiply(_) => 12,
            ExprInfix::Divide(_) => 12,
            ExprInfix::Remainder(_) => 12,
            ExprInfix::Add(_) => 11,
            ExprInfix::Subtract(_) => 11,
            ExprInfix::BitwiseLeft(_) => 10,
            ExprInfix::BitwiseRight(_) => 10,
            ExprInfix::BitwiseLeftUnsigned(_) => 10,
            ExprInfix::Less(_) => 9,
            ExprInfix::LessEqual(_) => 9,
            ExprInfix::Greater(_) => 9,
            ExprInfix::GreaterEqual(_) => 9,
            ExprInfix::In(_) => 9,
            ExprInfix::InstanceOf(_) => 9,
            ExprInfix::Equal(_) => 8,
            ExprInfix::NotEqual(_) => 8,
            ExprInfix::Identical(_) => 8,
            ExprInfix::NotIdentical(_) => 8,
            ExprInfix::BitwiseAnd(_) => 7,
            ExprInfix::BitwiseXor(_) => 6,
            ExprInfix::BitwiseOr(_) => 5,
            ExprInfix::And(_) => 4,
            ExprInfix::Or(_) => 3,
            ExprInfix::Coalesce(_) => 3,
            ExprInfix::TernaryQuestion(_) => 2,
            ExprInfix::TernaryAnswer(_) => 2,
            ExprInfix::Assign(_) => 2,
            ExprInfix::Comma(_) => 1,
        }
    }
}

impl<'a> Deref for Statements<'a> {
    type Target = Vec<Statement<'a>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<'a> Deref for Patterns<'a> {
    type Target = Vec<Pattern<'a>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

fn parse_parethesis_expr<'a>(iter: &mut TokenIter<'a>) -> Expr<'a> {
    parse_token(iter, "(", "Esperado (");
    let expr = Expr::parse_one(iter);
    parse_token(iter, ")", "Esperado )");
    expr
}

fn parse_token<'a>(iter: &mut TokenIter<'a>, expect: &str, message: impl Into<Cow<'static, str>>) {
    match iter.peek() {
        Some(Token {
            text,
            space: _,
            ty: TokenType::Punct | TokenType::Keyword,
        }) if *text == expect => {
            iter.next();
        }
        _ => iter.error(message),
    }
}

fn parse_identifier<'a>(iter: &mut TokenIter<'a>) -> &'a str {
    if iter.ty() == Some(TokenType::Identifier) {
        iter.take_text()
    } else {
        iter.error("Esperado um identificador");
        ""
    }
}

fn parse_arrow(iter: &mut TokenIter<'_>) {
    if let Some(&Token {
        text: "=",
        space: TokenSpace::Joint,
        ty: TokenType::Punct,
    }) = iter.peek()
    {
        iter.next();
        if let Some(&Token {
            text: ">",
            space: _,
            ty: TokenType::Punct,
        }) = iter.peek()
        {
            iter.next();
            return;
        }
    }
    iter.error("Esperado =>");
}
