#![allow(unused)]
use std::collections::HashMap;

use crate::runtime::{instruction, Program};
use crate::syntax::{
    Assignee, Expr, ExprInfix, ExprInfixCompoundAssign, ExprPostfix, ExprPrefix, ForEachInit,
    Pattern, Patterns, Script, SimpleAssignee, Statement, Statements, VariableDeclarationType,
};
use crate::token::{parse_number, parse_string, ParsedNumber};
use crate::utils::{self, CompError};

/// o bit mais significante de u32, deveria ser uma constante de u32
const MSB32: u32 = 1 << (u32::BITS - 1);

type FuncId = u32;

/// representa o programa em construção
///
/// pode ser unido a outras partes
#[derive(Debug)]
struct Wip<'a> {
    /// todo o código fonte
    source: &'a str,
    /// todas as strings usadas até agora
    strings: Vec<u16>,
    /// o código da raiz gerado até agora / o código do corpo de uma função
    code: Vec<u8>,
    /// todos os erros emitidos até agora
    errors: Vec<CompError<'a>>,
    /// a representação estática da pilha
    stack: Vec<Declaration<'a>>,
    /// é o index do Declaration::Frame atual + 1, ou 0 se não houver frame
    frame: usize,
    /// a quantidade de argumentos no frame atual
    argc: usize,
    /// todas as funções que já foram completamente compiladas (faltando apenas o preenchimento dos simbolos de funções)
    compiled_functions: Vec<u8>,
    /// um map que indica onde estão as implementações de cada função em compiled_functions
    ///
    /// a chave é o ponteiro para o começo do código dela (é assim que diferenciamos função a de b)
    ///
    /// o valor é o index em compiled_functions
    ///
    /// se a função não tiver sido compilada ainda, ela não estará presente nesse HashMap
    compiled_functions_map: HashMap<FuncId, u32>,
    /// todos os lugares em code, que functions foram usadas
    ///
    /// se u32 >= 0, é um index em code
    ///
    /// senão, é a negação do index em compiled_functions
    function_uses: Vec<u32>,
}

#[derive(Debug, Clone)]
enum DeclLocation<'a> {
    /// a declaração vive no objeto global, em outras palavras, nenhuma declaração foi encontrada
    Global,
    /// representa um (%m)
    ///
    /// o objecto encontrado vive no frame atual
    ///
    /// valores positiovos indicam variáveis
    ///
    /// valores negativos indicam paramêtros
    Frame(i32, Declaration<'a>),
    /// valores no começo da pilha, acessíveis com um index absoluto
    Static(u32, Declaration<'a>),
    /// o objecto encontrado é uma função, é acessado via usa func id
    Function(FuncId),
    /// o objecto encontrado vive no frame de outra função (todo!() ainda não implementado)
    Capture,
}

#[derive(Debug, Clone)]
enum Symbol {
    Resolved(u32),
    PendingSome(u8, [u32; 7]),
    PendingMany(Vec<u32>),
}

#[derive(Debug, Clone)]
enum Declaration<'a> {
    Var(&'a str),
    Let(&'a str),
    Const(&'a str),
    /// o ponteiro para o começo do código da função
    Function(&'a str, FuncId),
    /// indica o começo de uma função, indica que as variáveis atrás desta só pode ser acessadas por captures
    Frame {
        /// o index do frame anterior + 1, ou 0 se não houver frame anterior
        frame: usize,
        /// o argc do frame anterior
        argc: usize,
    },
}

enum FunctionBody<'a, 'b> {
    Statements(&'b Statements<'a>),
    Expr(&'b Expr<'a>),
}

pub fn compile<'a>(prog: &Script<'a>) -> Result<Program, Vec<CompError<'a>>> {
    let mut wip = Wip {
        source: prog.code,
        strings: Vec::new(),
        code: Vec::new(),
        errors: Vec::new(),
        stack: Vec::new(),
        frame: 0,
        argc: 0,
        compiled_functions: Vec::new(),
        compiled_functions_map: HashMap::new(),
        function_uses: Vec::new(),
    };
    compile_scope_statements(&mut wip, &prog.body, true);
    for st in &prog.body {
        compile_statement(&mut wip, st);
    }
    if !matches!(
        prog.body.last(),
        Some(Statement::Return(_) | Statement::Throw(_, _))
    ) {
        wip.write_inst(instruction::UNDEFINED);
        wip.write_inst(instruction::QUIT);
    }
    if wip.errors.is_empty() {
        Ok(wip.to_byte_code())
    } else {
        Err(wip.errors)
    }
}

// COMPILE SCOPE //
// as funções compile_scope fazem uma passada rápida no código
// e declaram todas as variáveis (apenas que devem ser declaradas) na pilha
// ou seja let e const no scopo atual (estamos no scopo atual se local == true)
//
// var e atribuições (=) no nosso scopo ou qualquer sub-scopo
//
// e por fim declarações function (não expressões function)

fn compile_scope_statements<'a>(wip: &mut Wip<'a>, sts: &[Statement<'a>], local: bool) {
    for st in sts {
        compile_scope_statement(wip, st, local);
    }
}
fn compile_scope_statement<'a>(wip: &mut Wip<'a>, st: &Statement<'a>, local: bool) {
    match st {
        Statement::Function { src, name, .. } => match wip.search_stack(name) {
            DeclLocation::Frame(_, Declaration::Var(_)) | _ => {
                wip.stack
                    .push(Declaration::Function(name, wip.get_func_id(src)));
            }
            DeclLocation::Frame(_, _) | DeclLocation::Function(_) => {
                wip.errors.push(CompError {
                    src: name,
                    desc: "Este identificador já foi declarado".into(),
                });
            }
        },
        Statement::VarDecl(vardecl) => {
            for decl in &vardecl.decls.0 {
                compile_scope_variable_declaration(wip, &vardecl.decltype, &decl, local);
            }
        }
        Statement::Expr(_) => {}
        Statement::Return(_) => {}
        Statement::Break(_) => {}
        Statement::Continue(_) => {}
        Statement::If { body, branch, .. } => {
            compile_scope_statements(wip, &body, false);
            if let Some(branch) = branch {
                compile_scope_statement(wip, &**branch, false);
            }
        }
        Statement::While { body, .. } => {
            compile_scope_statements(wip, &body, false);
        }
        Statement::Do { body, .. } => {
            compile_scope_statements(wip, &body, false);
        }
        Statement::ForExpr { body, .. } => {
            compile_scope_statements(wip, &body, false);
        }
        Statement::ForVarDecl { init, body, .. } => {
            for decl in &init.decls.0 {
                compile_scope_variable_declaration(wip, &init.decltype, &decl, false);
            }
            compile_scope_statements(wip, &body, false);
        }
        Statement::ForEach { init, body, .. } => {
            if let ForEachInit::VarDecl { decltype, decls } = init {
                compile_scope_variable_declaration(wip, decltype, decls, false);
            }
            compile_scope_statements(wip, &body, false);
        }
        Statement::Switch { body, .. } => {
            compile_scope_statements(wip, &body, false);
        }
        Statement::Case(_, _) => {}
        Statement::Throw(_, _) => {}
        Statement::Try {
            src: _,
            body,
            catch,
            finally,
        } => {
            compile_scope_statements(wip, &body, false);
            if let Some((_, catch)) = catch {
                compile_scope_statements(wip, &catch, false);
            }
            if let Some(finally) = finally {
                compile_scope_statements(wip, &finally, false);
            }
        }
        Statement::Scope(body) => compile_scope_statements(wip, &body, local),
        Statement::Debugger(_) => {}
    }
}
fn compile_scope_variable_declaration<'a>(
    wip: &mut Wip<'a>,
    decltype: &VariableDeclarationType<'a>,
    pat: &Pattern<'a>,
    local: bool,
) {
    // as declarações de let e const não vazam para scopes superiores
    match decltype {
        VariableDeclarationType::Let(_) | VariableDeclarationType::Const(_) => {
            if !local {
                // as declarações de let e const não vazam para scopes superiores
                return;
            }
            pat.for_each_declared(&mut |name| {
                if let DeclLocation::Frame(_, _) | DeclLocation::Function(_) =
                    wip.search_stack(name)
                {
                    wip.errors.push(CompError {
                        src: name,
                        desc: "Este identificador já foi declarado".into(),
                    });
                } else {
                    match decltype {
                        VariableDeclarationType::Var(_) => unreachable!(),
                        VariableDeclarationType::Let(_) => {
                            wip.stack.push(Declaration::Let(name));
                            if wip.frame != 0 {
                                wip.code.push(instruction::UNDEFINED);
                            }
                        }
                        VariableDeclarationType::Const(_) => {
                            wip.stack.push(Declaration::Const(name));
                            if wip.frame != 0 {
                                wip.code.push(instruction::UNDEFINED);
                            }
                        }
                    }
                }
            });
        }
        VariableDeclarationType::Var(_) => {
            pat.for_each_declared(&mut |name| match wip.search_stack(name) {
                DeclLocation::Frame(_, Declaration::Var(_))
                | DeclLocation::Static(_, Declaration::Var(_))
                | _ => {
                    wip.stack.push(Declaration::Var(name));
                    if wip.frame != 0 {
                        wip.code.push(instruction::UNDEFINED);
                    } else {
                        wip.write_inst(instruction::ASSIGN_STATIC_UNDEFINED);
                        wip.write_u32(wip.stack.len() as u32 - 1);
                    }
                }
                _ => {
                    wip.errors.push(CompError {
                        src: name,
                        desc: "Este identificador já foi declarado".into(),
                    });
                }
            });
        }
    }
}

fn compile_statements<'a>(wip: &mut Wip<'a>, sts: &[Statement<'a>]) {
    let old_stack_len = wip.stack_len_save();
    compile_scope_statements(wip, sts, true);
    for st in sts {
        compile_statement(wip, st);
    }
    wip.write_stack_len_restore(old_stack_len);
}

fn compile_statement<'a>(wip: &mut Wip<'a>, st: &Statement<'a>) {
    match st {
        Statement::Function {
            src,
            is_async,
            name,
            args,
            body,
        } => {
            compile_function(
                wip,
                *src,
                *is_async,
                Some(*name),
                args,
                FunctionBody::Statements(body),
            );
        }
        Statement::VarDecl(decls) => {
            for pat in &decls.decls.0 {
                compile_variable_declaration(wip, &decls.decltype, pat);
            }
        }
        Statement::Expr(expr) => {
            compile_expression(wip, expr);
            wip.write_inst(instruction::POP);
        }
        Statement::Return(expr) => {
            if let Some(expr) = expr {
                compile_expression(wip, expr);
            } else {
                wip.write_inst(instruction::UNDEFINED);
            }
            if wip.frame != 0 {
                match wip.frame_size() {
                    0 => {
                        wip.write_inst(instruction::RETURN);
                    }
                    frame_size => {
                        wip.write_inst(instruction::RETURN_POP);
                        wip.write_u32(frame_size);
                    }
                }
            } else {
                wip.write_inst(instruction::QUIT);
            }
        }
        Statement::Break(_) => todo!("break"),
        Statement::Continue(_) => todo!("continue"),
        Statement::If {
            src,
            condition,
            body,
            branch,
        } => {
            if let Some(branch) = branch {
                // if else
                let mut symbol_to_else = Symbol::new();
                let mut symbol_skip_else = Symbol::new();
                compile_expression(wip, condition);
                wip.write_inst(instruction::JPF);
                wip.write_symbol(&mut symbol_to_else);
                compile_statements(wip, &body);
                wip.write_inst(instruction::JMP);
                wip.write_symbol(&mut symbol_skip_else);
                wip.define_symbol(&mut symbol_to_else);
                compile_statement(wip, &branch);
                wip.define_symbol(&mut symbol_skip_else);
            } else {
                let mut symbol = Symbol::new();
                compile_expression(wip, condition);
                wip.write_inst(instruction::JPF);
                wip.write_symbol(&mut symbol);
                compile_statements(wip, &body);
                wip.define_symbol(&mut symbol);
            }
        }
        Statement::While {
            src,
            condition,
            body,
        } => {
            let mut symbol_loop = Symbol::new();
            let mut symbol_exit = Symbol::new();
            wip.define_symbol(&mut symbol_loop);
            compile_expression(wip, condition);
            wip.write_inst(instruction::JPF);
            wip.write_symbol(&mut symbol_exit);
            compile_statements(wip, &body);
            wip.write_inst(instruction::JMP);
            wip.write_symbol(&mut symbol_loop);
            wip.define_symbol(&mut symbol_exit);
        }
        Statement::Do {
            src,
            body,
            condition,
        } => {
            let mut symbol_loop = Symbol::new();
            wip.define_symbol(&mut symbol_loop);
            compile_statements(wip, &body);
            compile_expression(wip, condition);
            wip.write_inst(instruction::JPT);
            wip.write_symbol(&mut symbol_loop);
        }
        Statement::ForExpr {
            src: _,
            init,
            condition,
            increment,
            body,
        } => {
            let mut symbol_loop = Symbol::new();
            let mut symbol_exit = Symbol::new();
            compile_expression(wip, init);
            wip.write_inst(instruction::POP);
            wip.define_symbol(&mut symbol_loop);
            compile_expression(wip, condition);
            wip.write_inst(instruction::JPF);
            wip.write_symbol(&mut symbol_exit);
            compile_statements(wip, &body);
            compile_expression(wip, increment);
            wip.write_inst(instruction::POP);
            wip.write_inst(instruction::JMP);
            wip.write_symbol(&mut symbol_loop);
            wip.define_symbol(&mut symbol_exit);
        }
        Statement::ForVarDecl {
            src: _,
            init,
            condition,
            increment,
            body,
        } => {
            let mut symbol_loop = Symbol::new();
            let mut symbol_exit = Symbol::new();
            let old_stack_len = wip.stack_len_save();
            for pat in &init.decls.0 {
                compile_scope_variable_declaration(wip, &init.decltype, pat, true);
            }
            for pat in &init.decls.0 {
                compile_variable_declaration(wip, &init.decltype, pat);
            }
            wip.define_symbol(&mut symbol_loop);
            compile_expression(wip, condition);
            wip.write_inst(instruction::JPF);
            wip.write_symbol(&mut symbol_exit);
            compile_statements(wip, &body);
            compile_expression(wip, increment);
            wip.write_inst(instruction::POP);
            wip.write_inst(instruction::JMP);
            wip.write_symbol(&mut symbol_loop);
            wip.define_symbol(&mut symbol_exit);
            wip.write_stack_len_restore(old_stack_len);
        }
        Statement::ForEach {
            src,
            forty,
            init,
            iterator,
            body,
        } => todo!("foreach"),
        Statement::Switch { src, expr, body } => todo!("switch"),
        Statement::Case(_, _) => todo!("case"),
        Statement::Throw(_, _) => todo!("throw"),
        Statement::Try {
            src,
            body,
            catch,
            finally,
        } => todo!("try"),
        Statement::Debugger(_) => wip.write_inst(instruction::DEBUG),
        Statement::Scope(scope) => compile_statements(wip, scope),
    }
}
fn compile_variable_declaration<'a>(
    wip: &mut Wip<'a>,
    decltype: &VariableDeclarationType<'a>,
    pat: &Pattern<'a>,
) {
    match pat {
        Pattern::Identifier(name, expr) => {
            let loc = wip.search_stack(*name);
            if let Some(expr) = expr {
                compile_expression(wip, expr);
                match loc {
                    DeclLocation::Global => panic!("compile_scope deveria ter criado a declaração"),
                    DeclLocation::Frame(index, _) => {
                        wip.write_inst(instruction::ASSIGN);
                        wip.write_i32(index);
                        wip.write_inst(instruction::POP);
                    },
                    DeclLocation::Static(index, _) => {
                        wip.write_inst(instruction::ASSIGN_STATIC);
                        wip.write_u32(index);
                        wip.write_inst(instruction::POP);
                    },
                    DeclLocation::Function(_) => unreachable!("declaração de uma variável esta tentando se salvar em uma função?"),
                    DeclLocation::Capture => unreachable!("declaração de uma variável esta tentando se salvar em um capature?"),
                }
            }
        }
        Pattern::Array(_, _, _) | Pattern::Object(_, _, _) => todo!("Desestruturação"),
        Pattern::Rest(_, _) => unreachable!(
            "Uma declaração de variável não pode ser desestruturada apenas com ..."
        ),
    }
}
/// retorna quantas das expressões não são spread
fn compile_expressions<'a>(wip: &mut Wip<'a>, exprs: &[Expr<'a>]) -> u32 {
    let mut count = 0;
    for expr in exprs {
        if !matches!(expr, Expr::Spread(_, _)) {
            count += 1;
        }
        compile_expression(wip, expr);
    }
    count
}
fn compile_expression<'a>(wip: &mut Wip<'a>, expr: &Expr<'a>) {
    match expr {
        Expr::Error => wip.write_inst(instruction::UNDEFINED),
        Expr::Null(_) => wip.write_inst(instruction::NULL),
        Expr::True(_) => wip.write_inst(instruction::TRUE),
        Expr::False(_) => wip.write_inst(instruction::FALSE),
        Expr::This(_) => wip.write_inst(instruction::THIS),
        Expr::NewTarget(_) => wip.write_inst(instruction::NEW_TARGET),
        Expr::Identifier(name) => match wip.search_stack(name) {
            DeclLocation::Global if *name == "undefined" => {
                wip.write_inst(instruction::UNDEFINED);
            }
            DeclLocation::Global => {
                wip.write_inst(instruction::GLOBAL_MEMBER);
                wip.write_str(name);
            }
            DeclLocation::Frame(index, _) => {
                wip.write_inst(instruction::PUSH);
                wip.write_i32(index);
            }
            DeclLocation::Static(index, _) => {
                wip.write_inst(instruction::STATIC);
                wip.write_u32(index);
            }
            DeclLocation::Function(funcid) => {
                wip.write_inst(instruction::FUNCTION);
                wip.write_funcid(funcid);
            }
            DeclLocation::Capture => {
                todo!("captura de variáveis ainda não foi implementada")
            }
        },
        Expr::Number(number) => compile_number(wip, number),
        Expr::String(string) => compile_string(wip, string),
        Expr::Function {
            src,
            is_async,
            name,
            args,
            body,
        } => {
            let funcid = compile_function(
                wip,
                src,
                *is_async,
                *name,
                args,
                FunctionBody::Statements(body),
            );
            wip.write_inst(instruction::FUNCTION);
            wip.write_funcid(funcid);
        }
        Expr::FunctionShort {
            src,
            is_async,
            args,
            body,
        } => {
            let funcid = compile_function(
                wip,
                src,
                *is_async,
                None,
                args,
                FunctionBody::Statements(body),
            );
            wip.write_inst(instruction::FUNCTION);
            wip.write_funcid(funcid);
        }
        Expr::FunctionExpr {
            src,
            is_async,
            args,
            body,
        } => {
            let funcid =
                compile_function(wip, src, *is_async, None, args, FunctionBody::Expr(body));
            wip.write_inst(instruction::FUNCTION);
            wip.write_funcid(funcid);
        }
        Expr::Array(_, items) => {
            let argc = compile_expressions(wip, &items);
            wip.write_inst(instruction::ARRAY);
            wip.write_u32(argc);
        }
        Expr::Object(_, pairs) => {
            wip.write_inst(instruction::OBJECT);
            for (key, value) in pairs {
                compile_expression(wip, value);
                wip.write_inst(instruction::OBJECT_MEMBER);
                wip.write_str(key);
            }
        }
        Expr::NewCall(_, newcall) => {
            let argc = compile_expressions(wip, &newcall.1);
            compile_expression(wip, &newcall.0);
            wip.write_inst(instruction::CALL_NEW);
            wip.write_u32(argc);
        }
        Expr::PreIncrement(_, assignee) => match &**assignee {
            SimpleAssignee::Identifier(name) => match wip.search_stack(*name) {
                DeclLocation::Frame(_, Declaration::Const(_))
                | DeclLocation::Static(_, Declaration::Const(_))
                | DeclLocation::Function(_) => todo!("erro: não é possível modificar esse valor"),
                DeclLocation::Global => {
                    wip.write_inst(instruction::GLOBAL_MEMBER);
                    wip.write_str(*name);
                    wip.write_inst(instruction::INCREMENT);
                    wip.write_inst(instruction::ASSIGN_GLOBAL);
                    wip.write_str(*name);
                }
                DeclLocation::Frame(index, _) => {
                    wip.write_inst(instruction::PRE_INCREMENT);
                    wip.write_i32(index);
                }
                DeclLocation::Static(index, _) => {
                    wip.write_inst(instruction::PRE_INCREMENT_STATIC);
                    wip.write_u32(index);
                }
                DeclLocation::Capture => todo!("capture"),
            },
            SimpleAssignee::Member(_, expr, member) => {
                compile_expression(wip, expr);
                wip.write_inst(instruction::DUPE);
                wip.write_inst(instruction::MEMBER);
                wip.write_str(*member);
                wip.write_inst(instruction::INCREMENT);
                wip.write_inst(instruction::ASSIGN_MEMBER);
                wip.write_str(*member);
            }
            SimpleAssignee::Index(_, expr, index) => {
                compile_expression(wip, expr);
                compile_expression(wip, index);
                wip.write_inst(instruction::DUPE_PAIR);
                wip.write_inst(instruction::INDEX);
                wip.write_inst(instruction::INCREMENT);
                wip.write_inst(instruction::ASSIGN_INDEX);
            }
        },
        Expr::PreDecrement(_, assignee) => match &**assignee {
            SimpleAssignee::Identifier(name) => match wip.search_stack(*name) {
                DeclLocation::Frame(_, Declaration::Const(_))
                | DeclLocation::Static(_, Declaration::Const(_))
                | DeclLocation::Function(_) => todo!("erro: não é possível modificar esse valor"),
                DeclLocation::Global => {
                    wip.write_inst(instruction::GLOBAL_MEMBER);
                    wip.write_str(*name);
                    wip.write_inst(instruction::DECREMENT);
                    wip.write_inst(instruction::ASSIGN_GLOBAL);
                    wip.write_str(*name);
                }
                DeclLocation::Frame(index, _) => {
                    wip.write_inst(instruction::PRE_DECREMENT);
                    wip.write_i32(index);
                }
                DeclLocation::Static(index, _) => {
                    wip.write_inst(instruction::PRE_DECREMENT_STATIC);
                    wip.write_u32(index);
                }
                DeclLocation::Capture => todo!("capture"),
            },
            SimpleAssignee::Member(_, expr, member) => {
                compile_expression(wip, expr);
                wip.write_inst(instruction::DUPE);
                wip.write_inst(instruction::MEMBER);
                wip.write_str(*member);
                wip.write_inst(instruction::DECREMENT);
                wip.write_inst(instruction::ASSIGN_MEMBER);
                wip.write_str(*member);
            }
            SimpleAssignee::Index(_, expr, index) => {
                compile_expression(wip, expr);
                compile_expression(wip, index);
                wip.write_inst(instruction::DUPE_PAIR);
                wip.write_inst(instruction::INDEX);
                wip.write_inst(instruction::DECREMENT);
                wip.write_inst(instruction::ASSIGN_INDEX);
            }
        },
        Expr::PostIncrement(_, assignee) => match &**assignee {
            SimpleAssignee::Identifier(name) => match wip.search_stack(*name) {
                DeclLocation::Frame(_, Declaration::Const(_))
                | DeclLocation::Static(_, Declaration::Const(_))
                | DeclLocation::Function(_) => todo!("erro: não é possível modificar esse valor"),
                DeclLocation::Global => {
                    wip.write_inst(instruction::GLOBAL_MEMBER);
                    wip.write_str(*name);
                    wip.write_inst(instruction::INCREMENT);
                    wip.write_inst(instruction::ASSIGN_GLOBAL);
                    wip.write_str(*name);
                    wip.write_inst(instruction::DECREMENT);
                }
                DeclLocation::Frame(index, _) => {
                    wip.write_inst(instruction::POST_INCREMENT);
                    wip.write_i32(index);
                }
                DeclLocation::Static(index, _) => {
                    wip.write_inst(instruction::POST_INCREMENT_STATIC);
                    wip.write_u32(index);
                }
                DeclLocation::Capture => todo!("capture"),
            },
            SimpleAssignee::Member(_, expr, member) => {
                compile_expression(wip, expr);
                wip.write_inst(instruction::DUPE);
                wip.write_inst(instruction::MEMBER);
                wip.write_str(*member);
                wip.write_inst(instruction::INCREMENT);
                wip.write_inst(instruction::ASSIGN_MEMBER);
                wip.write_str(*member);
                wip.write_inst(instruction::DECREMENT);
            }
            SimpleAssignee::Index(_, expr, index) => {
                compile_expression(wip, expr);
                compile_expression(wip, index);
                wip.write_inst(instruction::DUPE_PAIR);
                wip.write_inst(instruction::INDEX);
                wip.write_inst(instruction::INCREMENT);
                wip.write_inst(instruction::ASSIGN_INDEX);
                wip.write_inst(instruction::DECREMENT);
            }
        },
        Expr::PostDecrement(_, assignee) => match &**assignee {
            SimpleAssignee::Identifier(name) => match wip.search_stack(*name) {
                DeclLocation::Frame(_, Declaration::Const(_))
                | DeclLocation::Static(_, Declaration::Const(_))
                | DeclLocation::Function(_) => todo!("erro: não é possível modificar esse valor"),
                DeclLocation::Global => {
                    wip.write_inst(instruction::GLOBAL_MEMBER);
                    wip.write_str(*name);
                    wip.write_inst(instruction::DECREMENT);
                    wip.write_inst(instruction::ASSIGN_GLOBAL);
                    wip.write_str(*name);
                    wip.write_inst(instruction::INCREMENT);
                }
                DeclLocation::Frame(index, _) => {
                    wip.write_inst(instruction::POST_DECREMENT);
                    wip.write_i32(index);
                }
                DeclLocation::Static(index, _) => {
                    wip.write_inst(instruction::POST_DECREMENT_STATIC);
                    wip.write_u32(index);
                }
                DeclLocation::Capture => todo!("capture"),
            },
            SimpleAssignee::Member(_, expr, member) => {
                compile_expression(wip, expr);
                wip.write_inst(instruction::DUPE);
                wip.write_inst(instruction::MEMBER);
                wip.write_str(*member);
                wip.write_inst(instruction::DECREMENT);
                wip.write_inst(instruction::ASSIGN_MEMBER);
                wip.write_str(*member);
                wip.write_inst(instruction::INCREMENT);
            }
            SimpleAssignee::Index(_, expr, index) => {
                compile_expression(wip, expr);
                compile_expression(wip, index);
                wip.write_inst(instruction::DUPE_PAIR);
                wip.write_inst(instruction::INDEX);
                wip.write_inst(instruction::DECREMENT);
                wip.write_inst(instruction::ASSIGN_INDEX);
                wip.write_inst(instruction::INCREMENT);
            }
        },
        Expr::Postfix(postfix) => {
            match &postfix.1 {
                ExprPostfix::Call(_, args) => {
                    let argc = compile_expressions(wip, args);
                    if let Expr::Postfix(subpostfix) = &postfix.0 {
                        match &subpostfix.1 {
                            ExprPostfix::Index(_, index_expr) => {
                                // chamada com colchete, expresão antes do colchete é this
                                compile_expression(wip, &subpostfix.0); // push this
                                wip.write_inst(instruction::DUPE);
                                compile_expression(wip, &index_expr);
                                wip.write_inst(instruction::INDEX); // push func
                            },
                            ExprPostfix::Member(_, member) => {
                                // chamada com ponto, expresão antes do ponto é this
                                compile_expression(wip, &subpostfix.0); // push this
                                wip.write_inst(instruction::DUPE);
                                wip.write_inst(instruction::MEMBER); // push func
                                wip.write_str(member);
                            },
                            _ => {
                                // chamada normal, this é undefined
                                wip.write_inst(instruction::UNDEFINED); // push this
                                compile_expression(wip, &postfix.0); // push func
                            }
                        }
                    } else {
                        // chamada normal, this é undefined
                        wip.write_inst(instruction::UNDEFINED); // push this
                        compile_expression(wip, &postfix.0); // push func
                    }
                    wip.write_inst(instruction::CALL);
                    wip.write_u32(argc);
                },
                ExprPostfix::Index(_, expr) => {
                    compile_expression(wip, &postfix.0);
                    compile_expression(wip, expr);
                    wip.write_inst(instruction::INDEX);
                },
                ExprPostfix::Member(_, name) => {
                    compile_expression(wip, &postfix.0);
                    wip.write_inst(instruction::MEMBER);
                    wip.write_str(name);
                },
                ExprPostfix::PostIncrement(_) => unreachable!("ExprPrefix::PostIncrement deveria ter sido transformado em Expr::PostIncrement"),
                ExprPostfix::PostDecrement(_) => unreachable!("ExprPrefix::PostDecrement deveria ter sido transformado em Expr::PostDecrement"),
            }
        }
        Expr::Infix(infix) => {
            compile_expression(wip, &infix.0);
            use crate::runtime::instruction::*;
            let infix_instruction = match &infix.1 {
                ExprInfix::And(_) => {
                    let mut symbol = Symbol::new();
                    wip.write_inst(instruction::JKF);
                    wip.write_symbol(&mut symbol);
                    wip.write_inst(instruction::POP);
                    compile_expression(wip, &infix.2);
                    wip.define_symbol(&mut symbol);
                    return;
                },
                ExprInfix::Or(_) => {
                    let mut symbol = Symbol::new();
                    wip.write_inst(instruction::JKT);
                    wip.write_symbol(&mut symbol);
                    wip.write_inst(instruction::POP);
                    compile_expression(wip, &infix.2);
                    wip.define_symbol(&mut symbol);
                    return;
                },
                ExprInfix::Coalesce(_) => {
                    let mut symbol = Symbol::new();
                    wip.write_inst(instruction::JKD);
                    wip.write_symbol(&mut symbol);
                    wip.write_inst(instruction::POP);
                    compile_expression(wip, &infix.2);
                    wip.define_symbol(&mut symbol);
                    return;
                },
                ExprInfix::TernaryQuestion(_) => unreachable!("ExprInfix::TernaryQuestion deveria ter sido transformado em Expr::Ternary"),
                ExprInfix::TernaryAnswer(_) => unreachable!("ExprInfix::TernaryAnswer deveria ter sido transformado em Expr::Ternary"),
                ExprInfix::Power(_) => POWER,
                ExprInfix::Multiply(_) => MULTIPLY,
                ExprInfix::Divide(_) => DIVIDE,
                ExprInfix::Remainder(_) => REMAINDER,
                ExprInfix::Add(_) => ADD,
                ExprInfix::Subtract(_) => SUBTRACT,
                ExprInfix::BitwiseLeft(_) => BITWISE_LEFT,
                ExprInfix::BitwiseRight(_) => BITWISE_RIGHT,
                ExprInfix::BitwiseRightUnsigned(_) => BITWISE_RIGHT_UNSIGNED,
                ExprInfix::Less(_) => LESS,
                ExprInfix::LessEqual(_) => LESS_EQUAL,
                ExprInfix::Greater(_) => GREATER,
                ExprInfix::GreaterEqual(_) => GREATER_EQUAL,
                ExprInfix::In(_) => IN,
                ExprInfix::InstanceOf(_) => INSTANCEOF,
                ExprInfix::Equal(_) => EQUAL,
                ExprInfix::NotEqual(_) => NOT_EQUAL,
                ExprInfix::Identical(_) => IDENTICAL,
                ExprInfix::NotIdentical(_) => NOT_IDENTICAL,
                ExprInfix::BitwiseAnd(_) => BITWISE_AND,
                ExprInfix::BitwiseXor(_) => BITWISE_XOR,
                ExprInfix::BitwiseOr(_) => BITWISE_OR,
                ExprInfix::Assign(_) => unreachable!("ExprInfix::Assign deveria ter sido transformado em Expr::Assign"),
                ExprInfix::CompoundAssign(_) => unreachable!("ExprInfix::CompoundAssign deveria ter sido transformado em Expr::CompoundAssign"),
                ExprInfix::Comma(_) => COMMA,
            };
            compile_expression(wip, &infix.2);
            wip.write_inst(infix_instruction);
        }
        Expr::Prefix(prefix) => {
            if let ExprPrefix::Delete(_) = prefix.0 {
                match &prefix.1 {
                    Expr::Identifier(name) => {
                        if let DeclLocation::Global = wip.search_stack(name) {
                            wip.write_inst(instruction::DELETE_GLOBAL);
                        } else {
                            // queremos apagar um identificador explicitamente declarado
                            // o comportamento correto aqui é retornar false
                            wip.write_inst(instruction::FALSE);
                        }
                        return;
                    }
                    Expr::Postfix(postfix) => match &postfix.1 {
                        ExprPostfix::Index(_, expr) => {
                            compile_expression(wip, &postfix.0);
                            compile_expression(wip, expr);
                            wip.write_inst(instruction::DELETE_INDEX);
                            return;
                        }
                        ExprPostfix::Member(_, member) => {
                            compile_expression(wip, &postfix.0);
                            wip.write_inst(instruction::DELETE_MEMBER);
                            wip.write_str(&member);
                            return;
                        }
                        _ => {}
                    },
                    _ => {}
                }
            } else if let ExprPrefix::New(_) = prefix.0 {
                compile_expression(wip, &prefix.1);
                wip.write_inst(instruction::CALL_NEW);
                wip.write_u32(0);
            } else {
                compile_expression(wip, &prefix.1);
                use crate::runtime::instruction::*;
                let prefix_instruction = match prefix.0 {
                    ExprPrefix::Delete(_) => unreachable!(),
                    ExprPrefix::Typeof(_) => TYPEOF,
                    ExprPrefix::Void(_) => VOID,
                    ExprPrefix::New(_) => unreachable!(),
                    ExprPrefix::BitwiseNot(_) => BITWISE_NOT,
                    ExprPrefix::Not(_) => NOT,
                    ExprPrefix::Await(_) => AWAIT,
                    ExprPrefix::UnaryPlus(_) => UNARY_PLUS,
                    ExprPrefix::UnaryMinus(_) => UNARY_MINUS,
                    ExprPrefix::PreIncrement(_) => unreachable!("ExprPrefix::PreIncrement deveria ter sido transformado em Expr::PreIncrement"),
                    ExprPrefix::PreDecrement(_) => unreachable!("ExprPrefix::PreDecrement deveria ter sido transformado em Expr::PreDecrement"),
                };
                wip.write_inst(prefix_instruction);
            }
        }
        Expr::Ternary(ternary) => {
            let mut to_branch = Symbol::new();
            let mut skip_branch = Symbol::new();
            compile_expression(wip, &ternary.0);
            wip.write_inst(instruction::JPF);
            wip.write_symbol(&mut to_branch);
            compile_expression(wip, &ternary.1);
            wip.write_inst(instruction::JMP);
            wip.write_symbol(&mut skip_branch);
            wip.define_symbol(&mut to_branch);
            compile_expression(wip, &ternary.2);
            wip.define_symbol(&mut skip_branch);
        }
        Expr::Assign(assign) => match &assign.0 {
            Assignee::Identifier(name) => match wip.search_stack(name) {
                DeclLocation::Frame(_, Declaration::Const(_))
                | DeclLocation::Static(_, Declaration::Const(_))
                | DeclLocation::Function(_) => todo!("erro: não é possível modificar esse valor"),
                DeclLocation::Global => {
                    compile_expression(wip, &assign.1);
                    wip.write_inst(instruction::ASSIGN_GLOBAL);
                    wip.write_str(name);
                }
                DeclLocation::Frame(index, _) => {
                    compile_expression(wip, &assign.1);
                    wip.write_inst(instruction::ASSIGN);
                    wip.write_i32(index);
                }
                DeclLocation::Static(index, _) => {
                    compile_expression(wip, &assign.1);
                    wip.write_inst(instruction::ASSIGN_STATIC);
                    wip.write_u32(index);
                }
                DeclLocation::Capture => todo!("capture"),
            },
            Assignee::Member(_, expr, member) => {
                compile_expression(wip, expr);
                compile_expression(wip, &assign.1);
                wip.write_inst(instruction::ASSIGN_MEMBER);
                wip.write_str(*member);
            }
            Assignee::Index(_, expr, index) => {
                compile_expression(wip, expr);
                compile_expression(wip, index);
                compile_expression(wip, &assign.1);
                wip.write_inst(instruction::ASSIGN_INDEX);
            }
            Assignee::Array(_, _) => todo!("destructuring"),
            Assignee::Object(_, _) => todo!("destructuring"),
            Assignee::Rest(_, _) => todo!("destructuring"),
        },
        Expr::CompoundAssign(assign) => {
            let mut symbol = None;
            /// esse número indica quantos valores foram criados dentro desse match
            let length = match &assign.0 {
                SimpleAssignee::Identifier(name) => match wip.search_stack(name) {
                    DeclLocation::Global => {
                        wip.write_inst(instruction::GLOBAL_MEMBER);
                        wip.write_str(name);
                        1
                    }
                    DeclLocation::Frame(index, _) => {
                        wip.write_inst(instruction::PUSH);
                        wip.write_i32(index);
                        1
                    }
                    DeclLocation::Static(index, _) => {
                        wip.write_inst(instruction::STATIC);
                        wip.write_u32(index);
                        1
                    }
                    DeclLocation::Function(_) => todo!("erro: não é possível modificar esse valor"),
                    DeclLocation::Capture => todo!("capture"),
                },
                SimpleAssignee::Member(_, expr, member) => {
                    compile_expression(wip, expr);
                    wip.write_inst(instruction::DUPE);
                    wip.write_inst(instruction::MEMBER);
                    wip.write_str(*member);
                    2
                }
                SimpleAssignee::Index(_, expr, index) => {
                    compile_expression(wip, expr);
                    compile_expression(wip, index);
                    wip.write_inst(instruction::DUPE_PAIR);
                    wip.write_inst(instruction::INDEX);
                    4
                }
            };
            let instruction = match &assign.1 {
                ExprInfixCompoundAssign::Power(_) => Some(instruction::POWER),
                ExprInfixCompoundAssign::Add(_) => Some(instruction::ADD),
                ExprInfixCompoundAssign::Subtract(_) => Some(instruction::SUBTRACT),
                ExprInfixCompoundAssign::Multiply(_) => Some(instruction::MULTIPLY),
                ExprInfixCompoundAssign::Divide(_) => Some(instruction::DIVIDE),
                ExprInfixCompoundAssign::Remainder(_) => Some(instruction::REMAINDER),
                ExprInfixCompoundAssign::BitwiseLeft(_) => Some(instruction::BITWISE_LEFT),
                ExprInfixCompoundAssign::BitwiseRight(_) => Some(instruction::BITWISE_RIGHT),
                ExprInfixCompoundAssign::BitwiseRightUnsigned(_) => {
                    Some(instruction::BITWISE_RIGHT_UNSIGNED)
                }
                ExprInfixCompoundAssign::BitwiseAnd(_) => Some(instruction::BITWISE_AND),
                ExprInfixCompoundAssign::BitwiseXor(_) => Some(instruction::BITWISE_XOR),
                ExprInfixCompoundAssign::BitwiseOr(_) => Some(instruction::BITWISE_OR),
                ExprInfixCompoundAssign::Coalesce(_) => {
                    if length == 1 {
                        let mut symbol_skip = Symbol::new();
                        wip.write_inst(instruction::JKD);
                        wip.write_symbol(&mut symbol_skip);
                        wip.write_inst(instruction::POP);
                        symbol = Some(symbol_skip);
                    } else {
                        let mut symbol_assign = Symbol::new();
                        let mut symbol_skip = Symbol::new();
                        wip.write_inst(instruction::JKN);
                        wip.write_symbol(&mut symbol_assign);
                        wip.write_inst(instruction::POP_RETAIN);
                        wip.write_u32(length - 1);
                        wip.write_inst(instruction::JMP);
                        wip.write_symbol(&mut symbol_skip);
                        wip.define_symbol(&mut symbol_assign);
                        wip.write_inst(instruction::POP);
                        symbol = Some(symbol_skip);
                    }
                    compile_expression(wip, &assign.2);
                    None
                }
            };
            if let Some(instruction) = instruction {
                compile_expression(wip, &assign.2);
                wip.write_inst(instruction);
            }
            match &assign.0 {
                SimpleAssignee::Identifier(name) => match wip.search_stack(name) {
                    DeclLocation::Frame(_, Declaration::Const(_))
                    | DeclLocation::Static(_, Declaration::Const(_))
                    | DeclLocation::Function(_) => {
                        todo!("erro: não é possível modificar esse valor")
                    }
                    DeclLocation::Global => {
                        wip.write_inst(instruction::ASSIGN_GLOBAL);
                        wip.write_str(name);
                    }
                    DeclLocation::Frame(index, _) => {
                        wip.write_inst(instruction::ASSIGN);
                        wip.write_i32(index);
                    }
                    DeclLocation::Static(index, _) => {
                        wip.write_inst(instruction::ASSIGN_STATIC);
                        wip.write_u32(index);
                    }
                    DeclLocation::Capture => todo!("capture"),
                },
                SimpleAssignee::Member(_, _, member) => {
                    wip.write_inst(instruction::ASSIGN_MEMBER);
                    wip.write_str(*member);
                }
                SimpleAssignee::Index(_, _, _) => {
                    wip.write_inst(instruction::ASSIGN_INDEX);
                }
            }
            if let Some(mut symbol) = symbol {
                wip.define_symbol(&mut symbol);
            }
        }
        Expr::Spread(_, expr) => {
            compile_expression(wip, expr);
            wip.write_inst(instruction::SPREAD);
        }
    }
}

fn compile_function<'a>(
    wip: &mut Wip<'a>,
    src: &'a str,
    is_async: Option<&'a str>,
    name: Option<&'a str>,
    args: &Patterns<'a>,
    body: FunctionBody<'a, '_>,
) -> FuncId {
    if is_async.is_some() {
        todo!("compilação de funções asíncronas ainda não foi implementado");
    }
    let funcid = wip.get_func_id(src);
    if wip.compiled_functions_map.contains_key(&funcid) {
        panic!("compile_function chamado com uum mesmo funcid duas vezes");
    }
    let prev_code = core::mem::take(&mut wip.code);
    let prev_function_uses_len = wip.function_uses.len();
    wip.push_frame();
    let old_save_len = wip.stack_len_save();
    wip.argc = args.len();
    if let Some(Pattern::Rest(_, _)) = args.last() {
        // o argumento ...rest não conta como argumento, pois é implementado como uma variável normal
        wip.argc -= 1;
    }
    for arg in args.iter() {
        match arg {
            Pattern::Identifier(name, _) => {
                wip.stack.push(Declaration::Let(name));
            }
            Pattern::Array(_, _, _) => {}
            Pattern::Object(_, _, _) => {}
            Pattern::Rest(_, _) => {}
        }
    }
    for (index, arg) in args.iter().enumerate() {
        match arg {
            Pattern::Identifier(_, expr)
            | Pattern::Array(_, _, expr)
            | Pattern::Object(_, _, expr) => {
                if let Some(expr) = expr {
                    let mut symbol = Symbol::new();
                    wip.write_inst(instruction::PUSH);
                    wip.write_i32(!(index as i32));
                    wip.write_inst(instruction::JPNU);
                    wip.write_inst(instruction::POP);
                    wip.write_symbol(&mut symbol);
                    compile_expression(wip, expr);
                    wip.write_inst(instruction::ASSIGN);
                    wip.write_i32(!(index as i32));
                    wip.define_symbol(&mut symbol);
                    wip.write_inst(instruction::POP);
                }
            }
            Pattern::Rest(_, _) => {}
        }
    }
    for (_index, arg) in args.iter().enumerate() {
        match arg {
            Pattern::Identifier(_, _) => {}
            Pattern::Array(_, _, _) => todo!("argument pattern"),
            Pattern::Object(_, _, _) => todo!("argument pattern"),
            Pattern::Rest(_, _) => {}
        }
    }
    if let Some(Pattern::Rest(_, _)) = args.last() {
        todo!("argument rest");
    }
    if let Some(name) = name {
        wip.stack.push(Declaration::Function(name, funcid));
    }
    match body {
        FunctionBody::Statements(body) => {
            compile_statements(wip, body);
            if !matches!(
                body.last(),
                Some(Statement::Return(_) | Statement::Throw(_, _))
            ) {
                wip.write_inst(instruction::UNDEFINED);
                wip.write_inst(instruction::RETURN);
            }
        }
        FunctionBody::Expr(expr) => {
            compile_expression(wip, expr);
            wip.write_inst(instruction::RETURN);
        }
    }
    wip.stack_len_restore(old_save_len);
    wip.pop_frame();
    let mut my_code = core::mem::replace(&mut wip.code, prev_code);
    // o offset do código compilado na compiled_functions
    let offset = wip.compiled_functions.len() as u32;
    wip.compiled_functions_map.insert(funcid, offset);
    wip.compiled_functions.append(&mut my_code);
    // o código da função adiciona indexes a function uses,
    // no entanto esses itens são relativos ao começo de wip.code, que agora foi movido para wip.compiled_functions
    // portanto é preciso atualizar os índices com o offset adequado
    for function_use in &mut wip.function_uses[prev_function_uses_len..] {
        if *function_use & MSB32 == 0 {
            unreachable!();
        }
        *function_use -= offset;
    }
    funcid
}

fn compile_number<'a>(wip: &mut Wip<'a>, number: &'a str) {
    match parse_number(number) {
        ParsedNumber::Integer(value) => match value {
            -0x80..=0x7F => {
                wip.write_inst(instruction::INTEGER_8);
                wip.write_i8(value as i8);
            }
            -0x8000..=0x7FFF => {
                wip.write_inst(instruction::INTEGER_16);
                wip.write_i16(value as i16);
            }
            -0x80000000..=0x7FFFFFFF => {
                wip.write_inst(instruction::INTEGER_32);
                wip.write_i32(value as i32);
            }
            _ => {
                wip.write_inst(instruction::INTEGER_64);
                wip.write_i64(value as i64);
            }
        },
        ParsedNumber::Float(value) => {
            wip.write_inst(instruction::FLOAT);
            wip.write_f64(value);
        }
    }
}

fn compile_string<'a>(wip: &mut Wip<'a>, string: &'a str) {
    wip.write_inst(instruction::STRING);
    wip.write_utf16(parse_string(string));
}

impl<'a> Wip<'a> {
    /// essa função termina o wip, e faz um programa, a principal coisa que ela faz é escrever os endereços das funções em seus devidos lugares
    ///
    /// no momento em que ela é chamada, todas as variáveis na pilha são interpretadas como static (globais declaradas explicitamente)
    ///
    /// o único efeito disso é que não podem haver Declaration::Frame na pilha, (uma função não pode estar sendo compilada), e que a quantidade
    /// de variáveis na pilha será usada para definir qual o "tamanho inicial da pilha", ou seja, a quantidade de variáveis estáticas
    fn to_byte_code(mut self) -> Program {
        let mut static_count: u32 = 0;
        for decl in &self.stack {
            match decl {
                Declaration::Var(_) | Declaration::Let(_) | Declaration::Const(_) => {
                    static_count += 1
                }
                Declaration::Function(_, _) => {}
                Declaration::Frame { .. } => unreachable!(),
            }
        }
        let mut code = Vec::new();
        code.reserve_exact(
            static_count.min(5) as usize + self.code.len() + self.compiled_functions.len(),
        );
        if static_count <= 4 {
            for _ in 0..static_count {
                code.push(instruction::UNITIALIZED);
            }
        } else {
            code.push(instruction::UNITIALIZED_MANY);
            code.extend_from_slice(&static_count.to_le_bytes());
        }
        let start_code_index = code.len() as u32;
        code.append(&mut self.code);
        let start_compiled_functions_index = code.len() as u32;
        code.append(&mut self.compiled_functions);
        for index in self.function_uses {
            let correct_index = if index & MSB32 == 0 {
                // index é "positivo", quer dizer que é um offset em code
                (index + start_code_index) as usize
            } else {
                // index é "negativo", quer dizer que é a negação do offset em compiled_functions, e não em code
                (!index + start_compiled_functions_index) as usize
            };
            let funcid = u32::from_le_bytes([
                code[correct_index + 0],
                code[correct_index + 1],
                code[correct_index + 2],
                code[correct_index + 3],
            ]);
            let absolute_address = self
                .compiled_functions_map
                .get(&funcid)
                .expect("função não foi compilada")
                + start_compiled_functions_index;
            let relative_address = absolute_address as i32 - (correct_index + 4) as i32;
            let bytes = relative_address.to_le_bytes();
            code[correct_index + 0] = bytes[0];
            code[correct_index + 1] = bytes[1];
            code[correct_index + 2] = bytes[2];
            code[correct_index + 3] = bytes[3];
        }
        Program {
            code,
            strings: self.strings,
        }
    }
    fn write_inst(&mut self, instruction: u8) {
        self.code.push(instruction);
    }
    /// escreve uma instrução e adiciona 1 temp a pilha simultaneamente
    ///
    /// prático para escrever uma instrução que adiciona um valor a pilha
    fn write_u32(&mut self, value: u32) {
        self.code.extend_from_slice(&value.to_le_bytes());
    }
    fn write_funcid(&mut self, value: FuncId) {
        if self.frame != 0 {
            self.function_uses.push(!(self.code.len() as u32));
        } else {
            self.function_uses.push(self.code.len() as u32);
        }
        self.code.extend_from_slice(&value.to_le_bytes());
    }
    fn write_i64(&mut self, value: i64) {
        self.code.extend_from_slice(&value.to_le_bytes());
    }
    fn write_i32(&mut self, value: i32) {
        self.code.extend_from_slice(&value.to_le_bytes());
    }
    fn write_i16(&mut self, value: i16) {
        self.code.extend_from_slice(&value.to_le_bytes());
    }
    fn write_i8(&mut self, value: i8) {
        self.code.extend_from_slice(&value.to_le_bytes());
    }
    fn write_f64(&mut self, value: f64) {
        self.code.extend_from_slice(&value.to_le_bytes());
    }
    fn write_str(&mut self, value: &str) {
        self.write_utf16(value.encode_utf16());
    }
    /// procura characters em strings, se encontrar, escreve um argumento (%s) com o texto encontrado
    ///
    /// se não encontrar adiciona o texto a strings, e escreve um argumento (%s) com a posição e tamanho do texto adicionado
    fn write_utf16(&mut self, characters: impl Iterator<Item = u16> + Clone) {
        let mut iter = characters.clone();
        let mut start = 0;
        for index in 0..self.strings.len() as u32 {
            if let Some(current) = iter.next() {
                if self.strings[index as usize] != current {
                    start = index + 1;
                    iter = characters.clone();
                }
            } else {
                // chegamos ao fim do iterador, todos os caracteres foram encontrados
                let length = index - start;
                self.write_u32(start);
                self.write_u32(length);
                return;
            }
        }
        self.strings.extend(iter);
        let length = self.strings.len() as u32 - start;
        self.write_u32(start);
        self.write_u32(length);
    }
    fn write_symbol(&mut self, symbol: &mut Symbol) {
        if let Some(resolved) = symbol.resolved() {
            // sabemos para onde o simbolo aponta
            // calcula e escreve agora
            let jump = resolved as i32 - self.code.len() as i32 - 4;
            self.code.extend_from_slice(&jump.to_le_bytes());
        } else {
            // não sabemos para onde o simbolo aponta
            // adiciona um valor temporario e adiciona esta posição a lista de peding em symbol
            symbol.push_pending(self.code.len() as u32);
            self.code.extend_from_slice(&i32::MIN.to_le_bytes());
        }
    }
    fn define_symbol(&mut self, symbol: &mut Symbol) {
        if symbol.resolved().is_some() {
            panic!("Wip::define_symbol chamado com um simbolo já definido");
        }
        let current = self.code.len() as i32;
        for pending in symbol.pending() {
            let value = current - 4 - *pending as i32;
            let bytes = value.to_le_bytes();
            self.code[*pending as usize + 0] = bytes[0];
            self.code[*pending as usize + 1] = bytes[1];
            self.code[*pending as usize + 2] = bytes[2];
            self.code[*pending as usize + 3] = bytes[3];
        }
        *symbol = Symbol::Resolved(self.code.len() as u32);
    }
    fn push_frame(&mut self) {
        self.stack.push(Declaration::Frame {
            frame: self.frame,
            argc: self.argc,
        });
        self.frame = self.stack.len();
        self.argc = 0;
    }
    fn pop_frame(&mut self) {
        match self.stack.pop() {
            Some(Declaration::Frame { frame, argc }) => {
                self.frame = frame;
                self.argc = argc;
            }
            decl => panic!(
                "Wip::pop_frame foi chamado sem haver um frame na pilha, {:?}",
                decl
            ),
        }
    }
    /// serve para podermos apagar as variáveis temporarias depois de serem usadas
    fn stack_len_save(&self) -> u32 {
        self.stack.len() as u32
    }
    /// serve para apagar as variáveis temporarias depois de serem usadas
    fn stack_len_restore(&mut self, len: u32) -> u32 {
        if len > self.stack.len() as u32 {
            panic!("Wip::stack_len_restore chamado com um valor maior que o tamanho atual");
        }
        for item in &self.stack[len as usize..] {
            if let Declaration::Frame { .. } = item {
                panic!("Wip::stack_len_restore tentou apagar um frame")
            }
        }
        let diff = self.stack.len() as u32 - len;
        self.stack.truncate(len as usize);
        diff
    }
    /// o mesmo que stack_len_restore, mas as instruções POP são escritas
    fn write_stack_len_restore(&mut self, len: u32) {
        let popped = self.stack_len_restore(len);
        if popped > 4 {
            self.write_inst(instruction::POP_MANY);
            self.write_u32(popped);
        } else if popped > 0 {
            for _ in 0..popped {
                self.write_inst(instruction::POP);
            }
        }
    }
    fn search_stack(&self, name: &str) -> DeclLocation<'a> {
        let mut frame_index;
        if self.frame == 0 {
            // não há frames, começamos no static
            frame_index = u32::MAX;
        } else {
            // há frames, começamos no frame atual
            frame_index = 0;
        }
        for (index, item) in self.stack.iter().enumerate().rev() {
            match item {
                Declaration::Var(declname)
                | Declaration::Let(declname)
                | Declaration::Const(declname) => {
                    if *declname == name {
                        return if frame_index == 0 {
                            // variável foi encontrada no frame atual
                            let mut frame_location = 0;
                            for item in self.stack[..index].iter().rev() {
                                match item {
                                    Declaration::Var(_)
                                    | Declaration::Let(_)
                                    | Declaration::Const(_) => frame_location += 1,
                                    Declaration::Function(_, _) => {}
                                    Declaration::Frame { .. } => break,
                                }
                            }
                            if frame_location >= self.argc {
                                // é uma variável temporária normal
                                DeclLocation::Frame(
                                    (frame_location - self.argc) as i32,
                                    item.clone(),
                                )
                            } else {
                                // é um argumento
                                DeclLocation::Frame(!(frame_location as i32), item.clone())
                            }
                        } else if frame_index == u32::MAX {
                            // variável foi encontrada nas globais declaradas (static)
                            DeclLocation::Static(index as u32, item.clone())
                        } else {
                            // variável foi encontrada no frame de outra função
                            DeclLocation::Capture
                        };
                    }
                }
                Declaration::Function(declname, funcid) => {
                    if *declname == name {
                        return DeclLocation::Function(*funcid);
                    }
                }
                Declaration::Frame { frame, argc } => {
                    if *frame != 0 {
                        frame_index += 1;
                    } else {
                        frame_index = u32::MAX;
                    }
                }
            }
        }
        DeclLocation::Global
    }
    fn frame_size(&self) -> u32 {
        let mut size = 0;
        if self.frame != 0 {
            for decl in &self.stack[self.frame + self.argc..] {
                match decl {
                    Declaration::Var(_) | Declaration::Let(_) | Declaration::Const(_) => size += 1,
                    Declaration::Function(_, _) => {}
                    Declaration::Frame { .. } => unreachable!(),
                }
            }
        }
        size
    }
    fn get_func_id(&self, src: &'a str) -> u32 {
        utils::offset_str(self.source, src).expect("Function src não é um slice do código fonte")
            as u32
    }
}

impl Symbol {
    fn new() -> Self {
        Self::PendingSome(0, [0; 7])
    }
    /// retorna a position em code do lugar para onde o symbolo fica (se já tiver sido resolvido)
    fn resolved(&self) -> Option<u32> {
        match self {
            Symbol::Resolved(position) => Some(*position),
            _ => None,
        }
    }
    /// retorna um slice com todas as posições onde devem ser escritas o offset
    fn pending(&self) -> &[u32] {
        match self {
            Symbol::Resolved(_) => &[],
            Symbol::PendingSome(len, array) => &array[0..*len as usize],
            Symbol::PendingMany(vector) => vector.as_slice(),
        }
    }
    fn push_pending(&mut self, position: u32) {
        if let Self::PendingSome(7, array) = self {
            let mut vector = Vec::with_capacity(16);
            vector.extend_from_slice(array);
            vector.push(position);
            *self = Self::PendingMany(vector);
        } else {
            match self {
                Self::Resolved(_) => {
                    panic!("Symbol::push_pending chamado sendo que o simbolo já está resolvido")
                }
                /*Self::PendingSome(7, array) => {
                    let mut vector = Vec::with_capacity(16);
                    vector.extend_from_slice(array);
                    vector.push(position);
                    *self = Self::PendingMany(vector);
                }*/
                Self::PendingSome(len, array) => {
                    array[*len as usize] = position;
                    *len += 1;
                }
                Self::PendingMany(vector) => {
                    vector.push(position);
                }
            }
        }
    }
}
