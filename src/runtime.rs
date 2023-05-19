use std::{cell::RefCell, collections::HashMap, fmt::Debug, hash::Hash, rc::Rc};

use crate::utils::{self, MAX_SAFE_INTEGER, MIN_SAFE_INTEGER};

const PROTOTYPE_KEY: String = cstring16!('p' 'r' 'o' 't' 'o' 't' 'y' 'p' 'e');

/// representa um programa compilado
#[derive(Clone)]
pub struct Program {
    pub code: Vec<u8>,
    pub strings: Vec<u16>,
}

macro_rules! instruction {
    ($($(#[doc = $doc:literal])* $ident:ident $(%$arg:ident)?;)*) => {
        /// um enum que pode ser convertido de e para um byte
        ///
        /// legenda dos parametros:
        /// %s -> (u32, u32), o primeiro indica um index na string table, o segundo o tamanho da string
        /// %j -> i32, aponta para uma localização neste programa sempre, relativo ao endreço após a instrução atual
        /// %m -> i32, um endereço no frame atual >0 = variável; <0 = parâmetro
        /// %a -> u32, um endereço do começo da pilha (variáveis globais declaradas, são chamadas de estaticas)
        /// %f -> f64, um float
        #[allow(unused)]
        pub mod instruction {
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub enum Parameter {
                None,
                String,
                Jump,
                Frame,
                ArgCount,
                Absolute,
                Integer64,
                Integer32,
                Integer16,
                Integer8,
                Float,
            }
            pub const TEXT: [&str; COUNT as usize] = [$(stringify!($ident),)*];
            pub const PARAMETERS: [Parameter; COUNT as usize] = [$( instructions_parameter!($(%$arg)?),)*];
            instructions_consts!{
                0;
                $($(#[doc = $doc])* $ident $(%$arg)?;)*
                /// não é uma instrução, é a quantidade de instruções
                COUNT;
            }
        }
    }
}
macro_rules! instructions_parameter {
    () => {
        Parameter::None
    };
    (%s) => {
        Parameter::String
    };
    (%j) => {
        Parameter::Jump
    };
    (%m) => {
        Parameter::Frame
    };
    (%n) => {
        Parameter::ArgCount
    };
    (%a) => {
        Parameter::Absolute
    };
    (%i64) => {
        Parameter::Integer64
    };
    (%i32) => {
        Parameter::Integer32
    };
    (%i16) => {
        Parameter::Integer16
    };
    (%i8) => {
        Parameter::Integer8
    };
    (%f) => {
        Parameter::Float
    };
}
macro_rules! instructions_consts {
    ($value:expr; $(#[doc = $doc:literal])* $ident:ident $(%$arg:ident)?; $($rest:tt)*) => {
        $(#[doc = $doc])*
        pub const $ident: u8 = $value;
        instructions_consts!{($value + 1); $($rest)*}
    };
    ($value:expr;) => {};
}

instruction! {
    /// () stop the program for debugging
    DEBUG;
    /// () push undefined
    UNDEFINED;
    /// () push null
    NULL;
    /// () push true
    TRUE;
    /// () push false
    FALSE;
    /// (%i) push integer
    INTEGER_64 %i64;
    /// (%i) push integer
    INTEGER_32 %i32;
    /// (%i) push integer
    INTEGER_16 %i16;
    /// (%i) push integer
    INTEGER_8 %i8;
    /// (%f) push float
    FLOAT %f;
    /// (%s) push string
    STRING %s;
    /// (%s) push big int
    BIGINT %s;
    /// () push this
    THIS;
    /// () push new_target
    NEW_TARGET;
    /// (%j) push user defined function, that starts at %j, in the program that is currently being executed
    FUNCTION %j;
    // (%n) pop (%n) values, make popped values into an array, push array (with Array prototype)
    ARRAY %n;
    /// () push empty object (with Object prototype)
    OBJECT;
    /// (%s) pop value, pop object, assign member %s to object, push object
    OBJECT_MEMBER %s;
    /// () pop value, pop object, spread value's properties in object, push object (has nothing to do with the spread register)
    OBJECT_SPREAD;
    /// (%s) pop value, push value.%s
    MEMBER %s;
    /// () pop index, pop object, push object\[index]
    INDEX;
    /// () pop value, push value, push value
    DUPE;
    /// () pop A, pop B, push B, push A, push B, push A
    DUPE_PAIR;
    /// (%m) push value of frame
    PUSH %m;
    /// () pop value
    POP;
    /// (%n) pop %m values from stack
    POP_MANY %n;
    /// (%n) pop value, pop %n values from stack, push value
    POP_RETAIN %n;
    /// (%s) push a value in global (if it is not found, throws a reference error)
    GLOBAL_MEMBER %s;
    /// () push the global object itself
    GLOBAL_OBJ;
    /// (%a) push value from the beginning of stack
    STATIC %a;

    /// () push unitialized value to stack
    UNITIALIZED;

    /// (%a) push multiple unitialized values to stack
    UNITIALIZED_MANY %a;

    /// (%n) -> pop function, pop this, call function with this and (%n) arguments, and reset spread register
    CALL %n;
    /// (%n) -> pop function, call function (as constructor) with this and (%n) arguments, and reset spread register
    CALL_NEW %n;

    /// () -> return
    RETURN;
    /// (%n) -> pop value, pop (%n), push value, and return
    RETURN_POP %n;
    /// () -> pop value, stop the program
    QUIT;

    /// () pop value, turn into an iterator, push all iterated values on the stack, add to the spread register the amount of iterated values
    SPREAD;

    /// (%m) stack(%m) = stack(0)
    ASSIGN %m;
    /// (%s) global(%s) = stack(0)
    ASSIGN_GLOBAL %s;
    /// (%s) object.%s = stack(0)
    ASSIGN_MEMBER %s;
    /// () pop value, pop index, pop object, object\[popped index] = popped value, push value
    ASSIGN_INDEX;
    /// (%a) stack(%a) = stack(0)
    ASSIGN_STATIC %a;

    // (%a) stack(%a) = Undefined
    ASSIGN_STATIC_UNDEFINED %a;

    /// (%s) -> attempt to delete global(%s), push either true or false
    DELETE_GLOBAL %s;
    /// (%s) -> pop object, attempt to delete object.%s, push either true or false
    DELETE_MEMBER %s;
    /// () -> pop index, pop object, attempt to delete object \[popped index], push either true or false
    DELETE_INDEX;

    /// (%j) jump
    JMP %j;
    /// (%j) pop, if truthy, jump
    JPT %j;
    /// (%j) pop, if falsy, jump
    JPF %j;
    /// (%j) if top of stack is truthy, jump
    JKT %j;
    /// (%j) if top of stack is falsy, jump
    JKF %j;
    /// (%j) pop, if null or undefined, jump
    JPN %j;
    /// (%j) pop, if not null and not undefined, jump
    JPD %j;
    /// (%j) if top of stack is null or undefined, jump
    JKN %j;
    /// (%j) if top of stack is not null and not undefined, jump
    JKD %j;
    /// (%j) if top of stack is not undefined, jump
    JPNU %j;

    /// () pop value, increment it, push value
    INCREMENT;
    /// () pop value, increment it, push value
    DECREMENT;
    /// (%m) increment value, then push copy onto stack
    PRE_INCREMENT %m;
    /// (%m) decrement value, then push copy onto stack
    PRE_DECREMENT %m;
    /// (%m) push copy onto stack, then increment value
    POST_INCREMENT %m;
    /// (%m) push copy onto stack, then decrement value
    POST_DECREMENT %m;
    /// (%a) increment value, then push copy onto stack
    PRE_INCREMENT_STATIC %a;
    /// (%a) decrement value, then push copy onto stack
    PRE_DECREMENT_STATIC %a;
    /// (%a) push copy onto stack, then increment value
    POST_INCREMENT_STATIC %a;
    /// (%a) push copy onto stack, then decrement value
    POST_DECREMENT_STATIC %a;

    // () infix operators (pops 2 values, pushes 1)
    POWER;
    MULTIPLY;
    DIVIDE;
    REMAINDER;
    ADD;
    SUBTRACT;
    BITWISE_LEFT;
    BITWISE_RIGHT;
    BITWISE_RIGHT_UNSIGNED;
    LESS;
    LESS_EQUAL;
    GREATER;
    GREATER_EQUAL;
    IN;
    INSTANCEOF;
    EQUAL;
    NOT_EQUAL;
    IDENTICAL;
    NOT_IDENTICAL;
    BITWISE_AND;
    BITWISE_XOR;
    BITWISE_OR;
    COMMA;

    // () prefix operators (pops 1 value, pushes 1 value) (unlike prefix, there are no simple postfix operators, all are handled in special ways)
    TYPEOF;
    VOID;
    BITWISE_NOT;
    NOT;
    UNARY_PLUS;
    UNARY_MINUS;

    AWAIT;

    /// (%j) push current catch to stack, set new catch at %j, with the current stack size as well (after to pushing old catch)
    TRY %j;
    /// () pop catch from stack, set it as the current catch (used after try block runs successfully)
    UNTRY;
    /// () pop value from stack, jump to current catch location, truncate stack to catch stack size, pop catch from stack, set it as the current catch
    THROW;

    /// (%j) push finally jmp object, with the location specified, it is expected that you JMP to a finally block after this instruction
    FINALLY_JMP %j;
    /// () pop value, push finally return object, with the value popped, it is expected that you JMP to a finally block after this instruction
    FINALLY_RETURN;
    /// () pop value, push finally throw object, with the value popped, it is expected that you JMP to a finally block after this instruction
    FINALLY_THROW;
    /// (%n) pop finally object/undefined from stack, if not undefined then (if finally object says so, push value) and jump to location in finally object
    ///
    /// if undefined, do nothing
    ///
    /// if finally jmp object, jump to instruction specified
    ///
    /// if finally return object, push value, then execute "RETURN_POP %n"
    ///
    /// if finally throw object, jump to current catch location, truncate stack to catch stack size, pop catch from stack, set it as the current catch
    FINALLY %n;
}

/// representa todo o estado da maquina virtual, quando executando algum programa
///
/// todos os campos desta struct só estão presentes na máquina virtual em execução, exceto global
#[derive(Debug, Clone)]
pub struct State {
    /// o programa que está sendo executado no momento
    prog: ProgReader,
    /// a pilha, ela é compartilhada entre as várias chamadas de função
    stack: Stack,
    /// a memoria do programa, criada externamente, e movida para cá, e quando o programa termina, ela é retornada
    memory: Memory,
}

/// representa a memoria da máquina virtual, isso é todo o estado que existe depois que ela termina de executar
#[derive(Debug, Clone)]
pub struct Memory {
    /// onde as variávies globais são guardadas
    pub global: Object,
    pub boolean_prototype: Object,
    pub string_prototype: Object,
    pub number_prototype: Object,
    pub object_prototype: Object,
    pub function_prototype: Object,
    pub array_prototype: Object,
}

#[derive(Debug, Clone)]
struct ProgReader {
    /// o programa que está sendo executado no momento
    program: Rc<Program>,
    /// o index da próxima instrução a ser executada
    pc: usize,
}

#[derive(Debug, Clone)]
struct Stack {
    stack: Vec<Slot>,
    frame: Frame,
}

/// representa um item na pilha, pode não ser um valor
#[derive(Debug, Clone)]
enum Slot {
    Value(Value),
    Unitialized,
    Frame {
        return_location: ProgReader,
        frame: Frame,
    },
}

/// todas as informações de uma chamada, são valores sempre acessíveis de dentro de um frame
#[derive(Debug, Clone)]
struct Frame {
    frame: usize,
    argc: u32,
    spread: u32,
    this: Object,
    new_target: Option<Object>,
}

/// representa um valor do javascript
#[derive(Clone)]
pub enum Value {
    Undefined,
    Null,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Object(Object),
}

/// mesma coisa que um value, mas simplificado para os dois tipos numericos
#[derive(Clone, Copy)]
pub enum Number {
    Integer(i64),
    Float(f64),
}

/// todo: no futuro otimizar isso, para que compartilhe buffers
#[derive(Clone)]
pub enum String {
    Owned(Vec<u16>),
    Static(&'static [u16]),
}

macro_rules! cstring16 {
    ($($char:literal)*) => {
        String::Static(&[$($char as u16,)*])
    };
}
pub(crate) use cstring16;

/// uma referência compartilhada a um Dict
#[derive(Clone)]
pub struct Object(Rc<RefCell<Dict>>);

/// o struct que representa um objeto/função de fato
pub struct Dict {
    /// a alma desse objeto, o que ele reamente é
    ///
    /// permite que objetos especiais guardem valores especiais aqui
    ///
    /// se for Soul::Function, o typeof disso é function
    soul: Soul,
    dict: HashMap<String, Value>,
    /// esse vec é uma otimização
    ///
    /// quando for obter um membro de dict, e esse membro for um numero sem sinal inteiro
    /// e o valor desse numero for menor que o len() deste vec,
    ///
    /// então procure esse valor no vec, e não no dict
    ///
    /// se no vec estiver None, é porque existe um buraco no array (equivalente a não encontrar no dict)
    ///
    /// nesse caso, faça o que você faria se não tivesse encontrado o membro em dict (procure no prototype)
    vec: Vec<Option<Value>>,
    prototype: Option<Object>,
}

/// o que o objecto realmente é, guarda informações adicionais sobre o objeto
#[derive(Clone)]
enum Soul {
    Object,
    Function(Function),
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone)]
enum Function {
    /// Chamada de um programa, o ProgReader fica no começo da função
    FunctionCall(ProgReader),
}

pub fn execute(program: Rc<Program>) -> Value {
    let mut state = State::new(program, Memory::new());
    state.run()
}

impl Program {
    fn read_instr(&self, pc: &mut usize) -> u8 {
        let instruction = self.code[*pc];
        *pc += 1;
        if instruction >= instruction::COUNT {
            panic!("Instrução inválida");
        }
        instruction
    }
    fn read_u32(&self, pc: &mut usize) -> u32 {
        let value = u32::from_le_bytes([
            self.code[*pc + 0],
            self.code[*pc + 1],
            self.code[*pc + 2],
            self.code[*pc + 3],
        ]);
        *pc += 4;
        value
    }
    fn read_i64(&self, pc: &mut usize) -> i64 {
        let value = i64::from_le_bytes([
            self.code[*pc + 0],
            self.code[*pc + 1],
            self.code[*pc + 2],
            self.code[*pc + 3],
            self.code[*pc + 4],
            self.code[*pc + 5],
            self.code[*pc + 6],
            self.code[*pc + 7],
        ]);
        *pc += 8;
        value
    }
    fn read_i32(&self, pc: &mut usize) -> i32 {
        let value = i32::from_le_bytes([
            self.code[*pc + 0],
            self.code[*pc + 1],
            self.code[*pc + 2],
            self.code[*pc + 3],
        ]);
        *pc += 4;
        value
    }
    fn read_i16(&self, pc: &mut usize) -> i16 {
        let value = i16::from_le_bytes([self.code[*pc + 0], self.code[*pc + 1]]);
        *pc += 2;
        value
    }
    fn read_i8(&self, pc: &mut usize) -> i8 {
        let value = i8::from_le_bytes([self.code[*pc]]);
        *pc += 1;
        value
    }
    fn read_f64(&self, pc: &mut usize) -> f64 {
        let value = f64::from_le_bytes([
            self.code[*pc + 0],
            self.code[*pc + 1],
            self.code[*pc + 2],
            self.code[*pc + 3],
            self.code[*pc + 4],
            self.code[*pc + 5],
            self.code[*pc + 6],
            self.code[*pc + 7],
        ]);
        *pc += 8;
        value
    }
    fn read_utf16(&self, pc: &mut usize) -> &[u16] {
        let start = self.read_u32(pc) as usize;
        let length = self.read_u32(pc) as usize;
        &self.strings[start..start + length]
    }
    fn read_str<'a>(&'a self, pc: &mut usize) -> std::string::String {
        std::string::String::from_utf16_lossy(self.read_utf16(pc))
    }
}

impl Debug for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use instruction::Parameter;
        const SPACES: &[u8] = &[b' '; 12];
        let spaces = std::str::from_utf8(SPACES).unwrap();
        let mut pc = 0;
        while pc < self.code.len() {
            let instruction_index = pc.to_string();
            let instruction = self.read_instr(&mut pc);
            let text = instruction::TEXT[instruction as usize];
            let param = instruction::PARAMETERS[instruction as usize];
            f.write_str(&instruction_index)?;
            f.write_str(&spaces[instruction_index.len().min(5 - 1)..5])?;
            f.write_str(text)?;
            f.write_str(&spaces[text.len().min(SPACES.len() - 1)..])?;
            match param {
                Parameter::None => {}
                Parameter::String => {
                    Debug::fmt(&self.read_str(&mut pc), f)?;
                }
                Parameter::Jump => {
                    Debug::fmt(&self.read_i32(&mut pc), f)?;
                }
                Parameter::Frame => {
                    Debug::fmt(&self.read_i32(&mut pc), f)?;
                }
                Parameter::ArgCount => {
                    Debug::fmt(&self.read_u32(&mut pc), f)?;
                }
                Parameter::Absolute => {
                    Debug::fmt(&self.read_u32(&mut pc), f)?;
                }
                Parameter::Integer64 => {
                    Debug::fmt(&self.read_i64(&mut pc), f)?;
                }
                Parameter::Integer32 => {
                    Debug::fmt(&self.read_i32(&mut pc), f)?;
                }
                Parameter::Integer16 => {
                    Debug::fmt(&self.read_i16(&mut pc), f)?;
                }
                Parameter::Integer8 => {
                    Debug::fmt(&self.read_i8(&mut pc), f)?;
                }
                Parameter::Float => {
                    Debug::fmt(&self.read_f64(&mut pc), f)?;
                }
            }
            f.write_str("\n")?;
        }
        Ok(())
    }
}

impl State {
    /// cria um novo estado, quando for executado, começará a executar o program
    pub fn new(program: Rc<Program>, memory: Memory) -> Self {
        State {
            prog: ProgReader::new(program),
            stack: Stack::new(memory.global.clone()),
            memory,
        }
    }
    /// executa a máquina virtual até que ela termine, e retorna o valor que ela retornou
    pub fn run(&mut self) -> Value {
        let this = self.stack.frame.this.clone();
        let mut prog = self.prog.clone();
        let mut stack = std::mem::replace(&mut self.stack, Stack::new(this));
        let memory = self.memory.clone();
        let value = loop {
            if let Some(value) = execute_step(&mut prog, &mut stack, &memory) {
                break value;
            }
        };
        self.prog = prog;
        self.stack = stack;
        value
    }
    /// retorna a memoria do programa
    #[allow(dead_code)]
    pub fn to_memory(self) -> Memory {
        self.memory
    }
}

impl Memory {
    pub fn new() -> Self {
        let object_prototype = Object::without_prototype();
        Self {
            global: Object::new(object_prototype.clone()),
            boolean_prototype: Object::new(object_prototype.clone()),
            string_prototype: Object::new(object_prototype.clone()),
            number_prototype: Object::new(object_prototype.clone()),
            function_prototype: Object::new(object_prototype.clone()),
            array_prototype: Object::new(object_prototype.clone()),
            object_prototype,
        }
    }
}

impl ProgReader {
    fn new(program: Rc<Program>) -> Self {
        Self { program, pc: 0 }
    }
    fn read_instr(&mut self) -> u8 {
        self.program.read_instr(&mut self.pc)
    }
    fn read_u32(&mut self) -> u32 {
        self.program.read_u32(&mut self.pc)
    }
    fn read_i64(&mut self) -> i64 {
        self.program.read_i64(&mut self.pc)
    }
    fn read_i32(&mut self) -> i32 {
        self.program.read_i32(&mut self.pc)
    }
    fn read_i16(&mut self) -> i16 {
        self.program.read_i16(&mut self.pc)
    }
    fn read_i8(&mut self) -> i8 {
        self.program.read_i8(&mut self.pc)
    }
    fn read_f64(&mut self) -> f64 {
        self.program.read_f64(&mut self.pc)
    }
    fn read_utf16(&mut self) -> String {
        let start = self.program.read_u32(&mut self.pc) as usize;
        let length = self.program.read_u32(&mut self.pc) as usize;
        String::Owned(self.program.strings[start..length + start].to_owned())
    }
    fn jump(&mut self, jump: i32) {
        if jump >= 0 {
            self.pc += jump as usize;
            if self.pc >= self.program.code.len() {
                panic!("Um JMP pulou para depois do fim do programa");
            }
        } else {
            self.pc -= (-jump) as usize;
        }
    }
}

impl Stack {
    fn new(this: Object) -> Self {
        Self {
            stack: Vec::new(),
            frame: Frame::new(this),
        }
    }
    fn absolute(&self, index: u32) -> &Value {
        let index = index as usize;
        if index >= self.stack.len() {
            panic!("erro: Stack::absolute foi chamado com um index além do fim da pilha");
        }
        if let Slot::Value(value) = &self.stack[index] {
            value
        } else {
            panic!("erro: Stack::absolute foi chamado em um slot que não era um Value")
        }
    }
    fn absolute_mut(&mut self, index: u32) -> &mut Value {
        let index = index as usize;
        if index >= self.stack.len() {
            panic!("erro: Stack::absolute_mut foi chamado com um index além do fim da pilha");
        }
        if let Slot::Value(value) = &mut self.stack[index] {
            value
        } else {
            panic!("erro: Stack::absolute_mut foi chamado em um slot que não era um Value")
        }
    }
    fn absolute_assign(&mut self, index: u32, value: Value) {
        let index = index as usize;
        if index >= self.stack.len() {
            panic!("erro: Stack::absolute_assign foi chamado com um index além do fim da pilha");
        }
        if !matches!(self.stack[index], Slot::Value(_) | Slot::Unitialized) {
            panic!("erro: Stack::absolute_assign foi chamado em um slot que não era um Value")
        }
        self.stack[index] = Slot::Value(value);
    }
    fn frame(&self, index: i32) -> &Value {
        let index = if index >= 0 {
            self.frame.frame + index as usize
        } else {
            let index = (-index) as usize - 1;
            if index >= self.frame.argc as usize {
                return &Value::Undefined;
            }
            self.frame.frame - 1 + index - self.frame.argc as usize
        };
        if index >= self.stack.len() {
            panic!("erro: Stack::frame foi chamado com um index além do fim da pilha");
        }
        match &self.stack[index] {
            Slot::Value(value) => value,
            Slot::Unitialized => todo!("erro: uso de variável antes da inicialização"),
            _ => panic!("erro: Stack::frame foi chamado em um slot que não era um Value"),
        }
    }
    fn frame_mut(&mut self, index: i32) -> &mut Value {
        let index = if index >= 0 {
            self.frame.frame + index as usize
        } else {
            let index = (-index) as usize - 1;
            if index >= self.frame.argc as usize {
                panic!("erro: frame_mut foi chamado em um argumento que não existe");
            }
            self.frame.frame - 1 + index - self.frame.argc as usize
        };
        if index >= self.stack.len() {
            panic!("erro: Stack::frame_mut foi chamado com um index além do fim da pilha");
        }
        match &mut self.stack[index] {
            Slot::Value(value) => value,
            Slot::Unitialized => todo!("erro: uso de variável antes da inicialização"),
            _ => panic!("erro: Stack::frame_mut foi chamado em um slot que não era um Value"),
        }
    }
    fn frame_assign(&mut self, index: i32, value: Value) {
        let index = if index >= 0 {
            self.frame.frame + index as usize
        } else {
            let index = (-index) as usize - 1;
            if index >= self.frame.argc as usize {
                panic!("erro: frame_assign foi chamado em um argumento que não existe");
            }
            self.frame.frame - 1 + index - self.frame.argc as usize
        };
        if index >= self.stack.len() {
            panic!("erro: Stack::frame_assign foi chamado com um index além do fim da pilha");
        }
        if !matches!(self.stack[index], Slot::Value(_) | Slot::Unitialized) {
            panic!("erro: Stack::frame_assign foi chamado em um slot que não era um Value")
        }
        self.stack[index] = Slot::Value(value);
    }
    fn push(&mut self, value: Value) {
        self.stack.push(Slot::Value(value))
    }
    fn pop(&mut self) -> Value {
        match self.stack.pop() {
            Some(Slot::Value(value)) => value,
            Some(Slot::Unitialized) => todo!("erro: uso de variável antes da inicialização"),
            Some(_) => panic!("Stack::pop foi chamado em um slot que não era um Value"),
            None => panic!("Stack::pop foi chamado com a pilha vazia"),
        }
    }
    fn pop_many(&mut self, count: u32) {
        let new_len = self.stack.len() - count as usize;
        for slot in &self.stack[new_len..] {
            match slot {
                Slot::Value(_) | Slot::Unitialized => {}
                _ => panic!("Stack::pop_many foi chamado em um slot que não era um Value"),
            }
        }
        self.stack.truncate(new_len);
    }
    fn push_frame(
        &mut self,
        return_location: ProgReader,
        argc: u32,
        this: Object,
        new_target: Option<Object>,
    ) {
        let new_frame = Frame {
            frame: self.len() + 1,
            argc,
            spread: 0,
            this,
            new_target,
        };
        self.stack.push(Slot::Frame {
            return_location,
            frame: std::mem::replace(&mut self.frame, new_frame),
        });
    }
    fn pop_frame(&mut self, return_value: Value) -> ProgReader {
        match self.stack.pop() {
            Some(Slot::Frame {
                return_location,
                frame,
            }) => {
                let old_frame = std::mem::replace(&mut self.frame, frame);
                self.pop_many(old_frame.argc);
                if old_frame.new_target.is_some() {
                    self.push(Value::Object(old_frame.this));
                } else {
                    self.push(return_value);
                }
                return return_location;
            }
            _ => panic!("Stack::pop_frame foi chamado sem haver um Slot::Frame no topo da pilha"),
        }
    }
    fn top(&self) -> &Value {
        match self.stack.last() {
            Some(Slot::Value(value)) => value,
            Some(Slot::Unitialized) => todo!("erro: uso de variável antes da inicialização"),
            Some(_) => panic!("Stack::top foi chamado em um slot que não era um Value"),
            None => panic!("Stack::top foi chamado com a pilha vazia"),
        }
    }
    fn top_mut(&mut self) -> &mut Value {
        match self.stack.last_mut() {
            Some(Slot::Value(value)) => value,
            Some(Slot::Unitialized) => todo!("erro: uso de variável antes da inicialização"),
            Some(_) => panic!("Stack::top_mut foi chamado em um slot que não era um Value"),
            None => panic!("Stack::top_mut foi chamado com a pilha vazia"),
        }
    }
    fn len(&self) -> usize {
        self.stack.len()
    }
    fn execute_infix(&mut self, operator: fn(Value, Value) -> Value) {
        let right = self.pop();
        let left = self.pop();
        self.push(operator(left, right));
    }
    fn execute_prefix(&mut self, operator: fn(Value) -> Value) {
        let value = self.pop();
        self.push(operator(value));
    }
}

impl Frame {
    fn new(this: Object) -> Self {
        Self {
            frame: 0,
            argc: 0,
            spread: 0,
            this,
            new_target: None,
        }
    }
}

impl Value {
    /// é preciso memory aqui, pois pode acontecer de obtermos um property para um primitivo
    ///
    /// nesse caso precisaríamos obter o prototype dele
    pub fn property(&self, key: &String, memory: &Memory) -> Value {
        match self {
            Value::Undefined => Value::Undefined,
            Value::Null => Value::Undefined,
            Value::Boolean(_) => memory.boolean_prototype.0.borrow().property(key),
            Value::Integer(_) => memory.number_prototype.0.borrow().property(key),
            Value::Float(_) => memory.number_prototype.0.borrow().property(key),
            Value::String(_) => memory.string_prototype.0.borrow().property(key),
            Value::Object(object) => object.property(key),
        }
    }
    #[allow(dead_code)]
    pub fn own_property(&self, key: &String) -> Option<Value> {
        if let Value::Object(object) = self {
            object.own_property(key)
        } else {
            None
        }
    }
    pub fn assign_property(&self, key: String, value: Value) -> bool {
        if let Value::Object(object) = self {
            object.assign_property(key, value);
            true
        } else {
            false
        }
    }
    pub fn delete_property(&self, key: &String) -> bool {
        if let Value::Object(object) = self {
            object.delete_property(key);
            true
        } else {
            false
        }
    }
    pub fn to_object(self, memory: &Memory) -> Option<Object> {
        match self {
            Value::Undefined => None,
            Value::Null => None,
            Value::Boolean(boolean) => Some(Object::from_soul(Soul::Boolean(boolean), memory)),
            Value::Integer(integer) => Some(Object::from_soul(Soul::Integer(integer), memory)),
            Value::Float(float) => Some(Object::from_soul(Soul::Float(float), memory)),
            Value::String(string) => Some(Object::from_soul(Soul::String(string), memory)),
            Value::Object(object) => Some(object),
        }
    }
    fn to_function(&self) -> Option<Function> {
        if let Value::Object(object) = &self {
            if let Soul::Function(function) = &object.0.borrow().soul {
                return Some(function.clone());
            }
        }
        None
    }
    pub fn is_defined(&self) -> bool {
        match self {
            Value::Undefined => false,
            Value::Null => false,
            Value::Boolean(_) => true,
            Value::Integer(_) => true,
            Value::Float(_) => true,
            Value::String(_) => true,
            Value::Object(_) => true,
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Undefined
    }
}

impl Number {
    pub fn to_value(self) -> Value {
        match self {
            Number::Integer(i @ MIN_SAFE_INTEGER..=MAX_SAFE_INTEGER) => Value::Integer(i),
            Number::Integer(i) => Value::Float(i as f64),
            Number::Float(f) => Value::Float(f),
        }
    }
    pub fn to_int(self) -> i64 {
        match self {
            Number::Integer(i) => i,
            Number::Float(f) => f as i64,
        }
    }
    #[allow(dead_code)]
    pub fn to_float(self) -> f64 {
        match self {
            Number::Integer(i) => i as f64,
            Number::Float(f) => f,
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Undefined => write!(f, "undefined"),
            Self::Null => write!(f, "null"),
            Self::Boolean(true) => write!(f, "true"),
            Self::Boolean(false) => write!(f, "false"),
            Self::Integer(val) => write!(f, "{}", val),
            Self::Float(val) => write!(f, "{}", val),
            Self::String(val) => write!(f, "{:#?}", val),
            Self::Object(val) => write!(f, "{:#?}", val),
        }
    }
}

impl String {
    pub fn as_slice(&self) -> &[u16] {
        match self {
            String::Owned(vec) => vec.as_slice(),
            String::Static(slice) => *slice,
        }
    }
    pub fn to_vec(self) -> Vec<u16> {
        match self {
            String::Owned(vec) => vec,
            String::Static(slice) => slice.to_owned(),
        }
    }
    pub fn len(&self) -> usize {
        match self {
            String::Owned(vec) => vec.len(),
            String::Static(slice) => slice.len(),
        }
    }
    pub fn to_number(&self) -> Number {
        const INFINITY: [u16; 8] = [
            'I' as u16, 'n' as u16, 'f' as u16, 'i' as u16, 'n' as u16, 'i' as u16, 't' as u16,
            'y' as u16,
        ];
        let slice = self.as_slice();
        let front_index = slice
            .iter()
            .enumerate()
            .filter(|x| !utils::is_space(*x.1 as u32))
            .map(|x| x.0)
            .next()
            .unwrap_or(slice.len());
        let back_index = slice
            .iter()
            .enumerate()
            .rev()
            .filter(|x| !utils::is_space(*x.1 as u32))
            .map(|x| x.0 + 1)
            .next()
            .unwrap_or(0);
        if front_index >= back_index {
            return Number::Float(f64::NAN);
        }
        let (slice, is_negative) = if slice[front_index] == '-' as u16 {
            (&slice[front_index + 1..back_index], true)
        } else {
            (&slice[front_index..back_index], false)
        };
        if slice.is_empty() {
            return Number::Float(f64::NAN);
        } else if slice == INFINITY {
            if is_negative {
                return Number::Float(f64::NEG_INFINITY);
            } else {
                return Number::Float(f64::INFINITY);
            }
        }
        let mut contains_dot = false;
        for char in slice {
            match *char {
                // '0'..='9'
                48..=57 => {}
                // '.'
                46 if !contains_dot => {
                    contains_dot = true;
                }
                _ => return Number::Float(f64::NAN),
            }
        }
        if !contains_dot {
            let mut digits = slice.iter().map(|x| *x as i64 - 48);
            let mut integer_acc = 0;
            loop {
                if let Some(digit) = digits.next() {
                    integer_acc = integer_acc * 10 + digit;
                    if integer_acc > utils::MAX_SAFE_INTEGER {
                        // como integer, não deu certo, tenta de novo como um float
                        break;
                    }
                } else {
                    return Number::Integer(integer_acc);
                }
            }
        }
        let mut temp = std::string::String::with_capacity(slice.len());
        temp.extend(slice.iter().map(|x| *x as u8 as char));
        Number::Float(temp.parse().unwrap_or(f64::NAN))
    }

    /// tenta converter essa string em um index, menor que excluding_max
    ///
    /// ela tem de ser um número simples, sem zeros no começo (exceto o própio número zero), composto completamente por dígitos ascii
    fn try_into_index(&self, excluding_max: usize) -> Option<usize> {
        if excluding_max == 0 {
            return None;
        }
        let slice = self.as_slice();
        if slice.is_empty() {
            None
        } else if slice == &[b'0' as u16] {
            Some(0)
        } else if matches!(slice[0], 49..=57) {
            let mut acc: usize = 0;
            for digit in slice {
                if *digit >= b'0' as u16 && *digit <= b'9' as u16 {
                    acc = acc.checked_mul(10)?;
                    acc = acc.checked_add((*digit - b'0' as u16) as usize)?;
                    if acc >= excluding_max {
                        return None;
                    }
                } else {
                    return None;
                }
            }
            Some(acc)
        } else {
            None
        }
    }
}

impl Hash for String {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state);
    }
}

impl PartialEq for String {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}
impl Eq for String {}

impl std::ops::Add for String {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        if self.len() == 0 {
            rhs
        } else if rhs.len() == 0 {
            self
        } else {
            let mut lhs = self.to_vec();
            lhs.extend_from_slice(rhs.as_slice());
            Self::Owned(lhs)
        }
    }
}

impl Debug for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}",
            std::string::String::from_utf16_lossy(self.as_slice())
        )
    }
}

impl PartialOrd for String {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for String {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_slice().cmp(other.as_slice())
    }
}

impl Object {
    pub fn new(prototype: Object) -> Self {
        Self(Rc::new(RefCell::new(Dict::new(Some(prototype)))))
    }
    pub fn without_prototype() -> Self {
        Self(Rc::new(RefCell::new(Dict::new(None))))
    }
    pub fn prototype(&self) -> Option<Object> {
        self.0.borrow().prototype.clone()
    }
    pub fn property(&self, key: &String) -> Value {
        self.0.borrow().property(key)
    }
    pub fn own_property(&self, key: &String) -> Option<Value> {
        self.0.borrow().own_property(key)
    }
    pub fn assign_property(&self, key: String, value: Value) {
        self.0.borrow_mut().assign_property(key, value);
    }
    pub fn delete_property(&self, key: &String) {
        self.0.borrow_mut().delete_property(key);
    }
    pub fn is_function(&self) -> bool {
        matches!(self.0.borrow().soul, Soul::Function(_))
    }
    fn to_function(&self) -> Option<Function> {
        if let Soul::Function(function) = &self.0.borrow().soul {
            Some(function.clone())
        } else {
            None
        }
    }
    fn from_soul(soul: Soul, memory: &Memory) -> Self {
        Dict::from_soul(soul, memory).to_object()
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for Object {}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: write a good debug function
        (&*self.0.borrow()).fmt(f)
    }
}

impl Dict {
    fn new(prototype: Option<Object>) -> Self {
        Self {
            soul: Soul::Object,
            dict: HashMap::new(),
            vec: Vec::new(),
            prototype,
        }
    }
    fn from_soul(soul: Soul, memory: &Memory) -> Self {
        let prototype = match &soul {
            Soul::Object => memory.object_prototype.clone(),
            Soul::Function(_) => memory.function_prototype.clone(),
            Soul::Boolean(_) => memory.boolean_prototype.clone(),
            Soul::Integer(_) => memory.number_prototype.clone(),
            Soul::Float(_) => memory.number_prototype.clone(),
            Soul::String(_) => memory.string_prototype.clone(),
        };
        Self {
            soul,
            dict: HashMap::new(),
            vec: Vec::new(),
            prototype: Some(prototype),
        }
    }
    fn property(&self, key: &String) -> Value {
        if let Some(value) = self.own_property(key) {
            value
        } else if let Some(prototype) = &self.prototype {
            prototype.property(key)
        } else {
            Value::Undefined
        }
    }
    fn own_property(&self, key: &String) -> Option<Value> {
        if let Some(index) = key.try_into_index(self.vec.len()) {
            self.vec[index].clone()
        } else {
            self.dict.get(key).cloned()
        }
    }
    fn assign_property(&mut self, key: String, value: Value) {
        if let Some(index) = key.try_into_index(self.vec.len()) {
            self.vec[index] = Some(value);
        } else {
            self.dict.insert(key, value);
        }
    }
    fn delete_property(&mut self, key: &String) {
        if let Some(index) = key.try_into_index(self.vec.len()) {
            self.vec[index] = None;
        } else {
            self.dict.remove(key);
        }
    }
    fn to_object(self) -> Object {
        Object(Rc::new(RefCell::new(self)))
    }
}

impl Debug for Dict {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: write a good debug function
        self.dict.fmt(f)
    }
}

/// executa um passo da máquina virtual,
///
/// retorna Some(...) se for a instrução stop
#[inline]
fn execute_step(prog: &mut ProgReader, stack: &mut Stack, memory: &Memory) -> Option<Value> {
    use instruction::*;
    match prog.read_instr() {
        DEBUG => {
            println!("DEBUG: {:#?}", stack.top());
        }
        UNDEFINED => {
            stack.push(Value::Undefined);
        }
        NULL => {
            stack.push(Value::Null);
        }
        TRUE => {
            stack.push(Value::Boolean(true));
        }
        FALSE => {
            stack.push(Value::Boolean(false));
        }
        INTEGER_64 => {
            stack.push(Value::Integer(prog.read_i64()));
        }
        INTEGER_32 => {
            stack.push(Value::Integer(prog.read_i32() as i64));
        }
        INTEGER_16 => {
            stack.push(Value::Integer(prog.read_i16() as i64));
        }
        INTEGER_8 => {
            stack.push(Value::Integer(prog.read_i8() as i64));
        }
        FLOAT => {
            stack.push(Value::Float(prog.read_f64()));
        }
        STRING => {
            stack.push(Value::String(prog.read_utf16()));
        }
        BIGINT => todo!("BIGINT"),
        THIS => {
            stack.push(Value::Object(stack.frame.this.clone()));
        }
        NEW_TARGET => {
            if let Some(new_target) = &stack.frame.new_target {
                stack.push(Value::Object(new_target.clone()));
            } else {
                stack.push(Value::Undefined);
            }
        }
        FUNCTION => {
            let jump = prog.read_i32();
            let absolute = if jump >= 0 {
                prog.pc + jump as usize
            } else {
                prog.pc - (-jump) as usize
            };
            let function = Function::FunctionCall(ProgReader {
                program: prog.program.clone(),
                pc: absolute,
            });
            let mut dict = Dict::from_soul(Soul::Function(function), memory);
            let prototype = Value::Object(Object::new(memory.global.clone()));
            dict.assign_property(PROTOTYPE_KEY.clone(), prototype);
            stack.push(Value::Object(dict.to_object()))
        }
        ARRAY => todo!("ARRAY"),
        OBJECT => {
            stack.push(Value::Object(Object::new(memory.object_prototype.clone())));
        }
        OBJECT_MEMBER => {
            let member = prog.read_utf16();
            let value = stack.pop();
            if let Value::Object(object) = stack.top_mut() {
                object.assign_property(member, value);
            }
        }
        OBJECT_SPREAD => todo!("OBJECT_SPREAD"),
        MEMBER => {
            let member = prog.read_utf16();
            let object = stack.pop();
            let value = object.property(&member, memory);
            stack.push(value);
        }
        INDEX => {
            let index = stack.pop();
            let object = stack.pop();
            let value = object.property(&index.to_string(), memory);
            stack.push(value);
        }

        DUPE => {
            let value = stack.top().clone();
            stack.push(value);
        }
        DUPE_PAIR => {
            let second = stack.pop();
            let first = stack.pop();
            stack.push(first.clone());
            stack.push(second.clone());
            stack.push(first);
            stack.push(second);
        }
        PUSH => {
            let address = prog.read_i32();
            let value = stack.frame(address).clone();
            stack.push(value);
        }
        POP => {
            stack.pop();
        }
        POP_MANY => {
            stack.pop_many(prog.read_u32());
        }
        POP_RETAIN => {
            let value = stack.pop();
            stack.pop_many(prog.read_u32());
            stack.push(value);
        }
        GLOBAL_MEMBER => {
            let s = prog.read_utf16();
            let dict = memory.global.0.borrow();
            if let Some(value) = dict.dict.get(&s) {
                stack.push(value.clone());
            } else {
                todo!("error: Reference Error, global não encontrada")
            }
        }
        GLOBAL_OBJ => {
            stack.push(Value::Object(memory.global.clone()));
        }
        STATIC => {
            let address = prog.read_u32();
            let value = stack.absolute(address).clone();
            stack.push(value);
        }
        UNITIALIZED => {
            stack.stack.push(Slot::Unitialized);
        }
        UNITIALIZED_MANY => {
            let count = prog.read_u32() as usize;
            stack.stack.reserve(count);
            for _ in 0..count {
                stack.stack.push(Slot::Unitialized);
            }
        }
        CALL => {
            let argc = prog.read_u32() + stack.frame.spread;
            stack.frame.spread = 0;
            let func = stack.pop();
            let this = stack.pop();
            match func.to_function() {
                Some(dispatch) => match dispatch {
                    Function::FunctionCall(call_site) => {
                        let this = this
                            .to_object(memory)
                            .unwrap_or_else(|| memory.global.clone());
                        let new_target = None;
                        let return_location = std::mem::replace(prog, call_site);
                        stack.push_frame(return_location, argc, this, new_target);
                    }
                },
                None => todo!("erro de execução, objecto não pode ser chamado"),
            }
        }
        CALL_NEW => {
            let argc = prog.read_u32() + stack.frame.spread;
            stack.frame.spread = 0;
            let func = stack.pop();
            if let Value::Object(func_object) = func {
                match func_object.to_function() {
                    Some(dispatch) => match dispatch {
                        Function::FunctionCall(call_site) => {
                            let prototype = if let Value::Object(prototype) =
                                func_object.property(&PROTOTYPE_KEY)
                            {
                                Some(prototype)
                            } else {
                                None
                            };
                            let this = Dict::new(prototype).to_object();
                            let new_target = Some(func_object);
                            let return_location = std::mem::replace(prog, call_site);
                            stack.push_frame(return_location, argc, this, new_target);
                        }
                    },
                    None => todo!("erro de execução, objeto não pode ser chamado"),
                }
            } else {
                todo!("erro de execução, objeto não pode ser chamado");
            }
        }
        RETURN => {
            let value = stack.pop();
            *prog = stack.pop_frame(value);
        }
        RETURN_POP => {
            let count = prog.read_u32();
            let value = stack.pop();
            stack.pop_many(count);
            *prog = stack.pop_frame(value);
        }
        QUIT => {
            return Some(stack.pop());
        }

        SPREAD => todo!("SPREAD"),

        ASSIGN => {
            let address = prog.read_i32();
            let value = stack.top().clone();
            stack.frame_assign(address, value);
        }
        ASSIGN_GLOBAL => {
            let member = prog.read_utf16();
            let value = stack.top().clone();
            memory.global.assign_property(member, value);
        }
        ASSIGN_MEMBER => {
            let member = prog.read_utf16();
            let object = stack.pop();
            let value = stack.top().clone();
            object.assign_property(member, value);
        }
        ASSIGN_INDEX => {
            let index = stack.pop();
            let object = stack.pop();
            let value = stack.top().clone();
            object.assign_property(index.to_string(), value);
        }
        ASSIGN_STATIC => {
            let address = prog.read_u32();
            let value = stack.top().clone();
            stack.absolute_assign(address, value);
        }

        ASSIGN_STATIC_UNDEFINED => {
            let address = prog.read_u32();
            stack.absolute_assign(address, Value::Undefined);
        }

        DELETE_GLOBAL => {
            let member = prog.read_utf16();
            memory.global.delete_property(&member);
            stack.push(Value::Boolean(true));
        }
        DELETE_MEMBER => {
            let member = prog.read_utf16();
            stack.pop().delete_property(&member);
            stack.push(Value::Boolean(true));
        }
        DELETE_INDEX => {
            let index = stack.pop();
            stack.pop().delete_property(&index.to_string());
            stack.push(Value::Boolean(true));
        }

        JMP => {
            let jump = prog.read_i32();
            prog.jump(jump);
        }
        JPT => {
            let jump = prog.read_i32();
            if stack.pop().to_boolean() {
                prog.jump(jump);
            }
        }
        JPF => {
            let jump = prog.read_i32();
            if !stack.pop().to_boolean() {
                prog.jump(jump);
            }
        }
        JKT => {
            let jump = prog.read_i32();
            if stack.top().to_boolean() {
                prog.jump(jump);
            }
        }
        JKF => {
            let jump = prog.read_i32();
            if !stack.top().to_boolean() {
                prog.jump(jump);
            }
        }
        JPN => {
            let jump = prog.read_i32();
            if !stack.pop().is_defined() {
                prog.jump(jump);
            }
        }
        JPD => {
            let jump = prog.read_i32();
            if stack.pop().is_defined() {
                prog.jump(jump);
            }
        }
        JKN => {
            let jump = prog.read_i32();
            if !stack.top().is_defined() {
                prog.jump(jump);
            }
        }
        JKD => {
            let jump = prog.read_i32();
            if stack.top().is_defined() {
                prog.jump(jump);
            }
        }
        JPNU => {
            let jump = prog.read_i32();
            if !matches!(stack.top(), Value::Undefined) {
                prog.jump(jump);
            }
        }
        INCREMENT => {
            stack.execute_prefix(Value::increment);
        }
        DECREMENT => {
            stack.execute_prefix(Value::decrement);
        }
        PRE_INCREMENT => {
            let address = prog.read_i32();
            let value = std::mem::take(stack.frame_mut(address)).increment();
            stack.frame_assign(address, value.clone());
            stack.push(value);
        }
        PRE_DECREMENT => {
            let address = prog.read_i32();
            let value = std::mem::take(stack.frame_mut(address)).decrement();
            stack.frame_assign(address, value.clone());
            stack.push(value);
        }
        POST_INCREMENT => {
            let address = prog.read_i32();
            let value = std::mem::take(stack.frame_mut(address));
            stack.frame_assign(address, value.clone().increment());
            stack.push(value);
        }
        POST_DECREMENT => {
            let address = prog.read_i32();
            let value = std::mem::take(stack.frame_mut(address));
            stack.frame_assign(address, value.clone().decrement());
            stack.push(value);
        }
        PRE_INCREMENT_STATIC => {
            let address = prog.read_u32();
            let value = std::mem::take(stack.absolute_mut(address)).increment();
            stack.absolute_assign(address, value.clone());
        }
        PRE_DECREMENT_STATIC => {
            let address = prog.read_u32();
            let value = std::mem::take(stack.absolute_mut(address)).decrement();
            stack.absolute_assign(address, value.clone());
        }
        POST_INCREMENT_STATIC => {
            let address = prog.read_u32();
            let value = std::mem::take(stack.absolute_mut(address));
            stack.absolute_assign(address, value.clone().increment());
        }
        POST_DECREMENT_STATIC => {
            let address = prog.read_u32();
            let value = std::mem::take(stack.absolute_mut(address));
            stack.absolute_assign(address, value.clone().decrement());
        }

        POWER => stack.execute_infix(Value::power),
        MULTIPLY => stack.execute_infix(std::ops::Mul::mul),
        DIVIDE => stack.execute_infix(std::ops::Div::div),
        REMAINDER => stack.execute_infix(std::ops::Rem::rem),
        ADD => stack.execute_infix(std::ops::Add::add),
        SUBTRACT => stack.execute_infix(std::ops::Sub::sub),
        BITWISE_LEFT => stack.execute_infix(std::ops::Shl::shl),
        BITWISE_RIGHT => stack.execute_infix(std::ops::Shr::shr),
        BITWISE_RIGHT_UNSIGNED => stack.execute_infix(Value::bitwise_right_unsigned),
        LESS => stack.execute_infix(|l, r| Value::Boolean(l < r)),
        LESS_EQUAL => stack.execute_infix(|l, r| Value::Boolean(l <= r)),
        GREATER => stack.execute_infix(|l, r| Value::Boolean(l > r)),
        GREATER_EQUAL => stack.execute_infix(|l, r| Value::Boolean(l >= r)),
        IN => todo!("operador in"),
        INSTANCEOF => stack.execute_infix(|l, r| Value::Boolean(l.instanceof(r))),
        EQUAL => stack.execute_infix(|l, r| Value::Boolean(l.sloppy_equals(r))),
        NOT_EQUAL => stack.execute_infix(|l, r| Value::Boolean(!l.sloppy_equals(r))),
        IDENTICAL => stack.execute_infix(|l, r| Value::Boolean(l == r)),
        NOT_IDENTICAL => stack.execute_infix(|l, r| Value::Boolean(l != r)),
        BITWISE_AND => stack.execute_infix(std::ops::BitAnd::bitand),
        BITWISE_XOR => stack.execute_infix(std::ops::BitXor::bitxor),
        BITWISE_OR => stack.execute_infix(std::ops::BitOr::bitor),
        COMMA => {
            let value = stack.pop();
            stack.pop();
            stack.push(value);
        }

        TYPEOF => stack.execute_prefix(Value::type_name),
        VOID => *stack.top_mut() = Value::Undefined,
        BITWISE_NOT => stack.execute_prefix(Value::bitwise_not),
        NOT => stack.execute_prefix(|x| Value::Boolean(!x)),
        UNARY_PLUS => stack.execute_prefix(|x| x.to_number().to_value()),
        UNARY_MINUS => stack.execute_prefix(std::ops::Neg::neg),

        AWAIT => todo!("AWAIT"),

        TRY => todo!("TRY"),
        UNTRY => todo!("UNTRY"),
        THROW => todo!("THROW"),

        FINALLY_JMP => todo!("FINALLY_JMP"),
        FINALLY_RETURN => todo!("FINALLY_RETURN"),
        FINALLY_THROW => todo!("FINALLY_THROW"),
        FINALLY => todo!("FINALLY"),

        COUNT.. => panic!("instrução inválida"),
    }
    None
}
