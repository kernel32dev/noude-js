use crate::runtime::{Key, Memory, Object, State, String, Value};

use super::{js_string, Function, Soul, Symbol};

macro_rules! enum_native {
    ($($variant:ident,)*) => {
        #[allow(non_camel_case_types)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum Native {
            $($variant,)*
        }
        impl Native {
            fn to_static_utf8(self) -> &'static str {
                match self {
                    $(Self::$variant => stringify!($variant),)*
                }
            }
            fn to_static_utf16(self) -> &'static [u16] {
                match self {
                    $(Self::$variant => &utf16_lit::utf16!(stringify!($variant)),)*
                }
            }
        }
    };
}

enum_native! {
    Symbol,
    ConsoleLog,
}

pub mod known_symbols {
    pub const ALL: &[(i64, &'static [u16])] = &[
        PROTOTYPE,
        FOR,
        KEY_FOR,
        ASYNC_ITERATOR,
        HAS_INSTANCE,
        IS_CONCAT_SPREADABLE,
        ITERATOR,
        MATCH,
        MATCH_ALL,
        REPLACE,
        SEARCH,
        SPECIES,
        SPLIT,
        TO_PRIMITIVE,
        TO_STRING_TAG,
        UNSCOPABLES,
    ];
    pub const PROTOTYPE: (i64, &'static [u16]) = (-1, &utf16_lit::utf16!("Symbol.prototype"));
    pub const FOR: (i64, &'static [u16]) = (-2, &utf16_lit::utf16!("Symbol.for"));
    pub const KEY_FOR: (i64, &'static [u16]) = (-3, &utf16_lit::utf16!("Symbol.keyFor"));
    pub const ASYNC_ITERATOR: (i64, &'static [u16]) =
        (-4, &utf16_lit::utf16!("Symbol.asyncIterator"));
    pub const HAS_INSTANCE: (i64, &'static [u16]) = (-5, &utf16_lit::utf16!("Symbol.hasInstance"));
    pub const IS_CONCAT_SPREADABLE: (i64, &'static [u16]) =
        (-6, &utf16_lit::utf16!("Symbol.isConcatSpreadable"));
    pub const ITERATOR: (i64, &'static [u16]) = (-7, &utf16_lit::utf16!("Symbol.iterator"));
    pub const MATCH: (i64, &'static [u16]) = (-8, &utf16_lit::utf16!("Symbol.match"));
    pub const MATCH_ALL: (i64, &'static [u16]) = (-9, &utf16_lit::utf16!("Symbol.matchAll"));
    pub const REPLACE: (i64, &'static [u16]) = (-10, &utf16_lit::utf16!("Symbol.replace"));
    pub const SEARCH: (i64, &'static [u16]) = (-11, &utf16_lit::utf16!("Symbol.search"));
    pub const SPECIES: (i64, &'static [u16]) = (-12, &utf16_lit::utf16!("Symbol.species"));
    pub const SPLIT: (i64, &'static [u16]) = (-13, &utf16_lit::utf16!("Symbol.split"));
    pub const TO_PRIMITIVE: (i64, &'static [u16]) = (-14, &utf16_lit::utf16!("Symbol.toPrimitive"));
    pub const TO_STRING_TAG: (i64, &'static [u16]) =
        (-15, &utf16_lit::utf16!("Symbol.toStringTag"));
    pub const UNSCOPABLES: (i64, &'static [u16]) = (-16, &utf16_lit::utf16!("Symbol.unscopables"));
}

impl Native {
    pub fn add_native_objects(memory: &Memory) {
        let symbol = memory.obj();
        for (known_id, known_name) in known_symbols::ALL {
            let key = String::Static(&(*known_name)[7..]);
            symbol.assign_property(
                key,
                Symbol {
                    id: *known_id,
                    name: String::Static(*known_name),
                },
            );
        }
        memory
            .global
            .assign_property(js_string!("Symbol"), Value::Object(symbol));
        let console = memory.obj();
        console.assign_property(js_string!("log"), Self::ConsoleLog.to_object(memory));
        memory
            .global
            .assign_property(js_string!("console"), Value::Object(console));
    }

    pub fn to_key(self) -> Key {
        Key::String(String::Static(self.to_static_utf16()))
    }

    pub fn to_object(self, memory: &Memory) -> Object {
        Object::from_soul(Soul::Function(Function::Native(self)), memory)
    }

    pub fn call_as_function(
        self, state: &mut State, memory: &Memory, this: Value, argc: usize,
    ) -> Value {
        match self {
            Native::Symbol => todo!("Native::Symbol.to_function"),
            Native::ConsoleLog => {
                for argi in 0..argc as i32 {
                    print!("{}", state.stack.absolute(((argi as usize) + state.stack.len() - argc) as u32));
                }
                println!("");
                Value::Undefined
            },
        }
    }

    pub fn call_as_constructor(self, state: &mut State, memory: &Memory, argc: usize) -> Object {
        todo!("call_as_constructor")
    }
}
