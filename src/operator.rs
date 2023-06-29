#![allow(unused_variables)]
use crate::runtime::{js_string, Key, Number, String, Value};

enum NumberPair {
    Integer(i64, i64),
    Float(f64, f64),
}

impl Value {
    pub fn power(self, rhs: Self) -> Self {
        match num_pair(&self, &rhs) {
            NumberPair::Integer(lhs, rhs) => {
                if rhs < 0 || rhs > u32::MAX as i64 {
                    Number::Float((lhs as f64).powf(rhs as f64))
                } else if let Some(out) = lhs.checked_pow(rhs as u32) {
                    Number::Integer(out)
                } else {
                    Number::Float((lhs as f64).powf(rhs as f64))
                }
            }
            NumberPair::Float(lhs, rhs) => Number::Float(lhs.powf(rhs)),
        }
        .to_value()
    }
    pub fn increment(self) -> Self {
        match self.to_number() {
            Number::Integer(i) => Number::Integer(i + 1),
            Number::Float(f) => Number::Float(f + 1.0),
        }
        .to_value()
    }
    pub fn decrement(self) -> Self {
        match self.to_number() {
            Number::Integer(i) => Number::Integer(i - 1),
            Number::Float(f) => Number::Float(f - 1.0),
        }
        .to_value()
    }
    pub fn type_name(self) -> Self {
        match self {
            Self::Undefined => Self::String(js_string!("undefined")),
            Self::Null => Self::String(js_string!("object")),
            Self::Boolean(_) => Self::String(js_string!("boolean")),
            Self::Integer(_) => Self::String(js_string!("number")),
            Self::Float(_) => Self::String(js_string!("float")),
            Self::String(_) => Self::String(js_string!("string")),
            Self::Object(object) if object.is_function() => {
                Self::String(js_string!("function"))
            }
            Self::Object(_) => Self::String(js_string!("object")),
            Self::Symbol(_, _) => Self::String(js_string!("symbol")),
        }
    }
    pub fn bitwise_not(self) -> Self {
        Self::Integer((!self.to_integer() as i32) as i64)
    }
    pub fn bitwise_right_unsigned(self, rhs: Self) -> Self {
        let (lhs, rhs) = int_pair(self, rhs);
        let shift = (rhs & 0x1F) as u32;
        Self::Integer((lhs as u32).rotate_right(shift) as i64)
    }
    pub fn sloppy_equals(self, rhs: Self) -> bool {
        match (self, rhs) {
            (Self::Symbol(0, lhs), Self::Symbol(0, rhs)) => lhs == rhs,
            (Self::Symbol(lhs, _), Self::Symbol(rhs, _)) => lhs == rhs,
            (Self::Symbol(_, _), _) | (_, Self::Symbol(_, _)) => false,
            (Self::String(lhs), Self::String(rhs)) => lhs == rhs,
            (lhs, Self::String(rhs)) => lhs.to_string() == rhs,
            (Self::String(lhs), rhs) => lhs == rhs.to_string(),
            (lhs, rhs) => match num_pair(&lhs, &rhs) {
                NumberPair::Integer(lhs, rhs) => lhs == rhs,
                NumberPair::Float(lhs, rhs) => lhs == rhs,
            },
        }
    }
    pub fn instanceof(self, rhs: Self) -> bool {
        if let (Self::Object(mut object), Self::Object(constructor)) = (self, rhs) {
            if !constructor.is_function() {
                todo!("error: não é um construtor");
            }
            const PROTOTYPE_KEY: Key = Key::String(js_string!("prototype"));
            let Some(Self::Object(constructor_prototype)) = constructor.property(PROTOTYPE_KEY.clone()) else {
                return false;
            };
            loop {
                let Some(prototype) = object.prototype() else {
                    return false;
                };
                if prototype == constructor_prototype {
                    return true;
                }
                object = prototype;
            }
        } else {
            false
        }
    }
    pub fn to_string(self) -> String {
        match self {
            Self::Undefined => js_string!("undefined"),
            Self::Null => js_string!("null"),
            Self::Boolean(true) => js_string!("true"),
            Self::Boolean(false) => js_string!("false"),
            Self::Integer(integer) => String::from_integer(integer),
            Self::Float(float) => String::from_float(float),
            Self::String(string) => string,
            Self::Object(object) => {
                //todo!("Object to_string");
                js_string!("object")
            }
            Self::Symbol(_, name) => {
                if name.len() == 0 {
                    js_string!("Symbol()")
                } else {
                    let mut vec = Vec::with_capacity(8 + name.len());
                    vec.extend("Symbol(".bytes().map(|x| x as u16));
                    vec.extend_from_slice(name.as_slice());
                    vec.push(')' as u16);
                    String::Owned(vec)
                }
            }
        }
    }
    pub fn to_number(&self) -> Number {
        match self {
            Self::Undefined => Number::Float(f64::NAN),
            Self::Null => Number::Integer(0),
            Self::Boolean(true) => Number::Integer(1),
            Self::Boolean(false) => Number::Integer(0),
            Self::Integer(i) => Number::Integer(*i),
            Self::Float(f) => Number::Float(*f),
            Self::String(string) => string.to_number(),
            Self::Object(_) => {
                //todo!("Object to_number"),
                Number::Float(f64::NAN)
            }
            Self::Symbol(_, _) => todo!("erro: não é possível converter symbol para numero"),
        }
    }
    pub fn to_integer(self) -> i64 {
        match self {
            Self::Undefined => 0,
            Self::Null => 0,
            Self::Boolean(true) => 1,
            Self::Boolean(false) => 0,
            Self::Integer(i) => i,
            Self::Float(f) => f as i64,
            Self::String(string) => string.to_number().to_int(),
            Self::Object(_) => {
                //todo!("Object to_number"),
                0
            }
            Self::Symbol(_, _) => todo!("erro: não é possível converter symbol para numero"),
        }
    }
    pub fn to_boolean(&self) -> bool {
        match self {
            Self::Undefined => false,
            Self::Null => false,
            Self::Boolean(boolean) => *boolean,
            Self::Integer(integer) => *integer != 0,
            Self::Float(float) => *float != 0.0 && !float.is_nan(),
            Self::String(string) => string.len() != 0,
            Self::Object(_) => true,
            Self::Symbol(_, _) => true,
        }
    }
}

impl std::ops::Not for Value {
    type Output = bool;
    fn not(self) -> Self::Output {
        !self.to_boolean()
    }
}
impl std::ops::Neg for Value {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self.to_number() {
            Number::Integer(i) => Number::Integer(-i).to_value(),
            Number::Float(f) => Number::Float(-f).to_value(),
        }
    }
}
impl std::ops::Add for Value {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::String(lhs), Self::String(rhs)) => Self::String(lhs + rhs),
            (lhs, Self::String(rhs)) => Self::String(lhs.to_string() + rhs),
            (Self::String(lhs), rhs) => Self::String(lhs + rhs.to_string()),
            (lhs, rhs) => num_op(lhs, rhs, i64::checked_add, std::ops::Add::add),
        }
    }
}
impl std::ops::Sub for Value {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        num_op(self, rhs, i64::checked_sub, std::ops::Sub::sub)
    }
}
impl std::ops::Mul for Value {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        num_op(self, rhs, i64::checked_mul, std::ops::Mul::mul)
    }
}
impl std::ops::Div for Value {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        num_op(self, rhs, i64::checked_div, std::ops::Div::div)
    }
}
impl std::ops::Rem for Value {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self::Output {
        num_op(self, rhs, i64::checked_rem, std::ops::Rem::rem)
    }
}
impl std::ops::BitAnd for Value {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        let (lhs, rhs) = int_pair(self, rhs);
        Value::Integer((lhs & rhs) as i64)
    }
}
impl std::ops::BitOr for Value {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        let (lhs, rhs) = int_pair(self, rhs);
        Value::Integer((lhs | rhs) as i64)
    }
}
impl std::ops::BitXor for Value {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output {
        let (lhs, rhs) = int_pair(self, rhs);
        Value::Integer((lhs ^ rhs) as i64)
    }
}
impl std::ops::Shl for Value {
    type Output = Self;
    fn shl(self, rhs: Self) -> Self::Output {
        let (lhs, rhs) = int_pair(self, rhs);
        let shift = (rhs & 0x1F) as u32;
        Value::Integer(lhs.wrapping_shl(shift) as i64)
    }
}
impl std::ops::Shr for Value {
    type Output = Self;
    fn shr(self, rhs: Self) -> Self::Output {
        let (lhs, rhs) = int_pair(self, rhs);
        let shift = (rhs & 0x1F) as u32;
        Value::Integer(lhs.wrapping_shr(shift) as i64)
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Undefined, Self::Undefined) => true,
            (Self::Null, Self::Null) => true,
            (Self::Boolean(lhs), Self::Boolean(rhs)) => lhs == rhs,
            (Self::Integer(lhs), Self::Integer(rhs)) => lhs == rhs,
            (Self::Float(lhs), Self::Float(rhs)) => lhs == rhs,
            (Self::String(lhs), Self::String(rhs)) => lhs == rhs,
            (Self::Object(lhs), Self::Object(rhs)) => lhs == rhs,
            (Self::Symbol(0, lhs), Self::Symbol(0, rhs)) => lhs == rhs,
            (Self::Symbol(lhs, _), Self::Symbol(rhs, _)) => lhs == rhs,
            (Self::Symbol(_, _), _) | (_, Self::Symbol(_, _)) => false,
            _ => false,
        }
    }
}
impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Symbol(0, lhs), Self::Symbol(0, rhs)) if lhs == rhs => Some(std::cmp::Ordering::Equal),
            (Self::Symbol(lhs, _), Self::Symbol(rhs, _)) if lhs == rhs => Some(std::cmp::Ordering::Equal),
            (Self::Symbol(_, _), _) | (_, Self::Symbol(_, _)) => None,
            (Self::String(lhs), Self::String(rhs)) => PartialOrd::partial_cmp(lhs, rhs),
            (lhs, Self::String(rhs)) => PartialOrd::partial_cmp(&lhs.clone().to_string(), rhs),
            (Self::String(lhs), rhs) => PartialOrd::partial_cmp(lhs, &rhs.clone().to_string()),
            (lhs, rhs) => match num_pair(lhs, rhs) {
                NumberPair::Integer(lhs, rhs) => PartialOrd::partial_cmp(&lhs, &rhs),
                NumberPair::Float(lhs, rhs) => PartialOrd::partial_cmp(&lhs, &rhs),
            },
        }
    }
}

fn num_pair(l: &Value, r: &Value) -> NumberPair {
    match (l.to_number(), r.to_number()) {
        (Number::Integer(l), Number::Integer(r)) => NumberPair::Integer(l, r),
        (Number::Integer(l), Number::Float(r)) => NumberPair::Float(l as f64, r),
        (Number::Float(l), Number::Integer(r)) => NumberPair::Float(l, r as f64),
        (Number::Float(l), Number::Float(r)) => NumberPair::Float(l, r),
    }
}

fn int_pair(l: Value, r: Value) -> (i32, i32) {
    match (l.to_number(), r.to_number()) {
        (Number::Integer(l), Number::Integer(r)) => (l as i32, r as i32),
        (Number::Integer(l), Number::Float(r)) => (l as i32, r as i32),
        (Number::Float(l), Number::Integer(r)) => (l as i32, r as i32),
        (Number::Float(l), Number::Float(r)) => (l as i32, r as i32),
    }
}

fn num_op(
    l: Value,
    r: Value,
    int_op: impl Fn(i64, i64) -> Option<i64>,
    float_op: impl Fn(f64, f64) -> f64,
) -> Value {
    match num_pair(&l, &r) {
        NumberPair::Integer(l, r) => {
            if let Some(out) = int_op(l, r) {
                Number::Integer(out)
            } else {
                Number::Float(float_op(l as f64, r as f64))
            }
        }
        NumberPair::Float(l, r) => Number::Float(float_op(l, r)),
    }
    .to_value()
}
