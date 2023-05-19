#![allow(unused_variables)]
use crate::runtime::{cstring16, Number, String, Value};

enum NumberPair {
    Integer(i64, i64),
    Float(f64, f64),
}

impl Value {
    pub fn power(self, rhs: Value) -> Value {
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
    pub fn increment(self) -> Value {
        match self.to_number() {
            Number::Integer(i) => Number::Integer(i + 1),
            Number::Float(f) => Number::Float(f + 1.0),
        }
        .to_value()
    }
    pub fn decrement(self) -> Value {
        match self.to_number() {
            Number::Integer(i) => Number::Integer(i - 1),
            Number::Float(f) => Number::Float(f - 1.0),
        }
        .to_value()
    }
    pub fn type_name(self) -> Value {
        match self {
            Value::Undefined => Value::String(cstring16!('u' 'n' 'd' 'e' 'f' 'i' 'n' 'e' 'd')),
            Value::Null => Value::String(cstring16!('o' 'b' 'j' 'e' 'c' 't')),
            Value::Boolean(_) => Value::String(cstring16!('b' 'o' 'o' 'l' 'e' 'a' 'n')),
            Value::Integer(_) => Value::String(cstring16!('n' 'u' 'm' 'b' 'e' 'r')),
            Value::Float(_) => Value::String(cstring16!('f' 'l' 'o' 'a' 't')),
            Value::String(_) => Value::String(cstring16!('s' 't' 'r' 'i' 'n' 'g')),
            Value::Object(object) if object.is_function() => {
                Value::String(cstring16!('f' 'u' 'n' 'c' 't' 'i' 'o' 'n'))
            }
            Value::Object(_) => Value::String(cstring16!('o' 'b' 'j' 'e' 'c' 't')),
        }
    }
    pub fn bitwise_not(self) -> Value {
        Value::Integer((!self.to_integer() as i32) as i64)
    }
    pub fn bitwise_right_unsigned(self, rhs: Value) -> Value {
        let (lhs, rhs) = int_pair(self, rhs);
        let shift = (rhs & 0x1F) as u32;
        Value::Integer((lhs as u32).rotate_right(shift) as i64)
    }
    pub fn sloppy_equals(self, rhs: Value) -> bool {
        match (self, rhs) {
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            (lhs, Value::String(rhs)) => lhs.to_string() == rhs,
            (Value::String(lhs), rhs) => lhs == rhs.to_string(),
            (lhs, rhs) => match num_pair(&lhs, &rhs) {
                NumberPair::Integer(lhs, rhs) => lhs == rhs,
                NumberPair::Float(lhs, rhs) => lhs == rhs,
            },
        }
    }
    pub fn instanceof(self, rhs: Value) -> bool {
        if let (Value::Object(mut object), Value::Object(constructor)) = (self, rhs) {
            if !constructor.is_function() {
                todo!("error: não é um construtor");
            }
            const PROTOTYPE_KEY: String = cstring16!('p' 'r' 'o' 't' 'o' 't' 'y' 'p' 'e');
            let Value::Object(constructor_prototype) = constructor.property(&PROTOTYPE_KEY) else {
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
            Value::Undefined => cstring16!('u' 'n' 'd' 'e' 'f' 'i' 'n' 'e' 'd'),
            Value::Null => cstring16!('n' 'u' 'l' 'l'),
            Value::Boolean(true) => cstring16!('t' 'r' 'u' 'e'),
            Value::Boolean(false) => cstring16!('f' 'a' 'l' 's' 'e'),
            Value::Integer(integer) => String::Owned(integer.to_string().encode_utf16().collect()),
            Value::Float(float) => String::Owned(float.to_string().encode_utf16().collect()),
            Value::String(string) => string,
            Value::Object(object) => {
                //todo!("Object to_string");
                cstring16!('o' 'b' 'j' 'e' 'c' 't')
            },
        }
    }
    pub fn to_number(&self) -> Number {
        match self {
            Value::Undefined => Number::Float(f64::NAN),
            Value::Null => Number::Float(f64::NAN),
            Value::Boolean(true) => Number::Integer(1),
            Value::Boolean(false) => Number::Integer(0),
            Value::Integer(i) => Number::Integer(*i),
            Value::Float(f) => Number::Float(*f),
            Value::String(string) => string.to_number(),
            Value::Object(_) => {
                //todo!("Object to_number"),
                Number::Float(f64::NAN)
            }
        }
    }
    pub fn to_integer(self) -> i64 {
        match self {
            Value::Undefined => 0,
            Value::Null => 0,
            Value::Boolean(true) => 1,
            Value::Boolean(false) => 0,
            Value::Integer(i) => i,
            Value::Float(f) => f as i64,
            Value::String(string) => string.to_number().to_int(),
            Value::Object(_) => {
                //todo!("Object to_number"),
                0
            }
        }
    }
    pub fn to_boolean(&self) -> bool {
        match self {
            Value::Undefined => false,
            Value::Null => false,
            Value::Boolean(boolean) => *boolean,
            Value::Integer(integer) => *integer != 0,
            Value::Float(float) => *float != 0.0 && !float.is_nan(),
            Value::String(string) => string.len() != 0,
            Value::Object(_) => true,
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
            (Value::String(lhs), Value::String(rhs)) => Value::String(lhs + rhs),
            (lhs, Value::String(rhs)) => Value::String(lhs.to_string() + rhs),
            (Value::String(lhs), rhs) => Value::String(lhs + rhs.to_string()),
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
            _ => false,
        }
    }
}
impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::String(lhs), Value::String(rhs)) => PartialOrd::partial_cmp(lhs, rhs),
            (lhs, Value::String(rhs)) => PartialOrd::partial_cmp(&lhs.clone().to_string(), rhs),
            (Value::String(lhs), rhs) => PartialOrd::partial_cmp(lhs, &rhs.clone().to_string()),
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
