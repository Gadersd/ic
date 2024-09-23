use crate::runtime::{Tag, Value, Port, PortType};







pub fn apply_operator(port_1: Port, port_2: Port) -> Port {
    let (tag_1, tag_2) = (port_1.get_tag(), port_2.get_tag());
    let (value_1, value_2) = (port_1.get_value(), port_2.get_value());

    let numeric_1 = Numeric::from_value(value_1).unwrap();
    let numeric_2 = Numeric::from_value(value_2).unwrap();

    let result = apply_numerics(numeric_1, numeric_2).unwrap();
    let value = result.to_value();

    Port::from_tag_value(tag_1, value)
}

pub fn apply_numerics(numeric_1: Numeric, numeric_2: Numeric) -> Option<Numeric> {
    match (numeric_1, numeric_2) {
        (Numeric::Number(_), Numeric::Number(_)) => None, 
        (Numeric::Operation(_), Numeric::Operation(_)) => None, 
        (Numeric::Sym(_), Numeric::Sym(_)) => None, 
        (Numeric::Sym(op), Numeric::Number(n)) => Some(Numeric::Operation(Operation{ operator: op, value: n.raw_value() })), 
        (Numeric::Number(n), Numeric::Sym(op)) => Some(Numeric::Operation(Operation{ operator: op, value: n.raw_value() })), 
        (Numeric::Operation(op), Numeric::Number(n)) => apply_operation(op, n).map(|n| Numeric::Number(n)), 
        (Numeric::Number(n), Numeric::Operation(op)) => apply_operation(op, n).map(|n| Numeric::Number(n)), 
        (Numeric::Operation(_), Numeric::Sym(_)) => panic!(), 
        (Numeric::Sym(_), Numeric::Operation(_)) => panic!(), 
    }
}

pub fn apply_operation(op: Operation, num: Number) -> Option<Number> {
    // Helper function to convert raw bits to the appropriate number type
    fn convert_raw_bits(raw: u32, num_type: &Number) -> Number {
        match num_type {
            Number::U24(_) => Number::U24(raw & 0xFFFFFF),
            Number::I24(_) => {
                let sign_extended = if raw & 0x800000 != 0 {
                    raw | 0xFF000000
                } else {
                    raw & 0xFFFFFF
                };
                Number::I24(sign_extended as i32)
            },
            Number::F24(_) => {
                let sign = (raw >> 23) & 1;
                let exponent = (raw >> 15) & 0xFF;
                let fraction = raw & 0x7FFF;
                let f32_bits = (sign << 31) | (exponent << 23) | (fraction << 8);
                Number::F24(f32::from_bits(f32_bits))
            }
        }
    }

    let op_value = convert_raw_bits(op.value, &num);

    match (op.operator, num, op_value) {
        (Operator::Add, Number::U24(n), Number::U24(v)) => Some(Number::U24(n.wrapping_add(v))),
        (Operator::Add, Number::I24(n), Number::I24(v)) => Some(Number::I24(n.wrapping_add(v))),
        (Operator::Add, Number::F24(n), Number::F24(v)) => Some(Number::F24(n + v)),

        (Operator::Subtract, Number::U24(n), Number::U24(v)) => Some(Number::U24(n.wrapping_sub(v))),
        (Operator::Subtract, Number::I24(n), Number::I24(v)) => Some(Number::I24(n.wrapping_sub(v))),
        (Operator::Subtract, Number::F24(n), Number::F24(v)) => Some(Number::F24(n - v)),

        (Operator::Multiply, Number::U24(n), Number::U24(v)) => Some(Number::U24(n.wrapping_mul(v))),
        (Operator::Multiply, Number::I24(n), Number::I24(v)) => Some(Number::I24(n.wrapping_mul(v))),
        (Operator::Multiply, Number::F24(n), Number::F24(v)) => Some(Number::F24(n * v)),

        (Operator::Divide, Number::U24(n), Number::U24(v)) => if v != 0 { Some(Number::U24(n / v)) } else { None },
        (Operator::Divide, Number::I24(n), Number::I24(v)) => if v != 0 { Some(Number::I24(n / v)) } else { None },
        (Operator::Divide, Number::F24(n), Number::F24(v)) => Some(Number::F24(n / v)),

        (Operator::Remainder, Number::U24(n), Number::U24(v)) => if v != 0 { Some(Number::U24(n % v)) } else { None },
        (Operator::Remainder, Number::I24(n), Number::I24(v)) => if v != 0 { Some(Number::I24(n % v)) } else { None },
        (Operator::Remainder, Number::F24(n), Number::F24(v)) => Some(Number::F24(n % v)),

        (Operator::Equal, Number::U24(n), Number::U24(v)) => Some(Number::U24((n == v) as u32)),
        (Operator::Equal, Number::I24(n), Number::I24(v)) => Some(Number::U24((n == v) as u32)),
        (Operator::Equal, Number::F24(n), Number::F24(v)) => Some(Number::U24((n == v) as u32)),

        (Operator::NotEqual, Number::U24(n), Number::U24(v)) => Some(Number::U24((n != v) as u32)),
        (Operator::NotEqual, Number::I24(n), Number::I24(v)) => Some(Number::U24((n != v) as u32)),
        (Operator::NotEqual, Number::F24(n), Number::F24(v)) => Some(Number::U24((n != v) as u32)),

        (Operator::LessThan, Number::U24(n), Number::U24(v)) => Some(Number::U24((n < v) as u32)),
        (Operator::LessThan, Number::I24(n), Number::I24(v)) => Some(Number::U24((n < v) as u32)),
        (Operator::LessThan, Number::F24(n), Number::F24(v)) => Some(Number::U24((n < v) as u32)),

        (Operator::GreaterThan, Number::U24(n), Number::U24(v)) => Some(Number::U24((n > v) as u32)),
        (Operator::GreaterThan, Number::I24(n), Number::I24(v)) => Some(Number::U24((n > v) as u32)),
        (Operator::GreaterThan, Number::F24(n), Number::F24(v)) => Some(Number::U24((n > v) as u32)),

        (Operator::And, Number::U24(n), Number::U24(v)) => Some(Number::U24(n & v)),
        (Operator::And, Number::I24(n), Number::I24(v)) => Some(Number::I24(n & v)),
        (Operator::And, Number::F24(n), Number::F24(v)) => Some(Number::F24(n.atan2(v))),

        (Operator::Or, Number::U24(n), Number::U24(v)) => Some(Number::U24(n | v)),
        (Operator::Or, Number::I24(n), Number::I24(v)) => Some(Number::I24(n | v)),
        (Operator::Or, Number::F24(n), Number::F24(v)) => Some(Number::F24(n.log(v))),

        (Operator::Xor, Number::U24(n), Number::U24(v)) => Some(Number::U24(n ^ v)),
        (Operator::Xor, Number::I24(n), Number::I24(v)) => Some(Number::I24(n ^ v)),
        (Operator::Xor, Number::F24(n), Number::F24(v)) => Some(Number::F24(n.powf(v))),

        (Operator::ShiftRight, Number::U24(n), Number::U24(v)) => Some(Number::U24(n >> (v & 0x1F))),
        (Operator::ShiftRight, Number::I24(n), Number::I24(v)) => Some(Number::I24(n >> (v & 0x1F))),
        (Operator::ShiftRight, Number::F24(_), _) => None,

        (Operator::ShiftLeft, Number::U24(n), Number::U24(v)) => Some(Number::U24(n << (v & 0x1F))),
        (Operator::ShiftLeft, Number::I24(n), Number::I24(v)) => Some(Number::I24(n << (v & 0x1F))),
        (Operator::ShiftLeft, Number::F24(_), _) => None,

        // Flipped operations
        (Operator::FpShiftLeft, Number::U24(n), Number::U24(v)) => Some(Number::U24(v << n)),
        (Operator::FpShiftLeft, Number::I24(n), Number::I24(v)) => Some(Number::I24(v << n)),

        (Operator::FpShiftRight, Number::U24(n), Number::U24(v)) => Some(Number::U24(v >> n)),
        (Operator::FpShiftRight, Number::I24(n), Number::I24(v)) => Some(Number::I24(v >> n)),

        (Operator::FpSubtract, Number::U24(n), Number::U24(v)) => Some(Number::U24(v - n)),
        (Operator::FpSubtract, Number::I24(n), Number::I24(v)) => Some(Number::I24(v - n)),
        (Operator::FpSubtract, Number::F24(n), Number::F24(v)) => Some(Number::F24(v - n)),

        (Operator::FpDivide, Number::U24(n), Number::U24(v)) => Some(Number::U24(v / n)),
        (Operator::FpDivide, Number::I24(n), Number::I24(v)) => Some(Number::I24(v / n)),
        (Operator::FpDivide, Number::F24(n), Number::F24(v)) => Some(Number::F24(v / n)),

        (Operator::FpRemainder, Number::U24(n), Number::U24(v)) => Some(Number::U24(v % n)),
        (Operator::FpRemainder, Number::I24(n), Number::I24(v)) => Some(Number::I24(v % n)),

        // Any other combination is not supported
        _ => None,
    }
}







#[derive(Debug, Clone)]
pub enum Numeric {
    Number(Number), 
    Operation(Operation), 
    Sym(Operator), 
}

impl Numeric {
    pub fn to_string(&self) -> String {
        match self {
            Numeric::Number(n) => n.to_string(), 
            Numeric::Operation(op) => op.to_string(), 
            Numeric::Sym(op) => format!("{:?}", op), 
        }
    }

    pub fn from_value(value: Value) -> Option<Self> {
        let tag = Self::numeric_value_to_tag(value);
        let raw_value: u32 = Self::numeric_value_to_value(value);

        match tag {
            0 => Some(Numeric::Number(Number::U24(raw_value & 0xFFFFFF))), // U24: Use lower 24 bits
            1 => {
                // I24: Sign-extend the 24-bit value to 32 bits
                let sign_extended = if raw_value & 0x800000 != 0 {
                    raw_value | 0xFF000000
                } else {
                    raw_value & 0xFFFFFF
                };
                Some(Numeric::Number(Number::I24(sign_extended as i32)))
            },
            2 => {
                // F24: Convert to IEEE 754 binary32
                let sign = (raw_value >> 23) & 1;
                let exponent = (raw_value >> 15) & 0xFF;
                let fraction = raw_value & 0x7FFF;
                
                let f32_bits = (sign << 31) | (exponent << 23) | (fraction << 8);
                Some(Numeric::Number(Number::F24(f32::from_bits(f32_bits))))
            },
            3 => Some(Numeric::Operation(Operation { operator: Operator::Add, value: raw_value })),
            4 => Some(Numeric::Operation(Operation { operator: Operator::Subtract, value: raw_value })),
            5 => Some(Numeric::Operation(Operation { operator: Operator::Multiply, value: raw_value })),
            6 => Some(Numeric::Operation(Operation { operator: Operator::Divide, value: raw_value })),
            7 => Some(Numeric::Operation(Operation { operator: Operator::Remainder, value: raw_value })),
            8 => Some(Numeric::Operation(Operation { operator: Operator::Equal, value: raw_value })),
            9 => Some(Numeric::Operation(Operation { operator: Operator::NotEqual, value: raw_value })),
            10 => Some(Numeric::Operation(Operation { operator: Operator::LessThan, value: raw_value })),
            11 => Some(Numeric::Operation(Operation { operator: Operator::GreaterThan, value: raw_value })),
            12 => Some(Numeric::Operation(Operation { operator: Operator::And, value: raw_value })),
            13 => Some(Numeric::Operation(Operation { operator: Operator::Or, value: raw_value })),
            14 => Some(Numeric::Operation(Operation { operator: Operator::Xor, value: raw_value })),
            15 => Some(Numeric::Operation(Operation { operator: Operator::ShiftRight, value: raw_value })),
            16 => Some(Numeric::Operation(Operation { operator: Operator::ShiftLeft, value: raw_value })),
            17 => Some(Numeric::Operation(Operation { operator: Operator::FpSubtract, value: raw_value })),
            18 => Some(Numeric::Operation(Operation { operator: Operator::FpDivide, value: raw_value })),
            19 => Some(Numeric::Operation(Operation { operator: Operator::FpRemainder, value: raw_value })),
            20 => Some(Numeric::Operation(Operation { operator: Operator::FpShiftRight, value: raw_value })),
            21 => Some(Numeric::Operation(Operation { operator: Operator::FpShiftLeft, value: raw_value })),
            22 => Some(Numeric::Sym(Operator::iter().nth(raw_value as usize)?)),
            _ => None,
        }
    }

    pub fn to_port(&self) -> Port {
        Port::from_type_value(PortType::Numeric, self.to_value())
    }

    pub fn to_value(&self) -> Value {
        match self {
            Numeric::Number(num) => num.to_value(), 
            Numeric::Operation(op) => op.to_value(),
            Numeric::Sym(op) => 22u32 | (( Operator::iter().enumerate().find(|(i, o)| op == o).unwrap().0 as u32) << 5 ), 
        }
    }

    fn numeric_value_to_tag(value: Value) -> Tag {
        (value & 0b11111) as Tag
    }
    
    fn numeric_value_to_value(value: Value) -> Value {
        value >> 5
    }
}







#[derive(Debug, Clone)]
pub struct Operation {
    pub operator: Operator, 
    pub value: u32, 
}

/*impl fmt::Debug for Operation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Operation {{ field: {} }}", self.field)
    }
}*/

impl Operation {
    pub fn to_string(&self) -> String {
        format!("[{:?}{}]", self.operator, self.value)
    }

    pub fn to_value(&self) -> Value {
        let type_bits = match self.operator {
            Operator::Add => 3u32,
            Operator::Subtract => 4u32,
            Operator::Multiply => 5u32,
            Operator::Divide => 6u32,
            Operator::Remainder => 7u32,
            Operator::Equal => 8u32,
            Operator::NotEqual => 9u32,
            Operator::LessThan => 10u32,
            Operator::GreaterThan => 11u32,
            Operator::And => 12u32,
            Operator::Or => 13u32,
            Operator::Xor => 14u32,
            Operator::ShiftRight => 15u32,
            Operator::ShiftLeft => 16u32,
            Operator::FpSubtract => 17u32,
            Operator::FpDivide => 18u32,
            Operator::FpRemainder => 19u32,
            Operator::FpShiftRight => 20u32,
            Operator::FpShiftLeft => 21u32,
        };
        type_bits | self.value << 5
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Number {
    U24(u32), 
    I24(i32), 
    F24(f32), 
}

impl Number {
    pub fn to_string(self) -> String {
        match self {
            Number::U24(n) => n.to_string(), 
            Number::I24(n) => n.to_string(), 
            Number::F24(n) => n.to_string(), 
        }
    }

    pub fn to_value(self) -> Value {
        let raw_value = self.raw_value() << 5;
        let tag_part = self.tag() as u32;
        tag_part | raw_value
    }

    pub fn tag(self) -> Tag {
        match self {
            Number::U24(_) => 0, 
            Number::I24(_) => 1, 
            Number::F24(_) => 2, 
        }
    }

    pub fn raw_value(self) -> Value {
        match self {
            Number::U24(n) => n & 0xFFFFFF,
            Number::I24(n) => n as u32 & 0xFFFFFF,
            Number::F24(f) => {
                let bits = f.to_bits();
                let sign = bits >> 31;
                let exponent = (bits >> 23) & 0xFF;
                let fraction = (bits >> 8) & 0x7FFF;
                (sign << 23) | (exponent << 15) | fraction
            }
        }
    }
}




#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Operator {
    Add, 
    Subtract, 
    Multiply, 
    Divide, 
    Remainder, 
    Equal, 
    NotEqual, 
    LessThan, 
    GreaterThan, 
    And, 
    Or, 
    Xor, 
    ShiftRight, 
    ShiftLeft, 
    FpSubtract, 
    FpDivide, 
    FpRemainder, 
    FpShiftRight, 
    FpShiftLeft, 
}

impl Operator {
    pub fn as_str(self) -> &'static str {
        match self {
            Operator::Add => "+",
            Operator::Subtract => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::Remainder => "%",
            Operator::Equal => "=",
            Operator::NotEqual => "!=",
            Operator::LessThan => "<",
            Operator::GreaterThan => ">",
            Operator::And => "&",
            Operator::Or => "|",
            Operator::Xor => "^",
            Operator::ShiftRight => ".>>",
            Operator::ShiftLeft => ".<<",
            Operator::FpSubtract => ":-",
            Operator::FpDivide => ":/",
            Operator::FpRemainder => ":%",
            Operator::FpShiftRight => ":>>",
            Operator::FpShiftLeft => ":<<",
        }
    }

    pub fn iter() -> impl Iterator<Item=Self> + Clone + ExactSizeIterator {
        [
            Operator::ShiftRight, 
            Operator::ShiftLeft, 
            Operator::Add, 
            Operator::Subtract, 
            Operator::Multiply, 
            Operator::Divide, 
            Operator::Remainder, 
            Operator::Equal, 
            Operator::NotEqual, 
            Operator::LessThan, 
            Operator::GreaterThan, 
            Operator::And, 
            Operator::Or, 
            Operator::Xor, 
            Operator::FpSubtract, 
            Operator::FpDivide, 
            Operator::FpRemainder, 
            Operator::FpShiftRight, 
            Operator::FpShiftLeft, 
        ].into_iter()
    }
}