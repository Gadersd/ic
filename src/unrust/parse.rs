use crate::unrust::lexical::{Token, lexical_parse};

use std::iter;

use crate::numeric::{self, Operator};
use crate::parser::{self, Node, Constructor, Duplicator, Alphanumeric, Redex, Book};
use crate::unrust::core::*;

use std::collections::HashMap;
use std::cmp::Ordering;





#[derive(Debug, Clone, Copy)]
pub enum VarUsage {
    Introduce, 
    Consume, 
}

pub trait VarMap<SCtx: Clone, GCtx> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) where Self: Sized;
}


#[derive(Debug, Clone)]
pub struct Program<E> {
    pub functions: HashMap<String, Function<E>>, 
    pub structs: HashMap<String, Struct>, 
    pub enums: HashMap<String, Enum>, 
}

impl<E> Program<E> {
    pub fn is_global(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    pub fn get_struct(&self, name: &str) -> Option<&Struct> {
        self.structs.get(name)
    }

    pub fn get_enum(&self, name: &str) -> Option<&Enum> {
        self.enums.get(name)
    }
}

// Should the program live in a scope of its own?
impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for Program<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let mut functions = HashMap::new();
        
        let (functions, ctx, gctx) = self.functions.into_iter().fold((functions, ctx, gctx), |(mut functions, ctx, gctx), (func_name, function)| {
            let (function, ctx, gctx) = function.map_vars(f.clone(), ctx, gctx);
            functions.insert(func_name, function);
            (functions, ctx, gctx)
        });

        let structs = self.structs;
        let enums = self.enums;

        (
            Self {
                functions, 
                structs, 
                enums, 
            }, 
            ctx, 
            gctx, 
        )
    }
}

impl<E: Parse> Program<E> {
    pub fn from_str(s: &str) -> Result<Self, ParseError> {
        let tokens = lexical_parse(s).map_err(|s| ParseError {index: 0, msg: s})?;
        println!("Tokens: {:?}", tokens);
        let text_cursor = TextCursor::new(&tokens[..]);
        let (program, s) = Self::parse(text_cursor)?;
        Ok(program)
    }
}

impl<E: Parse> Parse for Program<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let mut functions = HashMap::new();
        let mut structs = HashMap::new();
        let mut enums = HashMap::new();

        let (defs, s) = parse_nonempty_vec(s, Definition::parse)?;

        for def in defs {
            match def {
                Definition::Function(f) => {functions.insert(f.name.clone().into_string(), f);}, 
                Definition::Struct(s) => {structs.insert(s.name.clone().into_string(), s);}, 
                Definition::Enum(e) => {enums.insert(e.name.clone().into_string(), e);}
            };
        }

        Ok(
            (Program {
                functions, 
                structs, 
                enums, 
            }, 
            s)
        )
    }
}

#[derive(Debug, Clone)]
pub enum Definition<E> {
    Function(Function<E>), 
    Struct(Struct), 
    Enum(Enum), 
}

impl<E> From<Function<E>> for Definition<E> {
    fn from(function: Function<E>) -> Self {
        Definition::Function(function)
    }
}

impl<E> From<Struct> for Definition<E> {
    fn from(struct_def: Struct) -> Self {
        Definition::Struct(struct_def)
    }
}

impl<E> From<Enum> for Definition<E> {
    fn from(enum_def: Enum) -> Self {
        Definition::Enum(enum_def)
    }
}

impl<E: Parse> Parse for Definition<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let x = Function::parse(s.clone()).map(|(x, s)| (x.into(), s));
        let x = propagating_parse(s.clone(), |s| Struct::parse(s).map(|(x, s)| (x.into(), s)), x);
        let x = propagating_parse(s.clone(), |s| Enum::parse(s).map(|(x, s)| (x.into(), s)), x);

        x
    }
}

#[derive(Debug, Clone)]
pub struct Function<E> {
    pub name: Name, 
    pub parameters: Vec<Variable>, 
    pub block: Block<E>, 
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for Function<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let (func_name, ctx, gctx) = f(self.name.into(), VarUsage::Introduce, ctx, gctx);
        let name = func_name.name.into_string().into();

        let mut parameters = Vec::with_capacity(self.parameters.len());
        let (parameters, ctx_inner, gctx) = self.parameters.into_iter().fold((parameters, ctx.clone(), gctx), |(mut parameters, ctx, gctx), param| {
            let (param_name, ctx, gctx) = f(param, VarUsage::Introduce, ctx, gctx);
            parameters.push(param_name);
            (parameters, ctx, gctx)
        });

        let (block, _, gctx) = self.block.map_vars(f, ctx_inner, gctx);

        (
            Function {
                name, 
                parameters, 
                block, 
            }, 
            ctx, 
            gctx, 
        )
    }
}

impl<E: Parse> Parse for Function<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let s = parse_specific_token(s, &Token::Fn).map_err(|e| ParseError::new(e.index, "Expected fn".into()))?;
        let (name, s) = Name::parse(s)?;
        let (parameters, s) = Self::parse_params(s)?;
        let (block, s) = Block::parse(s)?;

        let f = Self {
            name, 
            parameters, 
            block, 
        };

        Ok((f, s))
    }
}

impl<E: Parse> Function<E> {
    //type Error = ();

    fn parse_params<'a>(s: TextCursor<'a>) -> ParseResult<'a, Vec<Variable>> {
        let s = parse_specific_token(s, &Token::LeftParenthesis).map_err(|e| ParseError::new(e.index, "Expected left parenthesis".into()))?;
        //let (params, s) = parse_vec(s, Variable::parse);
        let (params, s) = parse_comma_delimited(s, Variable::parse);
        let s = parse_specific_token(s, &Token::RightParenthesis).map_err(|e| ParseError::new(e.index, "Expected right parenthesis".into()))?;

        Ok((params, s))
    }
}



#[derive(Debug, Clone)]
pub struct IfElse<E> {
    pub condition: E, 
    pub block_pass: Block<E>, 
    pub block_fail: Block<E>, 
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for IfElse<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let (condition, _, gctx) = self.condition.map_vars(f.clone(), ctx.clone(), gctx);
        let (block_pass, _, gctx) = self.block_pass.map_vars(f.clone(), ctx.clone(), gctx);
        let (block_fail, _, gctx) = self.block_fail.map_vars(f, ctx.clone(), gctx);

        (
            Self {
                condition, 
                block_pass, 
                block_fail, 
            }, 
            ctx, 
            gctx, 
        )
    }
}

impl<E: From<IfElse<E>>> From<IfElses<E>> for IfElse<E> {
    fn from(mut if_elses: IfElses<E>) -> Self {
        let condition = if_elses.condition;
        let block_pass = if_elses.block;
        
        let block_fail = if !if_elses.else_ifs.is_empty() {
            let first_else_if = if_elses.else_ifs.remove(0);

            let deconstructed_if_elses: IfElse<E> = IfElses {
                condition: first_else_if.0, 
                block: first_else_if.1, 
                else_ifs: if_elses.else_ifs, 
                else_: if_elses.else_, 
            }.into();

            Block {
                statements: Vec::new(), 
                expression: deconstructed_if_elses.into(), 
            }
        } else {
            if_elses.else_
        };

        IfElse {
            condition, 
            block_pass, 
            block_fail, 
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Variable(Variable), 
    Number(Number), 
    Char(Char), 
    /*String(RawString), */
    Operation(Box<Operation<Self>>), 
    Tuple(Box<Tuple<Self>>), 
    TupleAccess(Box<TupleAccess<Self>>), 
    IfElses(Box<IfElses<Self>>), 
    Block(Box<Block<Self>>), 
    Call(Box<Call<Self>>), 
    Match(Box<Match<Self>>), 
    Switch(Box<Switch<Self>>), 
    Struct(Box<StructExpr<Self>>), 
    ElementAccess(ElementAccess), 
    Enum(Box<EnumExpr<Self>>), 
    Lambda(Box<Lambda<Self>>), 
}

impl<SCtx: Clone, GCtx> VarMap<SCtx, GCtx> for Expression {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        match self {
            Expression::Variable(var) => {
                let (var, ctx, gctx) = f(var, VarUsage::Consume, ctx, gctx);
                (var.into(), ctx, gctx)
            }, 
            Expression::Number(n) => (n.into(), ctx, gctx), 
            Expression::Char(ch) => (ch.into(), ctx, gctx), 
            Expression::Operation(op) => {
                let (op, ctx, gctx) = op.map_vars(f, ctx, gctx);
                (op.into(), ctx, gctx)
            }, 
            Expression::Tuple(t) => {
                let (t, ctx, gctx) = t.map_vars(f, ctx, gctx);
                (t.into(), ctx, gctx)
            }, 
            Expression::TupleAccess(ta) => {
                let (ta, ctx, gctx) = ta.map_vars(f, ctx, gctx);
                (ta.into(), ctx, gctx)
            }, 
            Expression::IfElses(ie) => {
                let (ie, ctx, gctx) = ie.map_vars(f, ctx, gctx);
                (ie.into(), ctx, gctx)
            }, 
            Expression::Block(block) => {
                let (block, ctx, gctx) = block.map_vars(f, ctx, gctx);
                (block.into(), ctx, gctx)
            }, 
            Expression::Call(call) => {
                let (call, ctx, gctx) = call.map_vars(f, ctx, gctx);
                (call.into(), ctx, gctx)
            }, 
            Expression::Match(match_) => {
                let (match_, ctx, gctx) = match_.map_vars(f, ctx, gctx);
                (match_.into(), ctx, gctx)
            }, 
            Expression::Switch(switch) => {
                let (switch, ctx, gctx) = switch.map_vars(f, ctx, gctx);
                (switch.into(), ctx, gctx)
            }, 
            Expression::Lambda(lambda) => {
                let (lambda, ctx, gctx) = lambda.map_vars(f, ctx, gctx);
                (Expression::Lambda(lambda.into()), ctx, gctx)
            }, 
            Expression::Struct(struct_) => {
                let (struct_, ctx, gctx) = struct_.map_vars(f, ctx, gctx);
                (Expression::Struct(struct_.into()), ctx, gctx)
            }, 
            Expression::ElementAccess(element_access) => {
                let (element_access, ctx, gctx) = element_access.map_vars(f, ctx, gctx);
                (Expression::ElementAccess(element_access.into()), ctx, gctx)
            }, 
            Expression::Enum(enum_) => {
                let (enum_, ctx, gctx) = enum_.map_vars(f, ctx, gctx);
                (Expression::Enum(enum_.into()), ctx, gctx)
            }, 
        }
    }
}

impl From<Variable> for Expression {
    fn from(v: Variable) -> Self {
        Expression::Variable(v)
    }
}

impl From<Number> for Expression {
    fn from(n: Number) -> Self {
        Expression::Number(n)
    }
}

impl From<Char> for Expression {
    fn from(c: Char) -> Self {
        Expression::Char(c)
    }
}

impl From<Operation<Expression>> for Expression {
    fn from(o: Operation<Expression>) -> Self {
        Expression::Operation(Box::new(o))
    }
}

impl From<Tuple<Expression>> for Expression {
    fn from(t: Tuple<Expression>) -> Self {
        Expression::Tuple(Box::new(t))
    }
}

impl From<TupleAccess<Expression>> for Expression {
    fn from(ta: TupleAccess<Expression>) -> Self {
        Expression::TupleAccess(Box::new(ta))
    }
}

impl From<IfElses<Expression>> for Expression {
    fn from(ie: IfElses<Expression>) -> Self {
        Expression::IfElses(Box::new(ie))
    }
}

impl From<IfElse<Expression>> for Expression {
    fn from(ie: IfElse<Expression>) -> Self {
        let if_elses: IfElses<Expression> = ie.into();
        if_elses.into()
    }
}

impl From<Block<Expression>> for Expression {
    fn from(b: Block<Expression>) -> Self {
        Expression::Block(Box::new(b))
    }
}

impl From<Call<Expression>> for Expression {
    fn from(c: Call<Expression>) -> Self {
        Expression::Call(Box::new(c))
    }
}

impl From<Match<Expression>> for Expression {
    fn from(m: Match<Expression>) -> Self {
        Expression::Match(Box::new(m))
    }
}

impl From<Switch<Expression>> for Expression {
    fn from(s: Switch<Expression>) -> Self {
        Expression::Switch(Box::new(s))
    }
}

impl From<StructExpr<Expression>> for Expression {
    fn from(s: StructExpr<Expression>) -> Self {
        Expression::Struct(Box::new(s))
    }
}

fn propagating_parse<'a, T>(s: TextCursor<'a>, f: impl for<'b> Fn(TextCursor<'b>) -> ParseResult<'b, T>, r: ParseResult<'a, T>) -> ParseResult<'a, T> {
    match r {
        Ok(ok) => Ok(ok), 
        Err(old_err) => {
            match f(s) {
                Ok(ok) => Ok(ok), 
                Err(new_err) => {
                    if new_err.index > old_err.index {
                        Err(new_err)
                    } else {
                        Err(old_err)
                    }
                }
            }
        }
    }
}

impl Parse for Expression {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        //println!("FUCK!!!!");
        let x = Char::parse(s.clone()).map(|(x, s)| (x.into(), s));
        let x = propagating_parse(s.clone(), |s| Variable::parse(s).map(|(x, s)| (x.into(), s)), x);
        
        
        
        let x = propagating_parse(s.clone(), |s| Number::parse(s).map(|(x, s)| (x.into(), s)), x);

        let x = propagating_parse(s.clone(), |s| Operation::parse(s).map(|(x, s)| (x.into(), s)), x);
        
        let x = propagating_parse(s.clone(), |s| Tuple::parse(s).map(|(x, s)| (x.into(), s)), x);
        let x = propagating_parse(s.clone(), |s| IfElses::parse(s).map(|(x, s)| (x.into(), s)), x);
        
        let x = propagating_parse(s.clone(), |s| Block::parse(s).map(|(x, s)| (x.into(), s)), x);
        
        let x = propagating_parse(s.clone(), |s| Call::parse(s).map(|(x, s)| (x.into(), s)), x);
        println!("FUCK!!!!");
        
        let x = propagating_parse(s.clone(), |s| Match::parse(s).map(|(x, s)| (x.into(), s)), x);
        let x = propagating_parse(s.clone(), |s| Switch::parse(s).map(|(x, s)| (x.into(), s)), x);
        let x = propagating_parse(s.clone(), |s| TupleAccess::parse(s).map(|(x, s)| (x.into(), s)), x);
        
        

        x
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Variable {
    pub name: Alphanumeric, 
    pub annotation: Option<Annotation>, 
}

impl Ord for Variable {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl PartialOrd for Variable {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Annotation {
    Struct(Name), 
    Enum((Name, Name)), 
}

impl Parse for Annotation {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let s = parse_specific_token(s, &Token::DoubleColon).map_err(|e| ParseError::new(e.index, "Expected double colon".into()))?;
        let (name1, s) = Name::parse(s)?;

        if let Ok((name2, s)) = parse_specific_token(s.clone(), &Token::DoubleColon).and_then(|s| Name::parse(s)) {
            let ann = Annotation::Enum( (name1, name2) );
            Ok((ann, s))
        } else {
            let ann = Annotation::Struct(name1);
            Ok((ann, s))
        }
    }
}

impl From<String> for Variable {
    fn from(s: String) -> Self {
        Variable {
            name: Alphanumeric {
                s: s.into(), 
            }, 
            annotation: None, 
        }
    }
}

impl From<Name> for Variable {
    fn from(name: Name) -> Self {
        Variable {
            name: Alphanumeric {
                s: name.s, 
            }, 
            annotation: None, 
        }
    }
}

impl Parse for Variable {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (name, s) = Name::parse(s).map(|(name, s)| (name.into_string().into(), s))?;
        let (annotation, s) = parse_option(s, Annotation::parse);

        let var = Variable {
            name, 
            annotation, 
        };

        Ok((var, s))
    }
}

#[derive(Debug, Clone)]
pub struct Char {
    pub ch: String, 
}

impl Parse for Char {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (tok, s) = parse_token(s)?;
        match tok {
            Token::Char(ch) => {
                let a = Self {
                    ch: ch.clone(), 
                };

                Ok((a, s))
            }, 
            _ => Err(ParseError::from_cursor(s, "Expected char".into())), 
        }
    }
}

/// Unstructured string
#[derive(Debug, Clone)]
pub struct RawString {
    pub s: String, 
}

impl Parse for RawString {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (tok, s) = parse_token(s)?;
        match tok {
            Token::String(string) => {
                let a = Self {
                    s: string.clone(), 
                };

                Ok((a, s))
            }, 
            _ => Err(ParseError::from_cursor(s, "Expected string".into())), 
        }
    }
}

#[derive(Debug, Clone)]
pub struct Number {
    pub num: Numeric, 
    pub native_num: NativeNumber, 
}

impl Parse for Number {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (num, s) = Numeric::parse(s)?;
        let native_num = NativeNumber::try_from(num.s.as_str()).map_err(|e| ParseError::new(s.index, "Unable to parse number".into()))?;

        let number = Number {
            num, 
            native_num, 
        };

        Ok((number, s))
    }
}

#[derive(Debug, Clone)]
pub struct Numeric {
    pub s: String, 
}

impl Parse for Numeric {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (tok, s) = parse_token(s)?;
        match tok {
            Token::Alphanumeric(text) => {
                let ch = text.chars().next().ok_or(ParseError::from_cursor(s.clone(), "Empty string".into()))?;
                if ch.is_numeric() || ch == '+' || ch == '-' {
                    let a = Self {
                        s: text.clone()
                    };

                    Ok((a, s))
                } else {
                    Err(ParseError::from_cursor(s, "Expected alphanumeric".into()))
                }
            }, 
            _ => Err(ParseError::from_cursor(s, "Expected alphanumeric".into())), 
        }
    }
}

#[derive(Debug, Clone)]
pub enum NativeNumber {
    U24(u32), 
    I24(i32), 
    F24(f32), 
}

impl NativeNumber {
    pub fn to_string(&self) -> String {
        match self {
            NativeNumber::U24(x) => x.to_string(), 
            NativeNumber::I24(x) => x.to_string(), 
            NativeNumber::F24(x) => x.to_string(), 
        }
    }
}

impl NativeNumber {
    pub fn try_from(s: &str) -> Result<Self, ()> {
        if s.ends_with("u24") {
            Ok(NativeNumber::U24( parse_u24(s.split_once("u24").ok_or(())?.0 )?))
        } else if s.ends_with("i24") {
            Ok(NativeNumber::I24( parse_i24(s.split_once("i24").ok_or(())?.0 )?))
        } else if s.ends_with("f24") {
            Ok(NativeNumber::F24( s.split_once("f24").ok_or(())?.0.parse::<f32>().map_err(|_| ())? ))
        } else {
            if s.contains('.') {
                Ok(NativeNumber::F24( s.parse::<f32>().map_err(|_| ())? ))
            } else {
                if let Ok(u24) = parse_u24(s) {
                    Ok(NativeNumber::U24( u24 ))
                } else if let Ok(i24) = parse_i24(s) {
                    Ok(NativeNumber::I24( i24 ))
                } else {
                    Err(())
                }
            }
        }
    }
}

pub fn parse_u24(s: &str) -> Result<u32, ()> {
    let val = s.parse::<u32>().map_err(|_| ())?;

    const max_u24: u32 = 1 << 24 - 1;
    if val <= max_u24 {
        Ok(val)
    } else {
        Err(())
    }
}

pub fn parse_i24(s: &str) -> Result<i32, ()> {
    let val = s.parse::<i32>().map_err(|_| ())?;

    const max_i24: i32 = 1 << 23 - 1;
    const min_i24: i32 = -max_i24;
    if val >= min_i24 && val <= max_i24 {
        Ok(val)
    } else {
        Err(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Name {
    pub s: String, 
}

impl Name {
    pub fn into_string(self) -> String {
        self.s
    }

    pub fn as_str(&self) -> &str {
        self.s.as_str()
    }
}

impl From<String> for Name {
    fn from(s: String) -> Self {
        Self {
            s, 
        }
    }
}

impl Parse for Name {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (tok, s) = parse_token(s)?;
        match tok {
            Token::Alphanumeric(text) => {
                if text.chars().next().ok_or(ParseError::from_cursor(s.clone(), "Empty string".into()))?.is_alphabetic() {
                    let a = Self {
                        s: text.clone()
                    };
    
                    Ok((a, s))
                } else {
                    Err(ParseError::from_cursor(s, "Expected alphanumeric".into()))
                }
            }, 
            _ => Err(ParseError::from_cursor(s, "Expected alphanumeric".into())), 
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block<E> {
    pub statements: Vec<SetVar<E>>, 
    pub expression: E, 
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for Block<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let mut statements = Vec::with_capacity(self.statements.len());

        let orig_ctx = ctx.clone();

        let (ctx, gctx, statements) = self.statements.into_iter().fold((ctx, gctx, statements), |(ctx, gctx, mut statements), statement| {
            let (statement, ctx, gctx) = statement.map_vars(f.clone(), ctx, gctx);
            statements.push(statement);
            (ctx, gctx, statements)
        });

        let (expression, _, gctx) = self.expression.map_vars(f.clone(), ctx, gctx);

        (
            Self {
                statements, 
                expression, 
            }, 
            orig_ctx, 
            gctx, 
        )
    }
}

impl<E> From<E> for Block<E> {
    fn from(expr: E) -> Self {
        Self {
            statements: Vec::new(), 
            expression: expr, 
        }
    }
}

impl Block<Expression> {
    pub fn try_from(expr: Expression) -> Result<Self, ()> {
        match expr {
            Expression::Block(block) => Ok(Box::into_inner(block)), 
            _ => Err(()), 
        }
    }
}

impl Block<CoreExpression> {
    pub fn try_from_core(expr: CoreExpression) -> Result<Self, ()> {
        match expr {
            CoreExpression::Block(block) => Ok(Box::into_inner(block)), 
            _ => Err(()), 
        }
    }
}

impl<E: Parse> Parse for Block<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let s = parse_specific_token(s, &Token::LeftBrace).map_err(|e| ParseError::new(e.index, "Expected left brace".into()))?;
        let (statements, s) = parse_vec(s, SetVar::parse);
        let (expression, s) = E::parse(s)?;
        //println!("HERE??????? {:?}", expression);
        let s = parse_specific_token(s, &Token::RightBrace).map_err(|e| ParseError::new(e.index, "Expected right brace".into()))?;
        println!("HERE???????");

        let block = Block {
            statements, 
            expression, 
        };

        Ok((block, s))
    }
}

#[derive(Debug, Clone)]
pub struct Tuple<E> {
    pub elements: Vec<E>, 
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for Tuple<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let mut elements = Vec::with_capacity(self.elements.len());
        let (elements, gctx) = self.elements.into_iter().fold((elements, gctx), |(mut elements, gctx), element| {
            let (element, _, gctx) = element.map_vars(f.clone(), ctx.clone(), gctx);
            elements.push(element);
            (elements, gctx)
        });

        (
            Self {
                elements, 
            }, 
            ctx, 
            gctx, 
        )
    }
}

impl<E: Parse> Parse for Tuple<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let s = parse_specific_token(s, &Token::LeftParenthesis).map_err(|e| ParseError::new(e.index, "Expected left parenthesis".into()))?;
        let (elements, s) = parse_vec(s, E::parse);
        let s = parse_specific_token(s, &Token::RightParenthesis).map_err(|e| ParseError::new(e.index, "Expected right parenthesis".into()))?;

        let tuple = Tuple {
            elements, 
        };

        Ok((tuple, s))
    }
}

#[derive(Debug, Clone)]
pub struct Operation<E> {
    pub expression_left: E, 
    pub expression_right: E, 
    pub operator: Operator, 
}

impl<E: Parse> Parse for Operation<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (expression_left, s) = E::parse(s)?;
        
        let (operator, s) = if let (Token::Operator(op), s) = parse_token(s.clone())? {
            (op.clone(), s)
        } else {
            return Err(ParseError::from_cursor(s, "Expected operator".into()));
        };

        let (expression_right, s) = E::parse(s)?;

        let operation = Operation {
            expression_left, 
            expression_right, 
            operator, 
        };

        Ok((operation, s)) 
    }
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for Operation<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let (expression_left, _, gctx) = self.expression_left.map_vars(f.clone(), ctx.clone(), gctx);
        let (expression_right, _, gctx) = self.expression_right.map_vars(f.clone(), ctx.clone(), gctx);
        let operator = self.operator;

        (
            Self {
                expression_left, 
                expression_right, 
                operator, 
            }, 
            ctx, 
            gctx, 
        )
    }
}



#[derive(Debug, Clone)]
pub struct Lambda<E> {
    pub parameters: Vec<Variable>, 
    pub block: Block<E>, 
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for Lambda<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let mut parameters = Vec::with_capacity(self.parameters.len());
        let (parameters, ctx_inner, gctx) = self.parameters.into_iter().fold((parameters, ctx.clone(), gctx), |(mut parameters, ctx, gctx), param| {
            let (param_name, ctx, gctx) = f(param, VarUsage::Introduce, ctx, gctx);
            parameters.push(param_name);
            (parameters, ctx, gctx)
        });

        let (block, _, gctx) = self.block.map_vars(f, ctx_inner, gctx);

        (
            Lambda { 
                parameters, 
                block, 
            }, 
            ctx, 
            gctx, 
        )
    }
}

impl<E: Parse> Parse for Lambda<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let s = parse_specific_token(s, &Token::Pipe).map_err(|e| ParseError::new(e.index, "Expected pipe".into()))?;
        let (parameters, s) = parse_comma_delimited(s, Variable::parse);
        let s = parse_specific_token(s, &Token::Pipe).map_err(|e| ParseError::new(e.index, "Expected pipe".into()))?;
        let (block, s) = Block::parse(s)?;

        let lambda = Lambda {
            parameters, 
            block, 
        };

        Ok((lambda, s))
    }
}


#[derive(Debug, Clone)]
pub struct ElementAccess {
    pub var: Variable, 
    pub field: Name, 
}

impl<SCtx: Clone, GCtx> VarMap<SCtx, GCtx> for ElementAccess {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let (var, ctx, gctx) = f(self.var, VarUsage::Consume, ctx, gctx);
        let field = self.field;

        (
            Self {
                var, 
                field, 
            }, 
            ctx, 
            gctx, 
        )
    }
}

impl Parse for ElementAccess {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (var, s) = Variable::parse(s)?;
        let s = parse_specific_token(s, &Token::Period).map_err(|e| ParseError::new(e.index, "Expected period".into()))?;
        let (field, s) = Name::parse(s)?;

        let element_access = ElementAccess {
            var, 
            field, 
        };

        Ok((element_access, s))
    }
}



#[derive(Debug, Clone)]
pub struct StructExpr<E> {
    pub name: Name, 
    pub elements: Vec<(Name, E)>, 
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for StructExpr<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let mut elements = Vec::with_capacity(self.elements.len());
        let (elements, gctx) = self.elements.into_iter().fold((elements, gctx), |(mut elements, gctx), (name, expr)| {
            let (expr, _, gctx) = expr.map_vars(f.clone(), ctx.clone(), gctx);

            elements.push( (name, expr) );
            (elements, gctx)
        });

        let name = self.name;

        (
            Self {
                name, 
                elements, 
            }, 
            ctx, 
            gctx, 
        )
    }
}

impl<E: Parse> Parse for StructExpr<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (name, s) = Name::parse(s)?;
        let s = parse_specific_token(s, &Token::LeftBrace).map_err(|e| ParseError::new(e.index, "Expected left brace".into()))?;
        
        let (elements, s) = parse_comma_delimited(s, Self::parse_element);

        let s = parse_specific_token(s, &Token::RightBrace).map_err(|e| ParseError::new(e.index, "Expected right brace".into()))?;

        let struct_ = StructExpr {
            name, 
            elements, 
        };

        Ok((struct_, s))
    }
}

impl<E: Parse> StructExpr<E> {
    fn parse_element<'a>(s: TextCursor<'a>) -> ParseResult<'a, (Name, E)> {
        let (name, s) = Name::parse(s)?;
        let s = parse_specific_token(s, &Token::Colon).map_err(|e| ParseError::new(e.index, "Expected colon".into()))?;
        let (element, s) = E::parse(s)?;
        Ok( ((name, element), s) )
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Name, 
    pub elements: Vec<Name>, 
}

impl Struct {
    pub fn get_field_index(&self, field: &str) -> Option<usize> {
        self.elements
            .iter()
            .enumerate()
            .find(|(_, elem)| elem.as_str() == field)
            .map(|(i, _)| i)
    }
}

impl Parse for Struct {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let s = parse_specific_token(s, &Token::Struct).map_err(|e| ParseError::new(e.index, "Expected struct".into()))?;
        let (name, s) = Name::parse(s)?;
        let s = parse_specific_token(s, &Token::LeftBrace).map_err(|e| ParseError::new(e.index, "Expected left brace".into()))?;
        
        let (elements, s) = parse_comma_delimited(s, Name::parse);

        let s = parse_specific_token(s, &Token::RightBrace).map_err(|e| ParseError::new(e.index, "Expected right brace".into()))?;

        let struct_ = Struct {
            name, 
            elements, 
        };

        Ok((struct_, s))
    }
}

#[derive(Debug, Clone)]
pub struct EnumExpr<E> {
    pub name: Name, 
    pub struct_: StructExpr<E>, 
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for EnumExpr<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let (struct_, _, gctx) = self.struct_.map_vars(f, ctx.clone(), gctx);
        let name = self.name;

        (
            Self {
                name, 
                struct_,  
            }, 
            ctx, 
            gctx, 
        )
    }
}

impl<E: Parse> Parse for EnumExpr<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (name, s) = Name::parse(s)?;
        let s = parse_specific_token(s, &Token::DoubleColon).map_err(|e| ParseError::new(e.index, "Expected double colon".into()))?;
        let (struct_, s) = StructExpr::parse(s)?;

        let enum_ = EnumExpr {
            name, 
            struct_, 
        };

        Ok((enum_, s))
    }
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: Name, 
    pub elements: Vec<(Name, Vec<Name>)>, 
}

impl Parse for Enum {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let s = parse_specific_token(s, &Token::Enum).map_err(|e| ParseError::new(e.index, "Expected enum".into()))?;
        let (name, s) = Name::parse(s)?;
        let s = parse_specific_token(s, &Token::LeftBrace).map_err(|e| ParseError::new(e.index, "Expected left brace".into()))?;

        let (elements, s) = parse_comma_delimited(s, Self::parse_element);

        let s = parse_specific_token(s, &Token::RightBrace).map_err(|e| ParseError::new(e.index, "Expected right brace".into()))?;

        let enum_ = Enum {
            name, 
            elements, 
        };

        Ok((enum_, s))
    }
}

impl Enum {
    pub fn num_variant_elements(&self, variant: &str) -> Option<usize> {
        self.elements.iter().find(|(name, _)| name.as_str() == variant).map(|(_, elems)| elems.len())
    }

    pub fn get_variant_field_index(&self, variant: &str, field: &str) -> Option<usize> {
        self.elements.iter()
            .find(|(variant_name, _)| variant_name.as_str() == variant)
            .and_then(|(_, elems)| 
                elems
                    .iter()
                    .enumerate()
                    .find(|(_, field_name)| field_name.as_str() == variant)
                    .map(|(i, _)| i)
        )
    }

    fn parse_element<'a>(s: TextCursor<'a>) -> ParseResult<'a, (Name, Vec<Name>)> {
        let (sub_type, s) = Name::parse(s)?;
        let s = parse_specific_token(s, &Token::LeftBrace).map_err(|e| ParseError::new(e.index, "Expected left brace".into()))?;
        let (elements, s) = parse_comma_delimited(s, Name::parse);
        let s = parse_specific_token(s, &Token::RightBrace).map_err(|e| ParseError::new(e.index, "Expected right brace".into()))?;

        Ok( ((sub_type, elements), s) )
    }
}

#[derive(Debug, Clone)]
pub struct IfElses<E> {
    pub condition: E, 
    pub block: Block<E>, 
    pub else_ifs: Vec<(E, Block<E>)>, 
    pub else_: Block<E>, 
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for IfElses<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let (condition, _, gctx) = self.condition.map_vars(f.clone(), ctx.clone(), gctx);
        let (block, _, gctx) = self.block.map_vars(f.clone(), ctx.clone(), gctx);

        let mut else_ifs = Vec::with_capacity(self.else_ifs.len());
        let (else_ifs, gctx) = self.else_ifs.into_iter().fold((else_ifs, gctx), |(mut else_ifs, gctx), (cond, block)| {
            let (cond, ctx, gctx) = cond.map_vars(f.clone(), ctx.clone(), gctx);
            let (block, _, gctx) = block.map_vars(f.clone(), ctx, gctx);

            else_ifs.push( (cond, block) );
            (else_ifs, gctx)
        });

        let (else_, _, gctx) = self.else_.map_vars(f, ctx.clone(), gctx);

        (
            Self {
                condition, 
                block, 
                else_ifs, 
                else_, 
            }, 
            ctx, 
            gctx, 
        )
    }
}

impl<E> From<IfElse<E>> for IfElses<E> {
    fn from(ie: IfElse<E>) -> Self {
        Self {
            condition: ie.condition, 
            block: ie.block_pass, 
            else_ifs: Vec::new(), 
            else_: ie.block_fail, 
        }
    }
}

impl<E: Parse> Parse for IfElses<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let s = parse_specific_token(s, &Token::If).map_err(|e| ParseError::new(e.index, "Expected if".into()))?;
        let (condition, s) = E::parse(s)?;
        let (block, s) = Block::parse(s)?;

        let (else_ifs, s) = parse_vec(s, Self::parse_else_if);

        let s = parse_specific_token(s, &Token::Else).map_err(|e| ParseError::new(e.index, "Expected else".into()))?;
        let (else_, s) = Block::parse(s)?;

        let if_ = IfElses {
            condition, 
            block, 
            else_ifs, 
            else_, 
        };

        Ok((if_, s))
    }
}

impl<E: Parse> IfElses<E> {
    //type Error = ();

    fn parse_else_if<'a>(s: TextCursor<'a>) -> ParseResult<'a, (E, Block<E>)> {
        let s = parse_specific_token(s, &Token::If).map_err(|e| ParseError::new(e.index, "Expected if".into()))?;
        let s = parse_specific_token(s, &Token::Else).map_err(|e| ParseError::new(e.index, "Expected else".into()))?;
        let (condition, s) = E::parse(s)?;
        let (block, s) = Block::parse(s)?;

        Ok( ((condition, block), s) )
    }
}

#[derive(Debug, Clone)]
pub struct Call<E> {
    pub expression: E, 
    pub arguments: Vec<E>, 
}

/*impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for Call<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let (expression, _, gctx) = self.expression.map_vars(f.clone(), ctx.clone(), gctx);

        let mut arguments = Vec::with_capacity(self.arguments.len());
        let (arguments, gctx) = self.arguments.into_iter().fold((arguments, gctx), |(mut arguments, gctx), arg| {
            let (arg, _, gctx) = arg.map_vars(f.clone(), ctx.clone(), gctx);
            arguments.push(arg);
            (arguments, gctx)
        });
        
        (
            Self {
                expression, 
                arguments, 
            }, 
            ctx, 
            gctx
        )
    }
}*/

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for Call<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let (expression, _, gctx) = self.expression.map_vars(f.clone(), ctx.clone(), gctx);

        let mut arguments = Vec::with_capacity(self.arguments.len());
        let (arguments, gctx) = self.arguments.into_iter().fold((arguments, gctx), |(mut arguments, gctx), arg| {
            let (arg, _, gctx) = arg.map_vars(f.clone(), ctx.clone(), gctx);
            arguments.push(arg);
            (arguments, gctx)
        });
        
        (
            Self {
                expression, 
                arguments, 
            }, 
            ctx, 
            gctx
        )
    }
}

/*trait VarMap {
    type SCtx;
    type GCtx; 

    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, mctx: GCtx) -> Self;
}*/

impl<E: Parse> Parse for Call<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (expression, s) = E::parse(s)?;
        let s = parse_specific_token(s, &Token::LeftParenthesis).map_err(|e| ParseError::new(e.index, "Expected left parenthesis".into()))?;
        let (arguments, s) = parse_comma_delimited(s, |s| E::parse(s));
        let s = parse_specific_token(s, &Token::RightParenthesis).map_err(|e| ParseError::new(e.index, "Expected right parenthesis".into()))?;

        let call = Call {
            expression, 
            arguments, 
        };

        Ok((call, s))
    }
}

#[derive(Debug, Clone)]
pub struct SetVar<E> {
    pub var: Variable, 
    pub value: E, 
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for SetVar<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let (value, _, gctx) = self.value.map_vars(f.clone(), ctx.clone(), gctx);
        let (var, ctx, gctx) = f(self.var, VarUsage::Introduce, ctx.clone(), gctx);

        (
            Self {
                var, 
                value,
            }, 
            ctx, 
            gctx
        )
    }
}

impl<E: Parse> Parse for SetVar<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (var, s) = Variable::parse(s)?;
        let s = parse_specific_token(s, &Token::Equal).map_err(|e| ParseError::new(e.index, "Expected =".into()))?;
        let (value, s) = E::parse(s)?;
        let s = parse_specific_token(s, &Token::Semicolon).map_err(|e| ParseError::new(e.index, "Expected semicolon".into()))?;

        let setvar = SetVar {
            var, 
            value, 
        };

        Ok((setvar, s))
    }
}

#[derive(Debug, Clone)]
pub struct TupleAccess<E> {
    pub expression: E, 
    pub index: usize, 
    pub len: usize, 
}

impl<E: Parse> Parse for TupleAccess<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (expression, s) = E::parse(s)?;
        let s = parse_specific_token(s, &Token::LeftBracket).map_err(|e| ParseError::new(e.index, "Expected left bracket".into()))?;
        let (index, s) = usize::parse(s)?;
        let s = parse_specific_token(s, &Token::Colon).map_err(|e| ParseError::new(e.index, "Expected colon".into()))?;
        let (len, s) = usize::parse(s)?;
        let s = parse_specific_token(s, &Token::RightBracket).map_err(|e| ParseError::new(e.index, "Expected right bracket".into()))?;

        let aa = TupleAccess {
            expression, 
            index, 
            len, 
        };

        Ok((aa, s))
    }
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for TupleAccess<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let (expression, _, gctx) = self.expression.map_vars(f.clone(), ctx.clone(), gctx);

        let index = self.index;
        let len = self.len;

        (
            Self {
                expression, 
                index, 
                len, 
            }, 
            ctx, 
            gctx, 
        )
    }
}

impl Parse for usize {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (tok, s) = parse_token(s)?;
        match tok {
            Token::Alphanumeric(text) => {
                if text.chars().all(|ch| ch.is_ascii_digit()) {
                    let val = text.parse().map_err(|e| ParseError::new(s.index, "Unable to parse alphanumeric".into()))?;
                    Ok((val, s))
                } else {
                    Err(ParseError::from_cursor(s, "Unable to parse alphanumeric".into()))
                }
            }, 
            _ => Err(ParseError::from_cursor(s, "Expected alphanumeric".into())), 
        }
    }
}

#[derive(Debug, Clone)]
pub struct Match<E> {
    pub expression: E, 
    pub cases: Vec<Matchcase<E>>, 
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for Match<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let (expression, _, gctx) = self.expression.map_vars(f.clone(), ctx.clone(), gctx);

        let mut cases = Vec::with_capacity(self.cases.len());
        let (cases, gctx) = self.cases.into_iter().fold((cases, gctx), |(mut cases, gctx), case| {
            let (case, _, gctx) = case.map_vars(f.clone(), ctx.clone(), gctx);
            cases.push(case);
            (cases, gctx)
        });

        (
            Self {
                expression,  
                cases, 
            }, 
            ctx, 
            gctx, 
        )
    }
}

impl<E: Parse> Parse for Match<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let s = parse_specific_token(s, &Token::Match).map_err(|e| ParseError::new(e.index, "Expected match statement".into()))?;
        let (expression, s) = E::parse(s)?;

        let s = parse_specific_token(s, &Token::LeftBrace).map_err(|e| ParseError::new(e.index, "Expected left brace".into()))?;

        let (cases, s) = parse_comma_delimited(s, Matchcase::parse);

        let s = parse_specific_token(s, &Token::RightBrace).map_err(|e| ParseError::new(e.index, "Expected right brace".into()))?;

        let match_ = Match {
            expression,  
            cases, 
        };

        Ok((match_, s))
    }
}

#[derive(Debug, Clone)]
pub struct Matchcase<E> {
    pub var: Variable, 
    pub expression: E, 
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for Matchcase<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let (var, ctx_inner, gctx) = f(self.var, VarUsage::Introduce, ctx.clone(), gctx);
        let (expression, _, gctx) = self.expression.map_vars(f.clone(), ctx_inner, gctx);

        (
            Self {
                var, 
                expression, 
            }, 
            ctx, 
            gctx, 
        )
    }
}

impl<E: Parse> Parse for Matchcase<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let (var, s) = Variable::parse(s)?;
        let s = parse_specific_token(s, &Token::Arrow).map_err(|e| ParseError::new(e.index, "Expected arrow".into()))?;
        let (expression, s) = E::parse(s)?;

        let matchcase = Matchcase {
            var, 
            expression, 
        };

        Ok((matchcase, s))
    }
}

#[derive(Debug, Clone)]
pub struct Switch<E> {
    pub expression: E, 
    pub cases: Vec<(E, E)>, 
    pub default: (Variable, E), 
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for Switch<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let (expression, _, gctx) = self.expression.map_vars(f.clone(), ctx.clone(), gctx);

        let mut cases = Vec::with_capacity(self.cases.len());
        let (cases, gctx) = self.cases.into_iter().fold((cases, gctx), |(mut cases, gctx), (cond, expr)| {
            let (cond, ctx, gctx) = cond.map_vars(f.clone(), ctx.clone(), gctx);
            let (expr, _, gctx) = expr.map_vars(f.clone(), ctx, gctx);

            cases.push( (cond, expr) );
            (cases, gctx)
        });

        let (default_var, ctx_inner, gctx) = f(self.default.0, VarUsage::Introduce, ctx.clone(), gctx);
        let (default_expr, _, gctx) = self.default.1.map_vars(f.clone(), ctx_inner, gctx);
        let default = (default_var, default_expr);

        (
            Self {
                expression, 
                cases, 
                default, 
            }, 
            ctx, 
            gctx, 
        )
    }
}

impl<E: Parse> Parse for Switch<E> {
    //type Error = ();

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> {
        let s = parse_specific_token(s, &Token::Switch).map_err(|e| ParseError::new(e.index, "Expected switch statement".into()))?;
        let (expression, s) = E::parse(s)?;
        let s = parse_specific_token(s, &Token::LeftBrace).map_err(|e| ParseError::new(e.index, "Expected left brace".into()))?;

        let (cases, s) = parse_comma_delimited(s, Self::parse_case);

        let s = parse_specific_token(s, &Token::Comma).map_err(|e| ParseError::new(e.index, "Expected comma".into()))?;
        let (var_default, s) = Variable::parse(s)?;
        let s = parse_specific_token(s, &Token::Arrow).map_err(|e| ParseError::new(e.index, "Expected arrow".into()))?;
        let (expression_default, s) = E::parse(s)?;
        let default = (var_default, expression_default);
        
        let s = parse_specific_token(s, &Token::RightBrace).map_err(|e| ParseError::new(e.index, "Expected right brace".into()))?;

        let switch = Switch {
            expression, 
            cases, 
            default, 
        };

        Ok((switch, s))
    }
}

impl<E: Parse> Switch<E> {
    //type Error = ();

    fn parse_case<'a>(s: TextCursor<'a>) -> ParseResult<'a, (E, E)> {
        let (from_expression, s) = E::parse(s)?;
        let s = parse_specific_token(s, &Token::Arrow).map_err(|e| ParseError::new(e.index, "Expected arrow".into()))?;
        let (to_expression, s) = E::parse(s)?;

        Ok( ((from_expression, to_expression), s) )
    }
}

pub trait Parse {
    //type Error;

    fn parse<'a>(s: TextCursor<'a>) -> ParseResult<'a, Self> where Self: Sized;
}

fn parse_comma_delimited<'a, T>(s: TextCursor<'a>, f: impl for<'b> Fn(TextCursor<'b>) -> ParseResult<'b, T>) -> (Vec<T>, TextCursor<'a>) {
    let (t, s) = if let Ok((t, s)) = f(s.clone()) {
        (t, s)
    } else {
        return (Vec::new(), s);
    };

    let (ts, s) = parse_vec(s, |s| {
        let s = parse_specific_token(s, &Token::Comma)?;
        f(s)
    });

    let all = iter::once(t).chain(ts).collect();

    (all, s)
}

fn parse_nonempty_vec<'a, T>(s: TextCursor<'a>, f: impl for<'b> Fn(TextCursor<'b>) -> ParseResult<'b, T>) -> ParseResult<'a, Vec<T>> {
    let (first, mut s) = f(s)?;
    let mut ts = vec![first];

    while let Ok((t, _s)) = f(s.clone()) {
        s = _s;
        ts.push(t);
    }

    Ok((ts, s))
}

fn parse_vec<'a, T>(mut s: TextCursor<'a>, f: impl for<'b> Fn(TextCursor<'b>) -> ParseResult<'b, T>) -> (Vec<T>, TextCursor<'a>) {
    let mut ts = Vec::new();

    while let Ok((t, _s)) = f(s.clone()) {
        s = _s;
        ts.push(t);
    }

    (ts, s)
}

fn parse_token<'a>(mut s: TextCursor<'a>) -> ParseResult<'a, &'a Token> {
    if let Some(tok) = s.s.get(s.index) {
        s.index += 1;
        Ok((tok, s))
    } else {
        Err(ParseError::from_cursor(s, "Tried to parse past text end".into()))
    }
}

fn parse_specific_token<'a>(s: TextCursor<'a>, tok: &Token) -> Result<TextCursor<'a>, ParseError> {
    let (t, s) = parse_token(s)?;
    if t == tok {
        Ok(s)
    } else {
        Err(ParseError::from_cursor(s, "Unexpected token".into()))
    }
}

fn parse_option<'a, T>(s: TextCursor<'a>, f: impl for<'b> Fn(TextCursor<'b>) -> ParseResult<'b, T>) -> (Option<T>, TextCursor<'a>) {
    if let Ok((t, s)) = f(s.clone()) {
        (Some(t), s)
    } else {
        (None, s)
    }
}




type ParseResult<'a, T> = Result<(T, TextCursor<'a>), ParseError>;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub index: usize, 
    pub msg: String, 
}

impl ParseError {
    fn new(index: usize, msg: String) -> Self {
        Self {
            index, 
            msg, 
        }
    }

    fn from_cursor<'a>(s: TextCursor<'a>, msg: String) -> Self {
        Self {
            index: s.index, 
            msg: msg, 
        }
    }
}

#[derive(Debug, Clone)]
pub struct TextCursor<'a> {
    index: usize, 
    s: &'a [Token], 
}

impl<'a> TextCursor<'a> {
    pub fn new(s: &'a [Token]) -> Self {
        Self {
            index: 0, 
            s: s, 
        }
    }
}