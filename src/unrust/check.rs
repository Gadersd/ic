use crate::unrust::parse::*;

use std::collections::{HashMap, BTreeSet};

pub struct CheckedProgram<E> {
    program: Program<E>, 
}

impl CheckedProgram<Expression> {
    pub fn into_program(self) -> Program<Expression> {
        self.program
    }

    pub fn from_program(program: Program<Expression>) -> Result<Self, ProgramError> {
        check_structs( program.structs.iter().map(|(name, struct_)| (name.as_str(), struct_)) ).map_err(|e| ProgramError::Struct(e))?;
        check_enums( program.enums.iter().map(|(name, enum_)| (name.as_str(), enum_)) ).map_err(|e| ProgramError::Enum(e))?;

        // check that all functions have unique names
        all_unique(program.functions.iter().map(|(name, _)| name.as_str())).map_err(|e| ProgramError::FunctionNameCollision(e.into()))?;

        // check variables
        let check_vars = |var: Variable, usage, mut var_exists: BTreeSet<String>, mut undefined_vars: BTreeSet<String>| {
            match usage {
                VarUsage::Introduce => {
                    var_exists.insert(var.name.as_str().into());
                    (var, var_exists, undefined_vars)
                }, 
                VarUsage::Consume => {
                    if !var_exists.contains(var.name.as_str()) {
                        undefined_vars.insert(var.name.as_str().into());
                    }
                    (var, var_exists, undefined_vars)
                }, 
            }
        };

        let (program, _, undefined_vars) = program.map_vars(check_vars, BTreeSet::new(), BTreeSet::new());

        if !undefined_vars.is_empty() {
            return Err(
                ProgramError::Expression(
                    ExpressionError::Variable(
                        VariableError::UndefinedVars(undefined_vars)
                    )
                )
            );
        }

        // propagate struct annotations
        let annotate = |mut var: Variable, usage, mut annotations: HashMap<String, Option<Annotation>>| {
            match usage {
                VarUsage::Introduce => {
                    annotations.insert(var.name.as_str().into(), var.annotation.clone());
                    (var, annotations)
                }, 
                VarUsage::Consume => {
                    let annotation = annotations.get(var.name.as_str()).unwrap().clone();
                    var.annotation = annotation;
                    (var, annotations)
                }, 
            }
        };

        let (program, _, undefined_vars) = program.map_vars(
            |v, u, ctx, gctx| {
                let (v, ctx) = annotate(v, u, ctx);
                (v, ctx, gctx)
            }, 
            HashMap::new(), 
            ()
        );

        // check functions
        for (_, function) in &program.functions {
            check_function(function, &program)?;
        }

        Ok(CheckedProgram {
            program, 
        })
    }
}

fn check_function(function: &Function<Expression>, program: &Program<Expression>) -> Result<(), ProgramError> {
    all_unique(function.parameters.iter().map(|var| var.name.as_str())).map_err(|e| ProgramError::FunctionParameterConflict((function.name.as_str().into(), e.into())))?;
    check_block(&function.block, program).map_err(|e| ProgramError::Expression(e))
}

/*#[derive(Debug, Clone)]
pub struct Function<E> {
    pub name: Name, 
    pub parameters: Vec<Variable>, 
    pub block: Block<E>, 
}*/

#[derive(Debug, Clone)]
pub enum ProgramError {
    FunctionParameterConflict((String, String)), 
    FunctionNameCollision(String), 
    Struct(StructError), 
    Enum(EnumError), 
    Expression(ExpressionError), 
}

#[derive(Debug, Clone)]
pub enum ExpressionError {
    Variable(VariableError), 
    Number(NumberError), 
    TupleAccess(TupleAccessError), 
    Match(MatchError), 
    Struct(StructExprError), 
    ElementAccess(StructAccessError), 
    Enum(EnumExprError), 
    Lambda(LambdaError), 
}

#[derive(Debug, Clone)]
pub enum VariableError {
    UndefinedVars(BTreeSet<String>), 
    InvalidAnnotation, 
}


fn check_expression(expr: &Expression, program: &Program<Expression>) -> Result<(), ExpressionError> {
    match expr {
        Expression::Variable(v) => v.annotation.as_ref().map(|ann| if check_annotation(ann, program) {Ok(())} else {Err(ExpressionError::Variable(VariableError::InvalidAnnotation))}).unwrap_or(Ok(())), 
        Expression::Number(n) => check_number(n), 
        Expression::Char(_) => Ok(()), 
        /*Expression::String(RawString), */
        Expression::Operation(op) => check_operation(op, program), 
        Expression::Tuple(tuple) => check_tuple(tuple, program), 
        Expression::TupleAccess(tuple_access) => check_tuple_access(tuple_access, program), 
        Expression::IfElses(if_elses) => check_if_elses(if_elses, program), 
        Expression::Block(block) => check_block(block, program), 
        Expression::Call(call) => check_call(call, program), 
        Expression::Match(match_) => check_match(match_, program), 
        Expression::Switch(switch) => check_switch(switch, program), 
        Expression::Struct(struct_) => check_struct_expr(struct_, program), 
        Expression::ElementAccess(sa) => check_struct_access(sa, program), 
        Expression::Enum(enum_) => check_enum_expr(enum_, program), 
        Expression::Lambda(lambda) => check_lambda(lambda, program), 
    }
}

/*#[derive(Debug, Clone)]
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
}*/






fn check_number(n: &Number) -> Result<(), ExpressionError> {
    match n.native_num {
        NativeNumber::U24(x) => {
            const max: u32 = 1 << 24 - 1;
            if x >= 0 && x <= max {
                Ok(())
            } else {
                Err(ExpressionError::Number(NumberError::OutOfBounds(n.native_num.clone())))
            }
        }, 
        NativeNumber::I24(x) => {
            const max: i32 = 1 << 23 - 1;
            const min: i32 = -(1 << 23);
            if x >= min && x <= max {
                Ok(())
            } else {
                Err(ExpressionError::Number(NumberError::OutOfBounds(n.native_num.clone())))
            }
        }, 
        NativeNumber::F24(x) => Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum NumberError {
    OutOfBounds(NativeNumber), 
}

fn check_operation(op: &Operation<Expression>, program: &Program<Expression>) -> Result<(), ExpressionError> {
    check_expression(&op.expression_left, program)?;
    check_expression(&op.expression_right, program)
}

fn check_tuple(tuple: &Tuple<Expression>, program: &Program<Expression>) -> Result<(), ExpressionError> {
    tuple.elements.iter().try_for_each(|elem| check_expression(elem, program))
}

fn check_tuple_access(tuple_access: &TupleAccess<Expression>, program: &Program<Expression>) -> Result<(), ExpressionError> {
    if tuple_access.index >= tuple_access.len {
        return Err(ExpressionError::TupleAccess(TupleAccessError::IndexOutOfBounds((tuple_access.index, tuple_access.len))));
    }
    check_expression(&tuple_access.expression, program)
}

#[derive(Debug, Clone)]
pub enum TupleAccessError {
    IndexOutOfBounds((usize, usize)), 
}

fn check_if_elses(if_elses: &IfElses<Expression>, program: &Program<Expression>) -> Result<(), ExpressionError> {
    check_expression(&if_elses.condition, program)?;
    check_block(&if_elses.block, program)?;
    if_elses.else_ifs.iter().try_for_each(|(cond, block)| {
        check_expression(cond, program)?;
        check_block(block, program)
    })?;
    check_block(&if_elses.else_, program)
}

fn check_block(block: &Block<Expression>, program: &Program<Expression>) -> Result<(), ExpressionError> {
    block.statements.iter().try_for_each(|sv| check_set_var(sv, program))?;
    check_expression(&block.expression, program)
}

fn check_set_var(sv: &SetVar<Expression>, program: &Program<Expression>) -> Result<(), ExpressionError> {
    check_expression(&sv.value, program)
}

fn check_call(call: &Call<Expression>, program: &Program<Expression>) -> Result<(), ExpressionError> {
    check_expression(&call.expression, program)?;
    call.arguments.iter().try_for_each(|arg| check_expression(arg, program))
}

fn check_match(match_: &Match<Expression>, program: &Program<Expression>) -> Result<(), ExpressionError> {
    check_expression(&match_.expression, program)?;

    match_.cases.iter().try_for_each(|case| check_matchcase(case, program))?;

    // enum for each case must be the same
    if let Some(case1) = match_.cases.get(0) {
        let enum1 = get_enum_annotation(case1.var.annotation.as_ref().unwrap()).unwrap().0;
        for case in &match_.cases {
            let enum2 = get_enum_annotation(case.var.annotation.as_ref().unwrap()).unwrap().0;
            if enum1 != enum2 {
                return Err(ExpressionError::Match(MatchError::CaseConflict((enum1.into(), enum2.into()))));
            }
        }
    }

    // check for missing cases
    if let Some(case1) = match_.cases.get(0) {
        let enum_type = get_enum_annotation(case1.var.annotation.as_ref().unwrap()).unwrap().0;
        let enum_def = program.enums.get(enum_type).unwrap();
        if enum_def.elements.len() > match_.cases.len() {
            return Err(ExpressionError::Match(MatchError::MissingCases));
        }
    }

    Ok(())
}

fn get_enum_annotation(ann: &Annotation) -> Option<(&str, &str)> {
    match ann {
        Annotation::Struct(_) => None, 
        Annotation::Enum((ename, sname)) => Some( (ename.as_str(), sname.as_str()) ), 
    }
}

#[derive(Debug, Clone)]
pub enum MatchError {
    InvalidEnum(String), 
    MissingCases, 
    MissingAnnotation, 
    InvalidAnnotation(String), 
    CaseConflict((String, String)), 
    InvalidEnumVariant((String, String)), 
}

fn check_matchcase(case: &Matchcase<Expression>, program: &Program<Expression>) -> Result<(), ExpressionError> {
    let ann = case.var.annotation.as_ref();
    let (enum_name, variant_name) = match ann {
        Some(Annotation::Struct(sname)) => {return Err(ExpressionError::Match(MatchError::InvalidAnnotation(sname.clone().into_string())));}, 
        Some(Annotation::Enum((enum_name, variant_name))) => (enum_name, variant_name), 
        None => {return Err(ExpressionError::Match(MatchError::MissingAnnotation));}, 
    };

    let enum_def = program.enums.get(enum_name.as_str()).unwrap();

    if !enum_def.elements.iter().find(|(name, _)| name.as_str() == variant_name.as_str()).is_none() {
        return Err(ExpressionError::Match(MatchError::InvalidEnumVariant((enum_name.as_str().into(), variant_name.as_str().into()))));
    }

    check_expression(&case.expression, program)
}

fn check_switch(switch: &Switch<Expression>, program: &Program<Expression>) -> Result<(), ExpressionError> {
    check_expression(&switch.expression, program)?;
    switch.cases.iter().try_for_each(|(cond, expr)| {
        check_expression(cond, program)?;
        check_expression(expr, program)
    })?;
    check_expression(&switch.default.1, program)
}

fn check_struct_expr(struct_: &StructExpr<Expression>, program: &Program<Expression>) -> Result<(), ExpressionError> {
    if !program.structs.contains_key(struct_.name.as_str()) {
        return Err(ExpressionError::Struct(StructExprError::InvalidName(struct_.name.as_str().into())));
    }

    let struct_def = program.structs.get(struct_.name.as_str()).unwrap();
    if struct_def.elements.len() > struct_.elements.len() {
        return Err(ExpressionError::Struct(StructExprError::MissingElements));
    }

    struct_.elements.iter().try_for_each(|(name, expr)| {
        if struct_def.elements.iter().find(|variant| variant == &name).is_none() {
            return Err(ExpressionError::Struct(StructExprError::InvalidElement((struct_.name.as_str().into(), name.as_str().into()))));
        }

        check_expression(expr, program)
    })
}

#[derive(Debug, Clone)]
pub enum StructExprError {
    InvalidName(String), 
    MissingElements, 
    InvalidElement((String, String)), 
}

fn check_struct_access(element_access: &ElementAccess, program: &Program<Expression>) -> Result<(), ExpressionError> {
    if let Some(annotation) = element_access.var.annotation.as_ref() {
        if !check_annotation(annotation, program) {
            return Err(ExpressionError::ElementAccess(StructAccessError::InvalidStruct));
        }
    } else {
        return Err(ExpressionError::ElementAccess(StructAccessError::NoStructAnnotation));
    }

    if !program.structs.contains_key(element_access.field.as_str()) {
        return Err(ExpressionError::ElementAccess(StructAccessError::InvalidStructVariant));
    }

    Ok(())
}

fn check_annotation(ann: &Annotation, program: &Program<Expression>) -> bool {
    match ann {
        Annotation::Struct(sname) => program.structs.contains_key(sname.as_str()), 
        Annotation::Enum((ename, sname)) => program.enums.get(ename.as_str()).map(|enum_| enum_.elements.iter().find(|(n, _)| n == sname).is_some()).unwrap_or(false), 
    }
}

#[derive(Debug, Clone)]
pub enum StructAccessError {
    InvalidStruct, 
    InvalidStructVariant, 
    NoStructAnnotation, 
}

fn check_enum_expr(enum_expr: &EnumExpr<Expression>, program: &Program<Expression>) -> Result<(), ExpressionError> {
    if !program.enums.contains_key(enum_expr.name.as_str()) {
        return Err(ExpressionError::Enum(EnumExprError::InvalidEnum));
    }

    let enum_def = program.enums.get(enum_expr.name.as_str()).unwrap();
    if let Some((_, elems)) = enum_def.elements.iter().find(|(name, _)| name == &enum_expr.struct_.name) {
        if elems.len() > enum_expr.struct_.elements.len() {
            return Err(ExpressionError::Enum(EnumExprError::MissingVariantElements));
        }
        for (struct_variant, _) in &enum_expr.struct_.elements {
            if elems.iter().find(|ename| ename.as_str() == struct_variant.as_str()).is_none() {
                return Err(ExpressionError::Enum(EnumExprError::InvalidVariantElement));
            }
        }
    } else {
        return Err(ExpressionError::Enum(EnumExprError::InvalidVariant));
    }

    enum_expr.struct_.elements.iter().try_for_each(|(_, elem)| {
        check_expression(elem, program)
    })
}

#[derive(Debug, Clone)]
pub enum EnumExprError {
    InvalidEnum, 
    InvalidVariant, 
    MissingVariantElements, 
    InvalidVariantElement, 
}


fn check_lambda(lambda: &Lambda<Expression>, program: &Program<Expression>) -> Result<(), ExpressionError> {
    all_unique(lambda.parameters.iter().map(|var| var.name.as_str())).map_err(|_| ExpressionError::Lambda(LambdaError::ParameterCollision))?;
    check_block(&lambda.block, program)
}

#[derive(Debug, Clone)]
pub enum LambdaError {
    ParameterCollision, 
}


// All struct names must be unique as well as their elements
fn check_structs<'a>(mut structs: impl Iterator<Item=(&'a str, &'a Struct)> + Clone) -> Result<(), StructError> {
    all_unique(
        structs.clone().map(|(name, _)| name)
    ).map_err(|e| StructError::NameCollision(e.into()))
    .and_then( |_|
        structs.try_for_each(|(_, struct_)| check_struct_def(struct_))
    )
}

// all elements must be unique
fn check_struct_def(struct_: &Struct) -> Result<(), StructError> {
    all_unique(struct_.elements.iter().map(|name| name.as_str()))
        .map_err(|e| StructError::ElementNameCollision(e.into()))
}

#[derive(Debug, Clone)]
pub enum StructError {
    ElementNameCollision(String), 
    NameCollision(String), 
}

// All enum names must be unique as well as their elements
fn check_enums<'a>(mut enums: impl Iterator<Item=(&'a str, &'a Enum)> + Clone) -> Result<(), EnumError> {
    all_unique(
        enums.clone().map(|(name, _)| name)
    ).map_err(|e| EnumError::NameCollision(e.into())).and_then(|_|
        enums.try_for_each(|(_, enum_)| check_enum_def(enum_))
    )
}

fn check_enum_def(enum_: &Enum) -> Result<(), EnumError> {
    // all elements must be unique
    all_unique(
        enum_.elements.iter().map(|(name, _)| name.as_str())
    ).map_err(|e| EnumError::ElementNameCollision(e.into()))
}

#[derive(Debug, Clone)]
pub enum EnumError {
    ElementNameCollision(String), 
    NameCollision(String), 
}

fn all_unique<T: Eq + Clone>(xs: impl Iterator<Item=T> + Clone) -> Result<(), T> {
    for (i, x1) in xs.clone().enumerate() {
        for x2 in xs.clone().skip(i + 1) {
            if x1 == x2 {
                return Err(x1.clone());
            }
        }
    }

    return Ok(())
}
