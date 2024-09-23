use crate::unrust::{parse::*, core::*, check::*};
use crate::numeric::{self, Operator};

use std::collections::HashMap;
use std::iter;

#[derive(Debug)]
pub struct UniqueNamer {
    count: usize, 
}

impl UniqueNamer {
    pub fn get_unique_name(&mut self) -> String {
        self.count += 1;
        id_to_name(self.count)
    }
}

/// Invertible map from natural numbers to strings
pub fn id_to_name(val: usize) -> String {
    let mut digits = Vec::new(); // base 26
    let mut accum = val;

    loop {
        let digit = accum - (accum / 26) * 26;
        digits.push(digit);
        accum = accum / 26;

        if accum == 0 {
            break;
        }
    }

    digits.reverse();
    let first = digits.pop().unwrap();

    // digits past first must be shifted back by 1
    iter::once(first).chain(digits.into_iter().map(|d| if d == 0 {25} else {d - 1})).map(|d| ('a' as u8 + d as u8) as char).collect()
}

/// Program in which every variable has a globally unique name
/// Ensures compatibility with global scope
#[derive(Debug)]
pub struct GlobalProgram<E> {
    pub program: Program<E>, 
    pub namer: UniqueNamer, 
    pub main_rename: String, 
}

impl GlobalProgram<Expression> {
    pub fn from_program(program: CheckedProgram<Expression>) -> Self {
        let program = program.into_program();

        let var_renames: HashMap<String, String> = HashMap::new();
        let mut namer = UniqueNamer {
            count: 0, 
        };

        let make_var_globally_unqiue = |var: Variable, usage, mut var_renames: HashMap<String, String>, mut namer: UniqueNamer| {
            match usage {
                VarUsage::Introduce => {
                    let new_name = namer.get_unique_name();
                    var_renames.insert(var.name.clone().into_string(), new_name.clone());

                    let var = Variable {
                        name: new_name.into(), 
                        annotation: var.annotation, 
                    };
                    (var, var_renames, namer)
                }, 
                VarUsage::Consume => {
                    let var = Variable {
                        name: var_renames.get(var.name.as_str()).unwrap().clone().into(), 
                        annotation: var.annotation, 
                    };
                    (var, var_renames, namer)
                }, 
            }
        };

        let (program, global_renames, namer) = program.map_vars(make_var_globally_unqiue, var_renames, namer);

        let main_rename = global_renames.get("main").unwrap().clone();
        
        GlobalProgram {
            program, 
            namer, 
            main_rename, 
        }
    }

    pub fn to_core(mut self) -> GlobalProgram<CoreExpression> {
        let structs = self.program.structs;
        let enums = self.program.enums;
        let mut namer = self.namer;

        let funcs = self.program.functions.clone();
        let mut additional_functions = HashMap::new();

        let mut functions: HashMap<_, _> = self.program.functions.into_iter().map(|(_, func)| {
            let name = func.name;
            let parameters = func.parameters;
            let block = match CoreExpression::from_expression(func.block.into(), &mut namer, &mut additional_functions, &funcs, &structs, &enums) {CoreExpression::Block(block) => Box::into_inner(block), _ => panic!()};
            let func = Function {
                name, 
                parameters, 
                block, 
            };
            (func.name.clone().into_string(), func)
        }).collect();

        functions.extend(additional_functions.into_iter());

        GlobalProgram {
            program: Program {
                functions, 
                structs, 
                enums, 
            }, 
            namer: namer, 
            main_rename: self.main_rename, 
        }
    }
}

impl<E> GlobalProgram<E> {
    pub fn get_struct(&self, name: &str) -> Option<&Struct> {
        self.program.get_struct(name)
    }

    pub fn get_enum(&self, name: &str) -> Option<&Enum> {
        self.program.get_enum(name)
    }


    pub fn num_struct_elems(&self, name: &str) -> Option<usize> {
        self.program.structs.get(name).map(|s| s.elements.len())
    }

    pub fn get_enum_tag_id(&self, enum_type: &Name, enum_variant: &Name) -> Option<usize> {
        self.program.enums.get(enum_type.as_str())
            .and_then(|enum_| 
                enum_.elements.iter().enumerate()
                    .find(|(_, elem)| &elem.0 == enum_variant)
                    .map(|(i, _)| i)
                )
    }

    pub fn unique_var_name(&mut self) -> Name {
        self.namer.get_unique_name().into()
    }
}


pub fn match_to_if_elses_block(match_: Match<Expression>, namer: &mut UniqueNamer, enums: &HashMap<String, Enum>) -> Block<Expression> {
    let expr_name = namer.get_unique_name();
    let tag_name = namer.get_unique_name();
    let struct_name = namer.get_unique_name();

    let var_expr = Expression::Variable(expr_name.clone().into());

    let var_statements: Vec<SetVar<Expression>> = vec![
        SetVar{var: expr_name.into(), value: match_.expression}.into(), 
        SetVar{var: tag_name.clone().into(), value: TupleAccess{expression: var_expr.clone(), index: 0, len: 2}.into()}.into(), 
        SetVar{var: struct_name.clone().into(), value: TupleAccess{expression: var_expr, index: 1, len: 2}.into()}.into(), 
    ];

    let (enum_type, enum_variant) = match_.cases.get(0).map(|case| match case.var.annotation.as_ref().unwrap() {Annotation::Enum((name, variant)) => (name.clone(), variant.clone()), _ => panic!()}).unwrap_or( ("".to_string().into(), "".to_string().into()) );

    let mut cases: Vec<(Expression, Block<Expression>)> = match_.cases.into_iter().map(|case| {
        let tag_id = enums.get(enum_type.as_str())
            .and_then(|enum_| 
                enum_.elements.iter().enumerate()
                    .find(|(_, elem)| &elem.0 == &enum_variant)
                    .map(|(i, _)| i)
                ).unwrap();
        
        let condition = Operation {
            expression_left: Expression::Variable(tag_name.clone().into()), 
            expression_right: Expression::Number(
                Number {
                    num: Numeric {s: tag_id.to_string()}, 
                    native_num: NativeNumber::U24(tag_id as u32), 
                }
            ), 
            operator: Operator::Equal, 
        }.into();

        /*let statements = vec![
            SetVar{var: case.var, value: TupleAccess{expression: struct_name, index: i}.into()}.into()
        ];*/

        let statements = vec![
            SetVar{var: case.var.clone(), value: Expression::Variable(struct_name.clone().into())}.into()
        ];

        let block = Block::<Expression> {
            statements: statements, 
            expression: case.expression, 
        };

        (condition, block)
    }).collect();

    let (condition, block) = cases.remove(0);
    let (_, else_) = cases.pop().unwrap();
    let else_ifs = cases;

    Block {
        statements: var_statements, 
        expression: IfElses {
            condition, 
            block, 
            else_ifs, 
            else_, 
        }.into()
    }
}

pub fn switch_to_if_elses_block(switch: Switch<Expression>, namer: &mut UniqueNamer) -> Block<Expression> {
    let expr_name = namer.get_unique_name();
    let var_expr = Expression::Variable(expr_name.clone().into());

    let var_statement = vec![
        SetVar{var: expr_name.into(), value: switch.expression}.into(), 
    ];

    let mut cases: Vec<(Expression, Block<Expression>)> = switch.cases.into_iter().map(|(expression_condition, block_pass)| {
        let condition = Operation {
            expression_left: var_expr.clone(), 
            expression_right: expression_condition, 
            operator: Operator::Equal, 
        };

        let block: Block<Expression> = block_pass.into();

        (condition.into(), block)
    }).collect();

    let else_ = {
        let default_var = switch.default.0;
        let default_expr = switch.default.1;
        Block {
            statements: vec![
                SetVar{var: default_var, value: var_expr}.into(), 
            ], 
            expression: default_expr, 
        }
    };

    let (condition, block) = cases.remove(0);
    let else_ifs = cases;

    Block {
        statements: var_statement, 
        expression: IfElses {
            condition, 
            block, 
            else_ifs, 
            else_, 
        }.into()
    }
}
