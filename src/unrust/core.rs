use crate::unrust::{parse::*, global::*};

use std::collections::HashMap;
use std::collections::BTreeSet;
use std::iter;

#[derive(Debug, Clone)]
pub enum CoreExpression {
    Variable(Variable), 
    Number(Number), 
    Char(Char), 
    Operation(Box<Operation<Self>>), 
    Tuple(Box<Tuple<Self>>), 
    TupleAccess(Box<TupleAccess<Self>>), 
    BinarySwitch(Box<BinarySwitch<Self>>), 
    Block(Box<Block<Self>>), 
    Call(Box<Call<Self>>), 
    Lambda(Box<Lambda<Self>>), 
}

impl<SCtx: Clone, GCtx> VarMap<SCtx, GCtx> for CoreExpression {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        match self {
            CoreExpression::Variable(var) => {
                let (var, ctx, gctx) = f(var, VarUsage::Consume, ctx, gctx);
                (var.into(), ctx, gctx)
            }, 
            CoreExpression::Number(n) => (n.into(), ctx, gctx), 
            CoreExpression::Char(ch) => (ch.into(), ctx, gctx), 
            CoreExpression::Operation(op) => {
                let (op, ctx, gctx) = op.map_vars(f, ctx, gctx);
                (op.into(), ctx, gctx)
            }, 
            CoreExpression::Tuple(t) => {
                let (t, ctx, gctx) = t.map_vars(f, ctx, gctx);
                (t.into(), ctx, gctx)
            }, 
            CoreExpression::TupleAccess(ta) => {
                let (ta, ctx, gctx) = ta.map_vars(f, ctx, gctx);
                (ta.into(), ctx, gctx)
            }, 
            CoreExpression::BinarySwitch(switch) => {
                let (switch, ctx, gctx) = switch.map_vars(f, ctx, gctx);
                (switch.into(), ctx, gctx)
            }, 
            CoreExpression::Block(block) => {
                let (block, ctx, gctx) = block.map_vars(f, ctx, gctx);
                (block.into(), ctx, gctx)
            }, 
            CoreExpression::Call(call) => {
                let (call, ctx, gctx) = call.map_vars(f, ctx, gctx);
                (call.into(), ctx, gctx)
            }, 
            CoreExpression::Lambda(lambda) => {
                let (lambda, ctx, gctx) = lambda.map_vars(f, ctx, gctx);
                (CoreExpression::Lambda(lambda.into()), ctx, gctx)
            }, 
        }
    }
}

impl CoreExpression {
    pub fn from_expression(expr: Expression, namer: &mut UniqueNamer, add_functions: &mut HashMap<String, Function<CoreExpression>>, functions: &HashMap<String, Function<Expression>>, structs: &HashMap<String, Struct>, enums: &HashMap<String, Enum>) -> CoreExpression {
        match expr {
            Expression::Variable(var) => CoreExpression::Variable(var), 
            Expression::Number(n) => CoreExpression::Number(n), 
            Expression::Char(ch) => CoreExpression::Char(ch), 
            /*String(RawString), */
            Expression::Operation(op) => CoreExpression::Operation(
                Operation {
                    expression_left: CoreExpression::from_expression(op.expression_left, namer, add_functions, functions, structs, enums), 
                    expression_right: CoreExpression::from_expression(op.expression_right, namer, add_functions, functions, structs, enums), 
                    operator: op.operator, 
                }.into()
            ), 
            Expression::Tuple(tup) => CoreExpression::Tuple(
                Tuple {
                    elements: tup.elements.into_iter().map(|elem| CoreExpression::from_expression(elem, namer, add_functions, functions, structs, enums)).collect(), 
                }.into()
            ), 
            Expression::TupleAccess(tup_acc) => CoreExpression::TupleAccess(
                TupleAccess {
                    expression: CoreExpression::from_expression(tup_acc.expression, namer, add_functions, functions, structs, enums), 
                    index: tup_acc.index,
                    len: tup_acc.len,  
                }.into()
            ), 
            Expression::IfElses(if_elses) => CoreExpression::Call(
                {
                    let if_else: IfElse<Expression> = Box::into_inner(if_elses).into();
    
    
                    let mut dependent_vars_pass = get_local_dependent_vars(&if_else.block_pass.clone().into(), &functions);
                    let mut dependent_vars_fail = get_local_dependent_vars(&if_else.block_fail.clone().into(), &functions);
                    dependent_vars_pass.append(&mut dependent_vars_fail);
                    let dependent_vars = dependent_vars_pass;
    
    
                    let pass_function_name = namer.get_unique_name();
                    let pass_function = Function {
                        name: pass_function_name.clone().into(), 
                        parameters: dependent_vars.iter().cloned().collect(), 
                        block: match CoreExpression::from_expression(if_else.block_pass.into(), namer, add_functions, functions, structs, enums) {CoreExpression::Block(block) => Box::into_inner(block), _ => panic!()}, 
                    };
    
                    let fail_function_name = namer.get_unique_name();
                    let fail_function = Function {
                        name: fail_function_name.clone().into(), 
                        parameters: dependent_vars.iter().cloned().collect(), 
                        block: match CoreExpression::from_expression(if_else.block_fail.into(), namer, add_functions, functions, structs, enums) {CoreExpression::Block(block) => Box::into_inner(block), _ => panic!()}, 
                    };
    
                    add_functions.insert(pass_function_name.clone(), pass_function);
                    add_functions.insert(fail_function_name.clone(), fail_function);

                    let bswitch =  BinarySwitch {
                        condition: CoreExpression::from_expression(if_else.condition, namer, add_functions, functions, structs, enums), 
                        pass: Variable {
                            name: pass_function_name.into(), 
                            annotation: None, 
                        }, 
                        fail: Variable {
                            name: fail_function_name.into(), 
                            annotation: None, 
                        }, 
                    };

                    Call {
                        expression: bswitch.into(), 
                        arguments: dependent_vars.iter().map(|v| v.clone().into()).collect(), 
                    }.into()
                }
            ), 
            Expression::Block(block) => CoreExpression::Block(
                Block {
                    statements: block.statements.into_iter().map(|sv| {
                        SetVar {
                            var: sv.var, 
                            value: CoreExpression::from_expression(sv.value, namer, add_functions, functions, structs, enums), 
                        }
                    }).collect(), 
                    expression: CoreExpression::from_expression(block.expression, namer, add_functions, functions, structs, enums), 
                }.into()
            ), 
            Expression::Call(call) => CoreExpression::Call(
                Call {
                    expression: CoreExpression::from_expression(call.expression, namer, add_functions, functions, structs, enums), 
                    arguments: call.arguments.into_iter().map(|arg| CoreExpression::from_expression(arg, namer, add_functions, functions, structs, enums)).collect(), 
                }.into()
            ), 
            Expression::Match(match_) => CoreExpression::from_expression(match_to_if_elses_block(*match_, namer, enums).into(), namer, add_functions, functions, structs, enums), 
            Expression::Switch(switch) => CoreExpression::from_expression(switch_to_if_elses_block(*switch, namer).into(), namer, add_functions, functions, structs, enums), 
            Expression::Struct(struct_) => {
                let g_struct = structs.get(struct_.name.as_str()).unwrap();
                let elements = rearrange_elements(struct_.elements, &g_struct.elements[..]);
                let elements: Vec<_> = elements.into_iter().map(|(name, expr)| 
                    CoreExpression::from_expression(expr, namer, add_functions, functions, structs, enums)
                ).collect();
    
                CoreExpression::Tuple(
                    Tuple {
                        elements, 
                    }.into()
                )
            }, 
            Expression::ElementAccess(sa) => {
                let var = sa.var;
                let field = sa.field;
    
                let annotation = var.annotation.clone().unwrap();
                match annotation {
                    Annotation::Struct(sname) => {
                        let struct_ = structs.get(sname.as_str()).unwrap();
    
                        TupleAccess {
                            expression: var.into(), 
                            index: struct_.get_field_index(field.as_str()).unwrap(),
                            len: struct_.elements.len(), 
                        }.into()
                    }, 
                    Annotation::Enum((ename, sname)) => {
                        let enum_ = enums.get(ename.as_str()).unwrap();
                        let index = enum_.get_variant_field_index(sname.as_str(), field.as_str()).unwrap();
    
                        TupleAccess {
                            expression: TupleAccess {
                                expression: var.into(), 
                                index: 1, // tag is first, struct is second
                                len: 2, 
                            }.into(), 
                            index: index, 
                            len: enum_.num_variant_elements(sname.as_str()).unwrap(), 
                        }.into()
                    }, 
                }
            }, 
            Expression::Enum(enum_) => {
                let g_enum = enums.get(enum_.name.as_str()).unwrap();
                let variant_idx = g_enum.elements.iter().enumerate().find(|(i, (name, _))| name == &enum_.struct_.name).map(|(i, _)| i).unwrap();
                let g_enum_struct_elems = &g_enum.elements[variant_idx].1;
    
                let elements = rearrange_elements(enum_.struct_.elements, &g_enum_struct_elems[..]);
                let elements = elements.into_iter().map(|(name, expr)| CoreExpression::from_expression(expr, namer, add_functions, functions, structs, enums)).collect();
                let elements = vec![
                    CoreExpression::Number(
                        Number {
                            num: Numeric {s: variant_idx.to_string()}, 
                            native_num: NativeNumber::U24(variant_idx as u32), 
                        }
                    ), 
                    CoreExpression::Tuple(
                        Tuple {
                            elements, 
                        }.into()
                    )
                ];
    
                CoreExpression::Tuple(
                    Tuple {
                        elements, 
                    }.into()
                )
            }, 
            Expression::Lambda(lambda) => {
                let parameters = lambda.parameters;
                let block = CoreExpression::from_expression(lambda.block.into(), namer, add_functions, functions, structs, enums).into();
    
                CoreExpression::Lambda(
                    Lambda {
                        parameters, 
                        block, 
                    }.into()
                )
            }, 
        }
    }

    /*pub fn from_expression(expr: Expression, namer: &mut UniqueNamer, functions: &mut HashMap<String, Function<CoreExpression>>, structs: &HashMap<String, Struct>, enums: &HashMap<String, Enum>) -> Self {
        match expr {
            Expression::Variable(var) => CoreExpression::Variable(var), 
            Expression::Number(n) => CoreExpression::Number(n), 
            Expression::Char(ch) => CoreExpression::Char(ch), 
            /*String(RawString), */
            Expression::Operation(op) => CoreExpression::Operation(
                Operation {
                    expression_left: CoreExpression::from_expression(op.expression_left, namer, structs, enums), 
                    expression_right: CoreExpression::from_expression(op.expression_right, namer, structs, enums), 
                    operator: op.operator, 
                }.into()
            ), 
            Expression::Tuple(tup) => CoreExpression::Tuple(
                Tuple {
                    elements: tup.elements.into_iter().map(|elem| CoreExpression::from_expression(elem, namer, functions, structs, enums)).collect(), 
                }.into()
            ), 
            Expression::TupleAccess(tup_acc) => CoreExpression::TupleAccess(
                TupleAccess {
                    expression: CoreExpression::from_expression(tup_acc.expression, namer, structs, enums), 
                    index: tup_acc.index,
                    len: tup_acc.len,  
                }.into()
            ), 
            Expression::IfElses(if_elses) => CoreExpression::IfElse(
                {
                    let if_else: IfElse<Expression> = Box::into_inner(if_elses).into();
                    IfElse {
                        condition: CoreExpression::from_expression(if_else.condition, namer, structs, enums), 
                        block_pass: match CoreExpression::from_expression(if_else.block_pass.into(), namer, structs, enums) {CoreExpression::Block(block) => Box::into_inner(block), _ => panic!()}, 
                        block_fail: match CoreExpression::from_expression(if_else.block_fail.into(), namer, structs, enums) {CoreExpression::Block(block) => Box::into_inner(block), _ => panic!()}, 
                    }.into()
                }
            ), 
            Expression::Block(block) => CoreExpression::Block(
                Block {
                    statements: block.statements.into_iter().map(|sv| {
                        SetVar {
                            var: sv.var, 
                            value: CoreExpression::from_expression(sv.value, namer, structs, enums), 
                        }
                    }).collect(), 
                    expression: CoreExpression::from_expression(block.expression, namer, structs, enums), 
                }.into()
            ), 
            Expression::Call(call) => CoreExpression::Call(
                Call {
                    expression: CoreExpression::from_expression(call.expression, namer, structs, enums), 
                    arguments: call.arguments.into_iter().map(|arg| CoreExpression::from_expression(arg, namer, functions, structs, enums)).collect(), 
                }.into()
            ), 
            Expression::Match(match_) => CoreExpression::from_expression(match_to_if_elses_block(*match_, namer, enums).into(), namer, structs, enums), 
            Expression::Switch(switch) => CoreExpression::from_expression(switch_to_if_elses_block(*switch, namer).into(), namer, structs, enums), 
            Expression::Struct(struct_) => {
                let g_struct = structs.get(struct_.name.as_str()).unwrap();
                let elements = rearrange_elements(struct_.elements, &g_struct.elements[..]);
                let elements: Vec<_> = elements.into_iter().map(|(name, expr)| 
                    CoreExpression::from_expression(expr, namer, functions, structs, enums)
                ).collect();

                CoreExpression::Tuple(
                    Tuple {
                        elements, 
                    }.into()
                )
            }, 
            Expression::ElementAccess(sa) => {
                let var = sa.var;
                let field = sa.field;

                let annotation = var.annotation.clone().unwrap();
                match annotation {
                    Annotation::Struct(sname) => {
                        let struct_ = structs.get(sname.as_str()).unwrap();

                        TupleAccess {
                            expression: var.into(), 
                            index: struct_.get_field_index(field.as_str()).unwrap(),
                            len: struct_.elements.len(), 
                        }.into()
                    }, 
                    Annotation::Enum((ename, sname)) => {
                        let enum_ = enums.get(ename.as_str()).unwrap();
                        let index = enum_.get_variant_field_index(sname.as_str(), field.as_str()).unwrap();

                        TupleAccess {
                            expression: TupleAccess {
                                expression: var.into(), 
                                index: 1, // tag is first, struct is second
                                len: 2, 
                            }.into(), 
                            index: index, 
                            len: enum_.num_variant_elements(sname.as_str()).unwrap(), 
                        }.into()
                    }, 
                }
            }, 
            Expression::Enum(enum_) => {
                let g_enum = enums.get(enum_.name.as_str()).unwrap();
                let variant_idx = g_enum.elements.iter().enumerate().find(|(i, (name, _))| name == &enum_.struct_.name).map(|(i, _)| i).unwrap();
                let g_enum_struct_elems = &g_enum.elements[variant_idx].1;

                let elements = rearrange_elements(enum_.struct_.elements, &g_enum_struct_elems[..]);
                let elements = elements.into_iter().map(|(name, expr)| CoreExpression::from_expression(expr, namer, functions, structs, enums)).collect();
                let elements = vec![
                    CoreExpression::Number(
                        Number {
                            num: Numeric {s: variant_idx.to_string()}, 
                            native_num: NativeNumber::U24(variant_idx as u32), 
                        }
                    ), 
                    CoreExpression::Tuple(
                        Tuple {
                            elements, 
                        }.into()
                    )
                ];

                CoreExpression::Tuple(
                    Tuple {
                        elements, 
                    }.into()
                )
            }, 
            Expression::Lambda(lambda) => {
                let parameters = lambda.parameters;
                let block = CoreExpression::from_expression(lambda.block.into(), namer, structs, enums).into();

                CoreExpression::Lambda(
                    Lambda {
                        parameters, 
                        block, 
                    }.into()
                )
            }, 
        }
    }*/
}

fn rearrange_elements<N: Eq, T>(mut elems: Vec<(N, T)>, order: &[N]) -> Vec<(N, T)> {
    elems.sort_by_key(|(n, t)| order.iter().enumerate().find(|(_, no)| no == &n).map(|(i, _)| i).unwrap());
    elems
}

impl From<Variable> for CoreExpression {
    fn from(v: Variable) -> Self {
        CoreExpression::Variable(v)
    }
}

impl From<Number> for CoreExpression {
    fn from(n: Number) -> Self {
        CoreExpression::Number(n)
    }
}

impl From<Char> for CoreExpression {
    fn from(c: Char) -> Self {
        CoreExpression::Char(c)
    }
}

impl From<Operation<CoreExpression>> for CoreExpression {
    fn from(o: Operation<CoreExpression>) -> Self {
        CoreExpression::Operation(Box::new(o))
    }
}

impl From<Tuple<CoreExpression>> for CoreExpression {
    fn from(t: Tuple<CoreExpression>) -> Self {
        CoreExpression::Tuple(Box::new(t))
    }
}

impl From<TupleAccess<CoreExpression>> for CoreExpression {
    fn from(ta: TupleAccess<CoreExpression>) -> Self {
        CoreExpression::TupleAccess(Box::new(ta))
    }
}

impl From<BinarySwitch<CoreExpression>> for CoreExpression {
    fn from(ie: BinarySwitch<CoreExpression>) -> Self {
        CoreExpression::BinarySwitch(Box::new(ie))
    }
}

impl From<Block<CoreExpression>> for CoreExpression {
    fn from(b: Block<CoreExpression>) -> Self {
        CoreExpression::Block(Box::new(b))
    }
}

impl From<Call<CoreExpression>> for CoreExpression {
    fn from(c: Call<CoreExpression>) -> Self {
        CoreExpression::Call(Box::new(c))
    }
}








#[derive(Debug, Clone)]
pub struct BinarySwitch<E> {
    pub condition: E, 
    pub pass: Variable, 
    pub fail: Variable, 
}

impl<SCtx: Clone, GCtx, E: VarMap<SCtx, GCtx>> VarMap<SCtx, GCtx> for BinarySwitch<E> {
    fn map_vars(self, f: impl Fn(Variable, VarUsage, SCtx, GCtx) -> (Variable, SCtx, GCtx) + Clone, ctx: SCtx, gctx: GCtx) -> (Self, SCtx, GCtx) {
        let (condition, _, gctx) = self.condition.map_vars(f.clone(), ctx.clone(), gctx);

        let (pass, _, gctx) = f(self.pass, VarUsage::Consume, ctx.clone(), gctx);
        let (fail, _, gctx) = f(self.fail, VarUsage::Consume, ctx.clone(), gctx);

        let switch = Self {
            condition, 
            pass, 
            fail, 
        };

        (switch, ctx, gctx)
    }
}


fn get_local_dependent_vars(expr: &Expression, functions: &HashMap<String, Function<Expression>>) -> BTreeSet<Variable> {
    &get_local_consumed_vars(expr, functions) - &get_local_introduced_vars(expr, functions)
}

fn get_local_consumed_vars(expr: &Expression, functions: &HashMap<String, Function<Expression>>) -> BTreeSet<Variable> {
    let get_consumes = |var: Variable, usage, mut consumes: BTreeSet<Variable>| {
        match usage {
            VarUsage::Introduce => {
                (var, consumes)
            }, 
            VarUsage::Consume => {
                // only local variables need duplicates
                if !functions.contains_key(var.name.as_str()) {
                    consumes.insert(var.clone());
                }
                
                (var, consumes)
            }, 
        }
    };

    let (_, _, consumes) = expr.clone().map_vars(|v, u, ctx, gctx| {
            let (v, gctx) = get_consumes(v, u, gctx);
            (v, ctx, gctx)
        }, 
        (), 
        BTreeSet::<Variable>::new()
    );

    consumes
}


fn get_local_introduced_vars(expr: &Expression, functions: &HashMap<String, Function<Expression>>) -> BTreeSet<Variable> {
    let get_introduces = |var: Variable, usage, mut introduces: BTreeSet<Variable>| {
        match usage {
            VarUsage::Introduce => {
                // only local variables need duplicates
                if !functions.contains_key(var.name.as_str()) {
                    introduces.insert(var.clone());
                }
                
                (var, introduces)
            }, 
            VarUsage::Consume => {
                (var, introduces)
            }, 
        }
    };

    let (_, _, introduces) = expr.clone().map_vars(|v, u, ctx, gctx| {
            let (v, gctx) = get_introduces(v, u, gctx);
            (v, ctx, gctx)
        }, 
        (), 
        BTreeSet::<Variable>::new()
    );

    introduces
}

