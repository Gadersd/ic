#[cfg(test)]
mod tests {
    use crate::unrust::lexical::*;
    use crate::unrust::parse::*;
    use crate::unrust::global::*;
    use crate::unrust::affine::*;
    use crate::unrust::check::*;

    use crate::lexical;
    use crate::parser;

    use crate::runtime::Runtime;

    /// Test main return value
    #[test]
    fn test_return() {
        let expected_normal_form = "@main = 5";
        let unrust_code = "fn main() { 5 }";

        assert!(does_reduce_to(&unrust_code, &expected_normal_form));
    }

    /// Test main return value
    #[test]
    fn test_add() {
        let expected_normal_form = "@main = 7";
        let unrust_code = "fn main() { 2 + 5 }";

        assert!(does_reduce_to(&unrust_code, &expected_normal_form));
    }

    fn does_reduce_to(program: &str, expected_normal_form: &str) -> bool {
        let expected_tokens = lexical::lexical_parse(expected_normal_form).unwrap();
        println!("tokens {:?}", expected_tokens);
        let expected_program = parser::Program::parse(&expected_tokens[..]).unwrap();
        println!("Expected program: {:?}", expected_program);
        let expected_node = expected_program.main.net.tree;

        /*let tokens = lexical_parse(&program).unwrap();
        println!("tokens {:?}", tokens);
        let program = Program::parse(&tokens[..]).unwrap();

        println!("program {:?}", program);*/


        let program: Result<Program<Expression>, _> = Program::from_str(program);
        let program = program.unwrap();
        println!("PROGRAM: {:?}", program);
        /*let program = match program {
            Ok(program) => program, 
            Err(pe) => {
                println!("{:?}", &unrust_code[pe.index..]);
                println!("{:?}", pe);
                return;
                //assert!(false);
                //return Err(pe);
            }, 
        };
        println!("{:?}", program);*/

        let checked_program = CheckedProgram::from_program(program).unwrap();
        let global_program = GlobalProgram::from_program(checked_program).to_core();
        let affine_program = GlobalAffineProgram::from_global_program(global_program);
        let ic_program = affine_program.transpile_to_node();


        let (main, book) = ic_program.to_defs();
        
        // execute
        let runtime = Runtime::new(&main, book);
        let node = runtime.run();
        println!("node: {:?}", node);

        expected_node.is_equal_to(&node)
    }
}