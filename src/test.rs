#[cfg(test)]
mod tests {
    use crate::lexical::*;
    use crate::parser::*;

    use crate::runtime::Runtime;

    /// Tests recursive function calling
    /// sum(n) = if n == 0 then 0 else n + sum(n - 1)
    #[test]
    fn test_summation() {
        let expected_normal_form = "@main = 15";
        let code = "@main = out & @sum ~ (5 out) @sum = (?((0 @sumoneless) out) out) @sumoneless = ({n1 $([+1] $([+] $(res out)))} out) & (n1 res) ~ @sum";

        assert!(does_reduce_to(&code, &expected_normal_form));
    }

    /// Identity function applied to identity
    #[test]
    fn test_identity_application() {
        let expected_normal_form = "@main = (x x)";
        let code = "@main = x & (y y) ~ ((z z) x)";

        assert!(does_reduce_to(&code, &expected_normal_form));
    }

    #[test]
    fn test_function_calling() {
        let expected_normal_form = "@main = 9";
        let code = "@main = s & @function ~ (8 s) @function = (i o) & i ~ $([+1] o)";

        assert!(does_reduce_to(&code, &expected_normal_form));
    }


    #[test]
    fn test_branch() {
        let expected_normal_form = "@main = 5";
        let code = "@main = u & 1 ~ ?((1 (?((5 9) b) b)) u) u)";

        assert!(does_reduce_to(&code, &expected_normal_form));
    }


    // test operators

    #[test]
    fn test_operator_add() {
        // u24
        let expected_normal_form = "@main = 5";
        let code = "@main = x & 2 ~ $([+3] x)";

        assert!(does_reduce_to(&code, &expected_normal_form));

        // i24
        let expected_normal_form = "@main = -7";
        let code = "@main = x & -5 ~ $([+ -2] x)";

        assert!(does_reduce_to(&code, &expected_normal_form));

        // f24
        let expected_normal_form = "@main = 9.0";
        let code = "@main = x & 2.5 ~ $([+ 6.5] x)";

        assert!(does_reduce_to(&code, &expected_normal_form));
    }

    #[test]
    fn test_operator_sub() {
        // u24
        let expected_normal_form = "@main = 4";
        let code = "@main = x & 9 ~ $([-5] x)";

        assert!(does_reduce_to(&code, &expected_normal_form));

        // i24
        let expected_normal_form = "@main = -6";
        let code = "@main = x & -5 ~ $([- 1i24] x)";

        assert!(does_reduce_to(&code, &expected_normal_form));

        // f24
        let expected_normal_form = "@main = 8.0";
        let code = "@main = x & 11.5 ~ $([- 3.5] x)";

        assert!(does_reduce_to(&code, &expected_normal_form));
    }

    #[test]
    fn test_operator_multiply() {
        // u24
        let expected_normal_form = "@main = 20";
        let code = "@main = x & 5 ~ $([* 4] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // i24
        let expected_normal_form = "@main = -15";
        let code = "@main = x & -3 ~ $([* 5i24] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // f24
        let expected_normal_form = "@main = 15.0";
        let code = "@main = x & 1.5 ~ $([* 10.0] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_divide() {
        // u24
        let expected_normal_form = "@main = 3";
        let code = "@main = x & 15 ~ $([/ 5] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // i24
        let expected_normal_form = "@main = -2";
        let code = "@main = x & -10 ~ $([/ 5i24] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));

        // f24
        let expected_normal_form = "@main = 2.5";
        let code = "@main = x & 5.0 ~ $([/ 2.0] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_remainder() {
        // u24
        let expected_normal_form = "@main = 1";
        let code = "@main = x & 10 ~ $([% 3] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // i24
        let expected_normal_form = "@main = -1";
        let code = "@main = x & -10 ~ $([% 3i24] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_equal() {
        // u24
        let expected_normal_form = "@main = 1";
        let code = "@main = x & 5 ~ $([= 5] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // i24
        let expected_normal_form = "@main = 0";
        let code = "@main = x & -5 ~ $([= 5i24] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // f24
        let expected_normal_form = "@main = 1";
        let code = "@main = x & 3.14 ~ $([= 3.14] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_not_equal() {
        // u24
        let expected_normal_form = "@main = 0";
        let code = "@main = x & 5 ~ $([!= 5] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // i24
        let expected_normal_form = "@main = 1";
        let code = "@main = x & -5 ~ $([!= 5i24] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // f24
        let expected_normal_form = "@main = 0";
        let code = "@main = x & 3.5 ~ $([!= 3.5] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_less_than() {
        // u24
        let expected_normal_form = "@main = 1";
        let code = "@main = x & 3 ~ $([< 5] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // i24
        let expected_normal_form = "@main = 1";
        let code = "@main = x & -5 ~ $([< 0i24] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // f24
        let expected_normal_form = "@main = 0";
        let code = "@main = x & 3.14 ~ $([< 3.0] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_greater_than() {
        // u24
        let expected_normal_form = "@main = 0";
        let code = "@main = x & 3 ~ $([> 5] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // i24
        let expected_normal_form = "@main = 0";
        let code = "@main = x & -5 ~ $([> 0i24] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // f24
        let expected_normal_form = "@main = 1";
        let code = "@main = x & 3.14 ~ $([> 3.0] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_and() {
        // u24
        let expected_normal_form = "@main = 1";
        let code = "@main = x & 3 ~ $([& 1] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // i24
        let expected_normal_form = "@main = 1i24";
        let code = "@main = x & -3 ~ $([& 3i24] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_or() {
        // u24
        let expected_normal_form = "@main = 7";
        let code = "@main = x & 5 ~ $([| 3] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // i24
        let expected_normal_form = "@main = -1";
        let code = "@main = x & -3 ~ $([| -1] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_xor() {
        // u24
        let expected_normal_form = "@main = 6";
        let code = "@main = x & 5 ~ $([^ 3] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // i24
        let expected_normal_form = "@main = -4";
        let code = "@main = x & -3 ~ $([^ 1i24] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_shift_right() {
        // u24
        let expected_normal_form = "@main = 2";
        let code = "@main = x & 8 ~ $([.>> 2] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // i24
        let expected_normal_form = "@main = -2";
        let code = "@main = x & -8 ~ $([.>> 2i24] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_shift_left() {
        // u24
        let expected_normal_form = "@main = 32";
        let code = "@main = x & 8 ~ $([.<< 2] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // i24
        let expected_normal_form = "@main = -4";
        let code = "@main = x & -2 ~ $([.<< 1i24] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_fp_subtract() {
        // u24
        let expected_normal_form = "@main = 4";
        let code = "@main = x & 5 ~ $([:- 9] x)";

        assert!(does_reduce_to(&code, &expected_normal_form));

        // i24
        let expected_normal_form = "@main = -6";
        let code = "@main = x & 1i24 ~ $([:- -5] x)";

        assert!(does_reduce_to(&code, &expected_normal_form));

        // f24
        let expected_normal_form = "@main = 8.0";
        let code = "@main = x & 3.5 ~ $([:- 11.5] x)";

        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_fp_divide() {
        // u24
        let expected_normal_form = "@main = 3";
        let code = "@main = x & 5 ~ $([:/ 15] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // i24
        let expected_normal_form = "@main = -2";
        let code = "@main = x & 5i24 ~ $([:/ -10] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));

        // f24
        let expected_normal_form = "@main = 2.5";
        let code = "@main = x & 2.0 ~ $([:/ 5.0] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_fp_remainder() {
        // u24
        let expected_normal_form = "@main = 1";
        let code = "@main = x & 3 ~ $([:% 10] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    
        // i24
        let expected_normal_form = "@main = -1";
        let code = "@main = x & 3i24 ~ $([:% -10] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }

    #[test]
    fn test_operator_fp_shift_right() {
        // u24
        let expected_normal_form = "@main = 4";
        let code = "@main = x & 1 ~ $([:>> 8] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));

        // i24
        let expected_normal_form = "@main = -2";
        let code = "@main = x & 1i24 ~ $([:>> -3] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }
    
    #[test]
    fn test_operator_fp_shift_left() {
        // u24
        let expected_normal_form = "@main = 16";
        let code = "@main = x & 1 ~ $([:<< 8] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));

        // i24
        let expected_normal_form = "@main = -4";
        let code = "@main = x & 1i24 ~ $([:<< -2] x)";
        assert!(does_reduce_to(&code, &expected_normal_form));
    }









    

    fn does_reduce_to(program: &str, expected_normal_form: &str) -> bool {
        let expected_tokens = lexical_parse(expected_normal_form).unwrap();
        println!("tokens {:?}", expected_tokens);
        let expected_program = Program::parse(&expected_tokens[..]).unwrap();
        println!("Expected program: {:?}", expected_program);
        let expected_node = expected_program.main.net.tree;

        let tokens = lexical_parse(&program).unwrap();
        println!("tokens {:?}", tokens);
        let program = Program::parse(&tokens[..]).unwrap();

        println!("program {:?}", program);

        let (main, book) = program.to_defs();
        
        // execute
        let runtime = Runtime::new(&main, book);
        let node = runtime.run();
        println!("node: {:?}", node);

        expected_node.is_equal_to(&node)
    }
}