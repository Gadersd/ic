use crate::numeric::{Operator};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LeftParenthesis, 
    RightParenthesis, 
    LeftBrace, 
    RightBrace, 
    LeftBracket, 
    RightBracket, 
    Semicolon, 
    Fn, 
    Struct, 
    Enum, 
    Operator(Operator), 
    Match, 
    Switch, 
    Equal, 
    Arrow, 
    Comma, 
    Colon, 
    DoubleColon, 
    If, 
    Else, 
    Char(String), 
    String(String), 
    Alphanumeric(String), 
    Period, 
    Pipe, 
}

impl Token {
    fn as_str(&self) -> &str {
        match self {
            Token::LeftParenthesis => "(", 
            Token::RightParenthesis => ")", 
            Token::LeftBrace => "{", 
            Token::RightBrace => "}", 
            Token::LeftBracket => "[", 
            Token::RightBracket => "]", 
            Token::Semicolon => ";", 
            Token::Fn => "fn", 
            Token::Struct => "struct", 
            Token::Enum => "enum", 
            Token::Operator(op) => operator_to_str(*op), 
            Token::Match => "match", 
            Token::Switch => "switch",  
            Token::Equal => "=", 
            Token::Alphanumeric(s) => s.as_str(), 
            Token::Arrow => "=>", 
            Token::Comma => ",", 
            Token::Colon => ":", 
            Token::DoubleColon => "::", 
            Token::If => "if", 
            Token::Else => "else", 
            Token::Char(ch) => ch.as_str(), 
            Token::String(s) => s.as_str(), 
            Token::Period => ".", 
            Token::Pipe => "|", 
        }
    }

    fn static_token_iter() -> impl Iterator<Item=Token> + Clone {
        [
            Token::LeftParenthesis, 
            Token::RightParenthesis, 
            Token::LeftBrace, 
            Token::RightBrace, 
            Token::LeftBracket, 
            Token::RightBracket, 
            Token::Semicolon, 
            Token::Fn, 
            Token::Struct, 
            Token::Enum, 
            Token::Match, 
            Token::Switch, 
            Token::Arrow, 
            Token::Equal, 
            Token::Comma, 
            Token::DoubleColon, 
            Token::Colon, 
            Token::If, 
            Token::Else, 
            Token::Period, 
            Token::Pipe, 
        ].into_iter()
    }

    fn operator_iter() -> impl Iterator<Item=Token> + Clone {
        Operator::iter().map(|op| Token::Operator(op))
    }
}








fn operator_to_str(op: Operator) -> &'static str {
    match op {
        Operator::Add => "+",
        Operator::Subtract => "-",
        Operator::Multiply => "*",
        Operator::Divide => "/",
        Operator::Remainder => "%",
        Operator::Equal => "==",
        Operator::NotEqual => "!=",
        Operator::LessThan => "<",
        Operator::GreaterThan => ">",
        Operator::And => "&",
        Operator::Or => "|",
        Operator::Xor => "^",
        Operator::ShiftRight => ">>",
        Operator::ShiftLeft => "<<",
        Operator::FpSubtract => ":-",
        Operator::FpDivide => ":/",
        Operator::FpRemainder => ":%",
        Operator::FpShiftRight => ":>>",
        Operator::FpShiftLeft => ":<<",
    }
}




fn parse_operator(s: &str) -> Option<(Operator, usize, usize)> {
    Operator::iter().filter_map(|op| {
        parse_fixed_token(s, operator_to_str(op)).map(|(s, e)| (op, s, e))
    }).next()
}



fn parse_char(s: &str) -> Option<(char, usize, usize)> {
    let start = get_first_nonwhitespace_index(s)?;
    if s.chars().nth(start).unwrap() == '\'' {
        let ch = s.chars().nth(start + 1)?;
        if s.chars().nth(start + 2)? == '\'' {
            let end = start + 3;

            Some( (ch, start, end) )
        } else {
            None
        }
    } else {
        None
    }
}

fn parse_string(s: &str) -> Option<(String, usize, usize)> {
    let start = get_first_nonwhitespace_index(s)?;
    if s.chars().nth(start).unwrap() == '"' {
        let (right_quote_idx, _) = s.chars().enumerate().skip(start + 1).find(|(_, ch)| *ch == '"')?;
        let end = right_quote_idx + 1;
        let string: String = s[start..end].into();

        Some( (string, start, end) )
    } else {
        None
    }
}

// for fixed length static tokens
fn parse_fixed_token(s: &str, tok: &str) -> Option<(usize, usize)> {
    let start = get_first_nonwhitespace_index(s)?;

    for i in 0..tok.len() {
        if s.chars().nth(start + i)? != tok.chars().nth(i)? {
            return None;
        }
    }

    Some( (start, start + tok.len()) )
}

fn parse_alphanumeric_token(s: &str) -> Option<(&str, usize, usize)> {
    let start = get_first_nonwhitespace_index(s)?;

    let mut first_non_alphanumeric = start;
    loop {
        let c = s.chars().nth(first_non_alphanumeric).unwrap();
        if !c.is_ascii_alphanumeric() && c != '-' && c != '.' {
            break;
        }

        first_non_alphanumeric += 1;

        // all alphanumeric past start
        if first_non_alphanumeric >= s.len() {
            return Some( (&s[start..first_non_alphanumeric], start, first_non_alphanumeric) );
        }
    }

    if first_non_alphanumeric > start {
        Some( (&s[start..first_non_alphanumeric], start, first_non_alphanumeric) )
    } else {
        None
    }
}

fn get_first_nonwhitespace_index(s: &str) -> Option<usize> {
    let mut start = 0;
    loop {
        if !s.chars().nth(start)?.is_whitespace() {
            return Some(start)
        }

        start += 1;
    }
}

fn get_first_whitespace_index(s: &str) -> Option<usize> {
    let mut start = 0;
    loop {
        if s.chars().nth(start)?.is_whitespace() {
            return Some(start)
        }

        start += 1;
    }
}

pub fn lexical_parse(s: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();

    let mut i = 0;
    let mut last_token = None;

    let mut k = 0;

    while i < s.len() {
        let token_end = if let Some( (op, _, end_offset) ) = parse_operator(&s[i..]) {
            Some( (Token::Operator(op), end_offset) )
        } else {
            Token::static_token_iter().filter_map(|token| {
                parse_fixed_token(&s[i..], token.as_str()).map(|(s, e)| (token, e))
            }).next()
        };

        let token = if let Some( (token, end_offset) ) = token_end {
            i += end_offset;
            token
        } else if let Some( (an, _, end_offset) ) = parse_alphanumeric_token(&s[i..]) {
            i += end_offset;
            Token::Alphanumeric(an.into())
        } else if let Some( (ch, _, end_offset) ) = parse_char(&s[i..]) {
            i += end_offset;
            Token::Char(ch.into())
        } else if let Some( (string, _, end_offset) ) = parse_string(&s[i..]) {
            i += end_offset;
            Token::String(string)
        } else {
            let start = get_first_nonwhitespace_index(&s[i..]).unwrap_or(i);
            let end = get_first_whitespace_index(&s[i..]).unwrap_or(s.len());
            return Err(format!("Parse error {}", &s[(i+start)..(i+end)]));
        };

        last_token = Some(token.clone());
        tokens.push(token);
    }

    Ok( tokens )
}











