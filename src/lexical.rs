use crate::numeric::{Operator};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Erase, 
    Reference, 
    LeftParenthesis, 
    RightParenthesis, 
    LeftBrace, 
    RightBrace, 
    LeftBracket, 
    RightBracket, 
    Operator(Operator), 
    Operate, 
    Switch, 
    Redex, 
    Ampersand, 
    Equal, 
    Alphanumeric(String), 
}

impl Token {
    fn as_str(&self) -> &str {
        match self {
            Token::Erase => "*",
            Token::Reference => "@",
            Token::LeftParenthesis => "(",
            Token::RightParenthesis => ")",
            Token::LeftBrace => "{",
            Token::RightBrace => "}",
            Token::LeftBracket => "[",
            Token::RightBracket => "]",
            Token::Operator(op) => op.as_str(), 
            Token::Operate => "$(", 
            Token::Switch => "?(",
            Token::Redex => "~",
            Token::Ampersand => "&",
            Token::Equal => "=",
            Token::Alphanumeric(s) => s.as_str(),
        }
    }

    fn static_token_iter() -> impl Iterator<Item=Token> + Clone {
        [
            Token::Erase, 
            Token::Reference, 
            Token::LeftParenthesis, 
            Token::RightParenthesis, 
            Token::LeftBrace, 
            Token::RightBrace, 
            Token::LeftBracket, 
            Token::RightBracket, 
            Token::Operate, 
            Token::Switch, 
            Token::Redex, 
            Token::Ampersand, 
            Token::Equal, 
        ].into_iter()
    }

    fn operator_iter() -> impl Iterator<Item=Token> + Clone {
        Operator::iter().map(|op| Token::Operator(op))
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
        //println!("{}", &s[i..]);
        let token_end = if last_token != Some(Token::LeftBracket) {
            Token::static_token_iter().filter_map(|token| {
                parse_fixed_token(&s[i..], token.as_str()).map(|(s, e)| (token, e))
            }).next()
        } else {
            Token::operator_iter().filter_map(|token| {
                parse_fixed_token(&s[i..], token.as_str()).map(|(s, e)| (token, e))
            }).next()
        };

        let token = if let Some( (token, end_offset) ) = token_end {
            i += end_offset;
            token
        } else {
            //println!("{}", &s[i..]);
            if let Some( (an, _, end_offset) ) = parse_alphanumeric_token(&s[i..]) {
                i += end_offset;
                Token::Alphanumeric(an.into())
            } else {
                let start = get_first_nonwhitespace_index(&s[i..]).unwrap_or(i);
                let end = get_first_whitespace_index(&s[i..]).unwrap_or(s.len());
                return Err(format!("Parse error {}", &s[(i+start)..(i+end)]));
            }
        };

        last_token = Some(token.clone());
        tokens.push(token);
    }

    Ok( tokens )
}

