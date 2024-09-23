use crate::lexical::{self, Token};
use crate::runtime::{self, Port, PortType, Pair, Value, Def};
use crate::numeric::{self, Operator};

use std::iter;
use std::collections::HashMap;


#[derive(Debug, Clone)]
pub struct Program {
    pub main: Book, 
    pub others: Vec<Book>, 
}

impl Program {
    pub fn parse(s: &[Token]) -> Option<Self> {
        let mut others = Vec::new();
        let mut main = None;

        let mut s = s;
        while let Some( (book, _s) ) = Book::parse(s) {
            s = _s;
            if book.name.s != "main" {
                others.push(book);
            } else {
                if main.is_none() {
                    main = Some(book);
                } else {
                    eprintln!("Conflicting main books");
                    return None;
                }
            }
        }

        if main.is_none() {
            eprintln!("No main book found");
            return None;
        }

        let main = main.unwrap();

        let program = Program {
            main, 
            others, 
        };

        Some(program)
    }

    pub fn to_defs(&self) -> (runtime::Def, runtime::Book) {
        let mut ref_ids = HashMap::new();

        let main_def = self.main.as_def(&mut ref_ids);
        
        let mut defs = Vec::new();
        for x in &self.others {
            let def = x.as_def(&mut ref_ids);
            let book_id = ref_ids[&x.name.s];
            
            defs.push( (def, book_id) );
        }
        /*let mut defs: Vec<_> = self.others.iter()
            .map(|x| {
                let def = x.as_def(&mut ref_ids);
                let book_id = ref_ids.get(&x.name.s).unwrap();

                (def, book_id)
            })
            .collect();*/

        defs.sort_by_key(|(_, book_id)| book_id.clone());
        let defs = defs.into_iter().map(|(def, _)| def).collect();

        let book = runtime::Book {
            defs, 
        };

        (main_def, book)
    }

    /*pub fn to_main_def(&self) -> runtime::Def {
        
    }

    pub fn to_book(&self) -> runtime::Book {
        let mut ref_ids = HashMap::new();

        let defs = self.others.iter()
            .map(|x| x.as_def(&mut ref_ids))
            .collect();

        runtime::Book {
            defs, 
        }
    }*/
    
    /*pub fn as_book(self) -> runtime::Book {
        let defs = iter::once(&self.main)
            .chain(self.others.iter())
            .map(|x| x.as_def())
            .collect();

        runtime::Book {
            defs, 
        }
    }*/

    pub fn from_core_books(main: &runtime::Def, others: &runtime::Book) -> Option<Self> {
        let main = Book::from_nodes("main".into(), main.root, &main.nodes[..], &main.rbag[..])?;
        let others = others.defs.iter()
            .enumerate()
            .map(|(i, def)| Book::from_nodes(value_as_name(i as u32), def.root, &def.nodes[..], &def.rbag[..]))
            .collect::<Option<Vec<_>>>()?;

        Some(
            Self {
                main, 
                others, 
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Book {
    pub name: Alphanumeric, 
    pub net: Net, 
}

impl Book {
    pub fn to_string(&self) -> String {
        format!("@{} = {}", self.name.as_str(), self.net.to_string())
    }

    pub fn from_nodes(name: String, root: Port, nodes: &[Pair], redexes: &[Pair]) -> Option<Self> {
        let net = Net::from_nodes(root, nodes, redexes)?;
        Some(
            Book {
                name: name.into(), 
                net: net, 
            }
        )
    }

    fn parse(s: &[Token]) -> Option<(Self, &[Token])> {
        let (reference, s) = parse_reference(s)?;
        let s = parse_specific_token(s, &Token::Equal)?;
        let (net, s) = Net::parse(s)?;

        let book = Self {
            name: reference, 
            net: net, 
        };

        Some( (book, s) )
    }

    fn as_def(&self, ref_ids: &mut HashMap<String, usize>) -> Def {
        let safe = !self.net.has_dup();

        let mut nodes = NodeStore {
            nodes: Vec::new(), 
            rbag: Vec::new(), 
            var_ids: HashMap::new(), 
        };

        let root = self.net.add_nodes(&mut nodes, ref_ids);
    
        // register this book
        if self.name.s != "main" && !ref_ids.contains_key(&self.name.s) {
            let id = ref_ids.len();
            ref_ids.insert(self.name.s.clone(), id);
            //println!("                 Inserted name: {} id {}", self.name.s, id);
        }
        
        Def {
            safe: safe, 
            root: root, 
            rbag: nodes.rbag, 
            nodes: nodes.nodes, 
        }
    }
}

struct NodeStore {
    nodes: Vec<Pair>, 
    rbag: Vec<Pair>, 
    var_ids: HashMap<String, usize>, 
}


#[derive(Debug, Clone)]
pub struct Net {
    pub tree: Node, 
    pub redexes: Vec<Redex>, 
}

impl Net {
    pub fn to_string(&self) -> String {
        format!("{} & {}", self.tree.to_string(), self.redexes.iter().map(|r| format!("{} & ", r.to_string())).collect::<String>())
    }

    fn from_nodes(root: Port, nodes: &[Pair], redexes: &[Pair]) -> Option<Self> {
        let root_node = Node::from_nodes(root, nodes)?;
        let redex_nodes = redexes.into_iter().map(|&r| Redex::from_redex_pair(r, nodes)).collect::<Option<Vec<_>>>()?;
        Some(
            Net {
                tree: root_node, 
                redexes: redex_nodes, 
            }
        )
    }

    fn has_dup(&self) -> bool {
        self.tree.has_dup() || self.redexes.iter().any(|redex| redex.has_dup())
    }

    fn parse(s: &[Token]) -> Option<(Self, &[Token])> {
        let (tree, s) = Node::parse(s)?;

        let mut redexes = Vec::new();
        let mut s = s;
        while let Some(_s) = parse_specific_token(s, &Token::Ampersand) {
            let (redex, _s) = Redex::parse(_s)?;
            redexes.push(redex);
            s = _s;
        }

        let net = Net {
            tree, 
            redexes, 
        };

        Some( (net, s) )
    }

    // returns the root
    fn add_nodes(&self, nodes: &mut NodeStore, ref_ids: &mut HashMap<String, usize>) -> Port {
        let root = self.tree.add_nodes(nodes, ref_ids);
        for redex in &self.redexes {
            redex.add_nodes(nodes, ref_ids);
        }

        root
    }
}

#[derive(Debug, Clone)]
pub struct Redex {
    pub tree_1: Node, 
    pub tree_2: Node, 
}

impl Redex {
    pub fn to_string(&self) -> String {
        format!("{} ~ {}", self.tree_1.to_string(), self.tree_2.to_string())
    }

    fn from_redex_pair(redex: Pair, nodes: &[Pair]) -> Option<Self> {
        let (port_1, port_2) = redex.to_ports();
        let (node_1, node_2) = (Node::from_nodes(port_1, nodes)?, Node::from_nodes(port_2, nodes)?);
        Some(
            Redex {
                tree_1: node_1, 
                tree_2: node_2, 
            }
        )
    }

    fn has_dup(&self) -> bool {
        self.tree_1.has_dup() || self.tree_2.has_dup()
    }

    fn add_nodes(&self, nodes: &mut NodeStore, ref_ids: &mut HashMap<String, usize>) {
        let port_1 = self.tree_1.add_nodes(nodes, ref_ids);
        let port_2 = self.tree_2.add_nodes(nodes, ref_ids);

        nodes.rbag.push(Pair::from_ports(port_1, port_2));
    }

    fn parse(s: &[Token]) -> Option<(Self, &[Token])> {
        let (n1, s) = Node::parse(s)?;
        let s = parse_specific_token(s, &Token::Redex)?;
        let (n2, s) = Node::parse(s)?;

        let redex = Redex {
            tree_1: n1, 
            tree_2: n2, 
        };

        Some( (redex, s) )
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    Eraser, 
    Reference(Alphanumeric), 
    Variable(Alphanumeric), 
    Numeric(Numeric), 
    Construct(Box<Constructor>), 
    Duplicate(Box<Duplicator>), 
    Operate(Box<Operate>), 
    Switch(Box<Switch>)
}

impl From<Numeric> for Node {
    fn from(value: Numeric) -> Self {
        Node::Numeric(value)
    }
}

impl From<Constructor> for Node {
    fn from(value: Constructor) -> Self {
        Node::Construct(Box::new(value))
    }
}

impl From<Duplicator> for Node {
    fn from(value: Duplicator) -> Self {
        Node::Duplicate(Box::new(value))
    }
}

impl From<Operate> for Node {
    fn from(value: Operate) -> Self {
        Node::Operate(Box::new(value))
    }
}

impl From<Switch> for Node {
    fn from(value: Switch) -> Self {
        Node::Switch(Box::new(value))
    }
}

impl Node {
    pub fn to_string(&self) -> String {
        match self {
            Node::Eraser => "*".into(), 
            Node::Reference(r) => format!("@{}", r.as_str()), 
            Node::Variable(var) => var.clone().into_string(), 
            Node::Numeric(num) => num.to_string(), 
            Node::Construct(con) => format!("({} {})", con.a.to_string(), con.b.to_string()), 
            Node::Duplicate(dup) => format!("{{{} {}}}", dup.a.to_string(), dup.b.to_string()), 
            Node::Operate(op) => format!("$({} {})", op.a.to_string(), op.b.to_string()), 
            Node::Switch(sw) => format!("?({} {})", sw.a.to_string(), sw.b.to_string()), 
        }
    }

    fn has_dup(&self) -> bool {
        match self {
            Node::Eraser => false, 
            Node::Reference(_) => false, 
            Node::Variable(_) => false, 
            Node::Numeric(_) => false, 
            Node::Construct(con) => con.has_dup(), 
            Node::Duplicate(_) => true, 
            Node::Operate(op) => op.has_dup(), 
            Node::Switch(sw) => sw.has_dup(), 
        }
    }

    pub fn is_equal_to(&self, other: &Node) -> bool {
        let (eq, _) = self._is_equal(other);
        eq
    }

    fn _is_equal(&self, other: &Node) -> (bool, HashMap<String, String>) {
        let nodes_of_pair_eq = |(a1, a2): (&Node, &Node), (b1, b2): (&Node, &Node)| {
            let (left_eq, hm_left) = a1._is_equal(b1);
            if left_eq {
                let (right_eq, hm_right) = a2._is_equal(b2);
                if right_eq {
                    // consolidate the var maps
                    let mut hm = hm_left;
                    for (from, to) in hm_right {
                        let agree = hm.get(&from).map(|_to| _to == &to).unwrap_or(true);
                        if agree {
                            hm.insert(from, to); 
                        } else {
                            return (false, HashMap::new());
                        }
                    }

                    (true, hm)
                } else {
                    (false, HashMap::new())
                }
            } else {
                (false, HashMap::new())
            }
        };

        match self {
            Node::Eraser => {
                if let Node::Eraser = other {
                    (true, HashMap::new())
                } else {
                    (false, HashMap::new())
                }
            }, 
            Node::Reference(r) => {
                if let Node::Reference(r2) = other {
                    (r == r2, HashMap::new())
                } else {
                    (false, HashMap::new())
                }
            }, 
            Node::Variable(var) => {
                if let Node::Variable(var2) = other {
                    (true, HashMap::from_iter(iter::once((var.s.clone(), var2.s.clone()))))
                } else {
                    (false, HashMap::new())
                }
            }, 
            Node::Numeric(num) => {
                if let Node::Numeric(num2) = other {
                    (num.equal(num2), HashMap::new())
                } else {
                    (false, HashMap::new())
                }
            }, 
            Node::Construct(con) => {
                if let Node::Construct(con2) = other {
                    nodes_of_pair_eq((&con.a, &con.b), (&con2.a, &con2.b))
                } else {
                    (false, HashMap::new())
                }
            }, 
            Node::Duplicate(dup) => {
                if let Node::Duplicate(dup2) = other {
                    nodes_of_pair_eq((&dup.a, &dup.b), (&dup2.a, &dup2.b))
                } else {
                    (false, HashMap::new())
                }
            }, 
            Node::Operate(op) => {
                if let Node::Operate(op2) = other {
                    nodes_of_pair_eq((&op.a, &op.b), (&op2.a, &op2.b))
                } else {
                    (false, HashMap::new())
                }
            }, 
            Node::Switch(sw) => {
                if let Node::Switch(sw2) = other {
                    nodes_of_pair_eq((&sw.a, &sw.b), (&sw2.a, &sw2.b))
                } else {
                    (false, HashMap::new())
                }
            }, 
        }
    }

    fn from_nodes(port: Port, nodes: &[Pair]) -> Option<Self> {
        let value = port.get_value();

        let ret = if port.is_binary() {
            let (port_1, port_2) = nodes.get(value as usize)?.to_ports();
            let (node_1, node_2) = (Node::from_nodes(port_1, nodes)?, Node::from_nodes(port_2, nodes)?);
            match port.get_type()? {
                PortType::Constructor => Node::Construct( Constructor {a: node_1, b: node_2}.into() ), 
                PortType::Duplicator => Node::Duplicate( Duplicator {a: node_1, b: node_2}.into() ), 
                PortType::Operator => Node::Operate( Operate {a: node_1, b: node_2}.into() ), 
                PortType::Switch => Node::Switch( Switch {a: node_1, b: node_2}.into() ), 
                _ => panic!(), 
            }
        } else {
            match port.get_type()? {
                PortType::Eraser => Node::Eraser, 
                PortType::Reference => Node::Reference((value_as_name(value)+"_ref").into()), 
                PortType::Numeric => Node::Numeric(Numeric::from_port_value(value)?), 
                PortType::Variable => Node::Variable((value_as_name(value)+"_var").into()), 
                _ => panic!(), 
            }
        };

        Some(ret)
    }

    fn add_nodes(&self, nodes: &mut NodeStore, ref_ids: &mut HashMap<String, usize>) -> Port {
        match self {
            Node::Eraser => Port::from_type_value(PortType::Eraser, 0), 
            Node::Reference(name) => {
                let id = ref_ids.get(&name.s).cloned().unwrap_or_else(|| {
                    let id = ref_ids.len();
                    ref_ids.insert(name.s.clone(), id);
                    //println!("                       name: {} val: {}", name.s, id);
                    id
                });
                
                Port::from_type_value(PortType::Reference, id as Value)
            }, 
            Node::Variable(name) => {
                let id = nodes.var_ids.get(&name.s).cloned().unwrap_or_else(|| {
                    let id = nodes.var_ids.len();
                    nodes.var_ids.insert(name.s.clone(), id);
                    id
                });
                
                Port::from_type_value(PortType::Variable, id as Value)
            }, 
            Node::Numeric(n) => {
                n.to_port()
            }, 
            Node::Construct(con) => {
                con.add_nodes(nodes, ref_ids)
            }, 
            Node::Duplicate(dup) => {
                dup.add_nodes(nodes, ref_ids)
            }, 
            Node::Operate(op) => {
                op.add_nodes(nodes, ref_ids)
            }, 
            Node::Switch(sw) => {
                sw.add_nodes(nodes, ref_ids)
            }
        }
    }

    fn parse(s: &[Token]) -> Option<(Self, &[Token])> {
        if let Some(s) = parse_specific_token(s, &Token::Erase) {
            Some( (Node::Eraser, s) )
        } else if let Some( (a, s) ) = parse_reference(s) {
            Some( (Node::Reference(a), s) )
        } else if let Some( (a, s) ) = parse_variable(s) {
            Some( (Node::Variable(a), s) )
        } else if let Some( (n, s) ) = Numeric::parse(s) {
            Some( (Node::Numeric(n), s) )
        } else if let Some( (c, s) ) = Constructor::parse(s) {
            Some( (Node::Construct(c.into()), s) )
        } else if let Some( (d, s) ) = Duplicator::parse(s) {
            Some( (Node::Duplicate(d.into()), s) )
        } else if let Some( (o, s) ) = Operate::parse(s) {
            Some( (Node::Operate(o.into()), s) )
        } else if let Some( (sw, s) ) = Switch::parse(s) {
            Some( (Node::Switch(sw.into()), s) )
        } else {
            None
        }
    }
}

fn value_as_name(val: Value) -> String {
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

fn parse_reference(s: &[Token]) -> Option<(Alphanumeric, &[Token])> {
    s.get(0).and_then(|tok| {
        if *tok == Token::Reference {
            Alphanumeric::parse(&s[1..])
        } else {
            None
        }
    })
}

fn parse_variable(s: &[Token]) -> Option<(Alphanumeric, &[Token])> {
    let (a, s) = Alphanumeric::parse(s)?;
    if a.s.chars().nth(0)?.is_alphabetic() {
        Some( (a, s) )
    } else {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Constructor {
    pub a: Node, 
    pub b: Node, 
}

impl Constructor {
    fn has_dup(&self) -> bool {
        self.a.has_dup() || self.b.has_dup()
    }

    fn add_nodes(&self, nodes: &mut NodeStore, ref_ids: &mut HashMap<String, usize>) -> Port {
        let port_a = self.a.add_nodes(nodes, ref_ids);
        let port_b = self.b.add_nodes(nodes, ref_ids);

        let node_idx = nodes.nodes.len() as u32;
        nodes.nodes.push(Pair::from_ports(port_a, port_b));

        Port::from_type_value(PortType::Constructor, node_idx)
    }

    fn parse(s: &[Token]) -> Option<(Self, &[Token])> {
        let s = parse_specific_token(s, &Token::LeftParenthesis)?;
        let (a, s) = Node::parse(s)?;
        let (b, s) = Node::parse(s)?;
        let s = parse_specific_token(s, &Token::RightParenthesis)?;

        Some( (Self {a, b}, s) )
    }
}

#[derive(Debug, Clone)]
pub struct Duplicator {
    pub a: Node, 
    pub b: Node, 
}

impl Duplicator {
    fn has_dup(&self) -> bool {
        self.a.has_dup() || self.b.has_dup()
    }

    fn add_nodes(&self, nodes: &mut NodeStore, ref_ids: &mut HashMap<String, usize>) -> Port {
        let port_a = self.a.add_nodes(nodes, ref_ids);
        let port_b = self.b.add_nodes(nodes, ref_ids);

        let node_idx = nodes.nodes.len() as u32;
        nodes.nodes.push(Pair::from_ports(port_a, port_b));

        Port::from_type_value(PortType::Duplicator, node_idx)
    }

    fn parse(s: &[Token]) -> Option<(Self, &[Token])> {
        let s = parse_specific_token(s, &Token::LeftBrace)?;
        let (a, s) = Node::parse(s)?;
        let (b, s) = Node::parse(s)?;
        let s = parse_specific_token(s, &Token::RightBrace)?;

        Some( (Self {a, b}, s) )
    }
}

#[derive(Debug, Clone)]
pub struct Operate {
    pub a: Node, 
    pub b: Node, 
}

impl Operate {
    fn has_dup(&self) -> bool {
        self.a.has_dup() || self.b.has_dup()
    }

    fn add_nodes(&self, nodes: &mut NodeStore, ref_ids: &mut HashMap<String, usize>) -> Port {
        let port_a = self.a.add_nodes(nodes, ref_ids);
        let port_b = self.b.add_nodes(nodes, ref_ids);

        let node_idx = nodes.nodes.len() as u32;
        nodes.nodes.push(Pair::from_ports(port_a, port_b));

        Port::from_type_value(PortType::Operator, node_idx)
    }

    fn parse(s: &[Token]) -> Option<(Self, &[Token])> {
        let s = parse_specific_token(s, &Token::Operate)?;
        let (a, s) = Node::parse(s)?;
        let (b, s) = Node::parse(s)?;
        let s = parse_specific_token(s, &Token::RightParenthesis)?;

        Some( (Self {a, b}, s) )
    }
}

#[derive(Debug, Clone)]
pub struct Switch {
    pub a: Node, 
    pub b: Node, 
}

impl Switch {
    fn has_dup(&self) -> bool {
        self.a.has_dup() || self.b.has_dup()
    }

    fn add_nodes(&self, nodes: &mut NodeStore, ref_ids: &mut HashMap<String, usize>) -> Port {
        let port_a = self.a.add_nodes(nodes, ref_ids);
        let port_b = self.b.add_nodes(nodes, ref_ids);

        let node_idx = nodes.nodes.len() as u32;
        nodes.nodes.push(Pair::from_ports(port_a, port_b));

        Port::from_type_value(PortType::Switch, node_idx)
    }

    fn parse(s: &[Token]) -> Option<(Self, &[Token])> {
        let s = parse_specific_token(s, &Token::Switch)?;
        let (a, s) = Node::parse(s)?;
        let (b, s) = Node::parse(s)?;
        let s = parse_specific_token(s, &Token::RightParenthesis)?;

        Some( (Self {a, b}, s) )
    }
}








#[derive(Debug, Clone)]
pub enum Numeric {
    Number(Number), 
    Operation(Operation), 
    Sym(Symbol), 
}

impl Numeric {
    pub fn to_string(&self) -> String {
        match self {
            Numeric::Number(n) => n.to_string(), 
            Numeric::Operation(op) => op.to_string(), 
            Numeric::Sym(sym) => sym.to_string(), 
        }
    }

    fn equal(&self, other: &Self) -> bool {
        match (self, other) {
            (Numeric::Number(n1), Numeric::Number(n2)) => n1.equal(n2), 
            (Numeric::Operation(op1), Numeric::Operation(op2)) => op1 == op2, 
            (Numeric::Sym(op1), Numeric::Sym(op2)) => op1 == op2, 
            _ => false, 
        }
    }

    fn to_port(&self) -> Port {
        self.as_core_numeric().to_port()
    }

    fn to_value(&self) -> Value {
        self.as_core_numeric().to_value()
    }

    fn from_port_value(value: Value) -> Option<Self> {
        Some( Self::from_core_numeric(&numeric::Numeric::from_value(value)?) )
    }

    fn as_core_numeric(&self) -> numeric::Numeric {
        match self {
            Numeric::Number(num) => numeric::Numeric::Number(num.native_num), 
            Numeric::Operation(op) => numeric::Numeric::Operation(op.as_core()),
            Numeric::Sym(sym) => numeric::Numeric::Sym(sym.op), 
        }
    }

    fn from_core_numeric(numeric: &numeric::Numeric) -> Self {
        match numeric {
            numeric::Numeric::Number(num) => Numeric::Number(Number::from_core_number(*num)), 
            numeric::Numeric::Operation(op) => Numeric::Operation(Operation::from_core(op.clone())), 
            numeric::Numeric::Sym(op) => Numeric::Sym(Symbol {op: *op}), 
        }
    }

    fn parse(s: &[Token]) -> Option<(Self, &[Token])> {
        if let Some( (n, s) ) = Number::parse(s) {
            Some( (Numeric::Number(n), s) )
        } else if let Some( (op, s) ) = Operation::parse(s) {
            Some( (Numeric::Operation(op), s) )
        } else if let Some( (sym, s) ) = Symbol::parse(s) {
            Some( (Numeric::Sym(sym), s) )
        } else {
            None
        }
    }
}




use std::str::FromStr;

#[derive(Debug, Clone)]
pub struct Number {
    pub n: Alphanumeric, 
    pub native_num: numeric::Number, 
}

impl Number {
    pub fn to_string(&self) -> String {
        self.n.clone().into_string()
    }

    fn equal(&self, other: &Self) -> bool {
        self.native_num == other.native_num
    }

    fn from_core_number(num: numeric::Number) -> Self {
        let name = match num {
            numeric::Number::U24(uint) => uint.to_string().into(), 
            numeric::Number::I24(int) => int.to_string().into(), 
            numeric::Number::F24(f) => f.to_string().into(), 
        };

        Self {
            n: name, 
            native_num: num, 
        }
    }

    fn parse(s: &[Token]) -> Option<(Self, &[Token])> {
        let (a, s) = Alphanumeric::parse(s)?;

        let num = if a.s.ends_with("u24") {
            numeric::Number::U24( parse_u24(a.s.split_once("u24")?.0)? )
        } else if a.s.ends_with("i24") {
            numeric::Number::I24( parse_i24(a.s.split_once("i24")?.0)? )
        } else if a.s.ends_with("f24") {
            numeric::Number::F24( a.s.split_once("f24")?.0.parse::<f32>().ok()? )
        } else {
            if a.s.contains('.') {
                numeric::Number::F24( a.s.parse::<f32>().ok()? )
            } else {
                if let Some(u24) = parse_u24(&a.s) {
                    numeric::Number::U24( u24 )
                } else if let Some(i24) = parse_i24(&a.s) {
                    numeric::Number::I24( i24 )
                } else {
                    return None;
                }
            }
        };

        let n = Number {
            n: a, 
            native_num: num, 
        };

        Some( (n, s) )
    }
}

fn parse_u24(s: &str) -> Option<u32> {
    let val = s.parse::<u32>().ok()?;

    const max_u24: u32 = 1 << 24 - 1;
    if val <= max_u24 {
        Some(val)
    } else {
        None
    }
}

fn parse_i24(s: &str) -> Option<i32> {
    let val = s.parse::<i32>().ok()?;

    const max_i24: i32 = 1 << 23 - 1;
    const min_i24: i32 = -(1 << 23);
    if val >= min_i24 && val <= max_i24 {
        Some(val)
    } else {
        None
    }
}











#[derive(Debug, Clone, PartialEq)]
pub struct Operation {
    pub op: Operator, 
    pub val: u32, 
}

impl Operation {
    pub fn to_string(&self) -> String {
        format!("[{:?}{}]", self.op, self.val)
    }

    fn as_core(&self) -> numeric::Operation {
        numeric::Operation {
            operator: self.op, 
            value: self.val, 
        }
    }

    fn from_core(op: numeric::Operation) -> Self {
        Self {
            op: op.operator, 
            val: op.value, 
        }
    }

    fn parse(s: &[Token]) -> Option<(Self, &[Token])> {
        let s = parse_specific_token(s, &Token::LeftBracket)?;

        let t = if let Token::Operator(op) = s.get(0)? {
            Some( (op.clone(), &s[1..]) )
        } else {
            None
        };

        let (op, s) = t.unwrap();

        let (num, s) = Number::parse(s)?;
        let raw_num = num.native_num.raw_value();

        let s = parse_specific_token(s, &Token::RightBracket)?;

        let oper = Self {
            op: op, 
            val: raw_num, 
        };

        Some( (oper, s) )
    }
}




#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub op: Operator, 
}

impl Symbol {
    pub fn to_string(&self) -> String {
        format!("[{:?}]", self.op)
    }

    /*fn as_core(&self) -> numeric::Operation {
        numeric::Operation {
            operator: self.op, 
            value: self.val, 
        }
    }

    fn from_core(op: numeric::Operation) -> Self {
        Self {
            op: op.operator, 
            val: op.value, 
        }
    }*/

    fn parse(s: &[Token]) -> Option<(Self, &[Token])> {
        let s = parse_specific_token(s, &Token::LeftBracket)?;

        let t = if let Token::Operator(op) = s.get(0)? {
            Some( (op.clone(), &s[1..]) )
        } else {
            None
        };

        let (op, s) = t.unwrap();

        let s = parse_specific_token(s, &Token::RightBracket)?;

        Some( (Self {op: op}, s) )
    }
}







#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct Alphanumeric {
    pub s: String, 
}

impl Alphanumeric {
    pub fn into_string(self) -> String {
        self.s
    }

    pub fn as_str(&self) -> &str {
        self.s.as_str()
    }
}

impl From<String> for Alphanumeric {
    fn from(s: String) -> Self {
        Alphanumeric { s: s }
    }
}

impl From<&str> for Alphanumeric {
    fn from(s: &str) -> Self {
        Alphanumeric { s: s.to_string() }
    }
}

impl Alphanumeric {
    fn parse(s: &[Token]) -> Option<(Self, &[Token])> {
        s.get(0).and_then(|tok| {
            match tok {
                Token::Alphanumeric(text) => {
                    let a = Self {
                        s: text.clone()
                    };
    
                    Some( (a, &s[1..]) )
                }, 
                _ => None, 
            }
        })
    }
}



fn parse_token(s: &[Token]) -> Option<(&Token, &[Token])> {
    s.get(0).map(|tok| (tok, &s[1..]))
}

fn parse_specific_token<'a, 'b>(s: &'a [Token], tok: &'b Token) -> Option<&'a [Token]> {
    let (t, s) = parse_token(s)?;
    if t == tok {
        Some( s )
    } else {
        None
    }
}