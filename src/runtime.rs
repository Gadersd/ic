use crate::numeric::{apply_operator, Numeric, Number};
use crate::parser;

pub type Tag = u8;    // 3 bits rounded up to 8
pub type Value = u32; // 29 bits rounded up to 32

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Port(pub u32); // Tag + Value (32 bits)
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Pair(pub u64); // Port + Port (64 bits)

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PortType {
    Eraser, 
    Reference, 
    Numeric, 
    Constructor, 
    Duplicator, 
    Operator, 
    Switch, 
    Variable, 
}

impl PortType {
    pub fn to_tag(self) -> Tag {
        match self {
            PortType::Eraser => 0, 
            PortType::Reference => 1, 
            PortType::Numeric => 2, 
            PortType::Constructor => 3, 
            PortType::Duplicator => 4, 
            PortType::Operator => 5, 
            PortType::Switch => 6, 
            PortType::Variable => 7, 
        }
    }
}

impl Port {
    pub fn is_type(self, pt: PortType) -> bool {
        self.get_tag() == pt.to_tag()
    }

    pub fn from_type_value(pt: PortType, value: Value) -> Self {
        let tag = pt.to_tag();
        Self::from_tag_value(tag, value)
    }

    pub fn get_type(self) -> Option<PortType> {
        let tag = self.get_tag();

        match tag {
            0 => Some(PortType::Eraser), 
            1 => Some(PortType::Reference), 
            2 => Some(PortType::Numeric), 
            3 => Some(PortType::Constructor), 
            4 => Some(PortType::Duplicator), 
            5 => Some(PortType::Operator), 
            6 => Some(PortType::Switch), 
            7 => Some(PortType::Variable), 
            _ => None, 
        }
    }

    pub fn from_tag_value(tag: Tag, val: Value) -> Self {
        Port(tag as u32 | val << 3)
    }

    pub fn get_tag(self) -> Tag {
        (self.0 & 0b111) as Tag
    }

    pub fn get_value(self) -> Value {
        self.0 >> 3
    }

    pub fn is_binary(self) -> bool {
        if let Some(tp) = self.get_type() {
            match tp {
                PortType::Constructor => true, 
                PortType::Duplicator => true, 
                PortType::Operator => true, 
                PortType::Switch => true, 
                _ => false, 
            }
        } else {
            false
        }
    }

    fn get_invalid_port() -> Self {
        Port::from_type_value(PortType::Eraser, 1)
    }
}

impl Pair {
    pub fn from_ports(port_1: Port, port_2: Port) -> Self {
        Pair((port_1.0 as u64) | (port_2.0 as u64) << 32)
    }

    pub fn to_ports(self) -> (Port, Port) {
        let port1 = Port(self.0 as u32);
        let port2 = Port((self.0 >> 32) as u32);
        (port1, port2)
    }
}

#[derive(Clone)]
pub struct Book {
    pub defs: Vec<Def>, 
}

#[derive(Clone)]
pub struct Def {
    pub safe: bool,      // safe means absence of dup nodes
    pub root: Port,      // points to the root node
    pub rbag: Vec<Pair>, // reducible redexes
    pub nodes: Vec<Pair>, 
}

use std::thread;
use std::mem::swap;
use std::sync::RwLock;
use std::sync::atomic::Ordering;

use crate::allocator::*;


//use crate::parser;


pub struct Runtime {
    root: Port, 
    nodes: RwLock<BumpAllocator>, 
    vars: RwLock<BumpAllocator>, 
    subst: Dictionary, 
    rbag_nonref: Stack, 
    rbag_ref: Stack, 
    book: Book, 
}

impl Runtime {
    pub fn new(main: &Def, book: Book) -> Self {
        let n_preallocated_nodes = main.nodes.len() * 2;
        let n_preallocated_vars = 100;
        let n_preallocated_substs = 1000;
        let n_preallocated_redexes = main.rbag.len() * 2;

        let root = main.root;

        let nodes = BumpAllocator::new(n_preallocated_nodes);
        let vars = BumpAllocator::new(n_preallocated_vars);
        let subst = Dictionary::new(n_preallocated_substs, Port::get_invalid_port().0 as u64); // default to invalid
        //let rbag = Stack::new(n_preallocated_redexes);
        let rbag_nonref = Stack::new(n_preallocated_redexes);
        let rbag_ref = Stack::new(n_preallocated_redexes);

        // copy over nodes
        nodes.allocate_u64s_from_iter(main.nodes.iter().map(|pair| pair.0)).unwrap();

        // set var ids
        let n_var_ids = main.nodes.iter().flat_map(|node| {
            let (port_1, port_2) = node.to_ports();
            [port_1, port_2].into_iter().filter_map(|port| {
                if port.is_type(PortType::Variable) {
                    Some(port.get_value())
                } else {
                    None
                }
            })
        }).max().map(|id| id + 1).unwrap_or(0);
        vars.allocate_u64s(n_var_ids as usize).unwrap();

        // copy over redexes
        for r in &main.rbag {
            //rbag.push(r.0);
            Self::add_redex(&rbag_nonref, &rbag_ref, *r);
        }

        Runtime {
            root: root, 
            nodes: nodes.into(), 
            vars: vars.into(), 
            subst: subst, 
            rbag_nonref, 
            rbag_ref, 
            //rbag: rbag, 
            book: book, 
        }
    }

    fn add_redex(rbag_nonref: &Stack, rbag_ref: &Stack, redex: Pair) {
        let (port_1, port_2) = redex.to_ports();
        if port_1.is_type(PortType::Reference) || port_2.is_type(PortType::Reference) {
            rbag_ref.push(redex.0);
        } else {
            rbag_nonref.push(redex.0);
        }
    }

    /*pub fn run_steps(&self) {
        //let mut nodes = Vec::new();

        let clone = || {
            Runtime {
                root: self.root, 
                nodes: BumpAllocator::from_slice(&self.nodes.read().unwrap().to_vec()[..]).into(), 
                vars: BumpAllocator::from_slice(&self.vars.read().unwrap().to_vec()[..]).into(), 
                subst: Dictionary::from_slice(&self.subst.to_vec()[..], self.subst.get_default()), 
                rbag_nonref: Stack::from_slice(&self.rbag_nonref.to_vec()[..]), 
                rbag_ref: Stack::from_slice(&self.rbag_ref.to_vec()[..]), 
                book: self.book.clone(), 
            }
        };

        for i in 3..18 {
            let self_clone = clone();
            let (node, is_done) = self_clone.run(i);
            //println!("Node: {:?}", node);


            /*let main_def = Def {
                safe: true,      // safe means absence of dup nodes
                root: self_clone.root,      // points to the root node
                rbag: self_clone.rbag_nonref.to_vec().into_iter().chain(self_clone.rbag_ref.to_vec()).map(|u| Pair(u)).collect(), // reducible redexes
                nodes: self_clone.nodes.read().unwrap().to_vec().into_iter().map(|u| Pair(u)).collect(), 
            };

            let program = parser::Program::from_core_books(&main_def, &self_clone.book);
            println!("Program: {:?}", program.map(|program| program.main.to_string()));*/

            println!("Node: {:?}", node);


            if is_done {
                break;
            }
        }
    }*/

    pub fn run(&self/*, itrs: usize*/) -> parser::Node /*(parser::Node, bool)*/ {
        let concurrent_nodes = EntityStore::new(&self.nodes);
        let concurrent_vars = EntityStore::new(&self.vars);

        let num_cores = 1;//num_cpus::get();


        println!("{} cores", num_cores);

        /*let mut net = Net {
            nodes: concurrent_nodes.clone(), 
            vars: concurrent_vars.clone(), 
            subst: &self.subst, 
            rbag: &self.rbag, 
            book: &self.book, 
        };

        while net.reduce().unwrap() {}*/

        let subst_var = |mut port: Port| {
            while port.is_type(PortType::Variable) {
                //println!("port tag {} value {}", port.get_tag(), port.get_value());
                let new_port = Port(self.subst.get(port.get_value() as usize) as u32);
                //println!("New port tag {} value {}", new_port.get_tag(), new_port.get_value());
                if new_port != Port::get_invalid_port() {
                    port = new_port;
                } else {
                    break;
                }
            }
            port
        };

        /*let mut is_done = false;
        let jk = &mut is_done;*/

        thread::scope(|s| {
            for thread_idx in 0..num_cores {
                let concurrent_nodes = concurrent_nodes.clone();
                let concurrent_vars = concurrent_vars.clone();

                s.spawn(move || {
                    let mut net = Net {
                        nodes: concurrent_nodes, 
                        vars: concurrent_vars, 
                        subst: &self.subst, 
                        //rbag: &self.rbag, 
                        rbag_nonref: &self.rbag_nonref, 
                        rbag_ref: &self.rbag_ref, 
                        book: &self.book, 
                    };

                    while net.reduce().unwrap() {}

                    /*for i in 0..itrs {
                        if !net.reduce().unwrap() {
                            //is_done = true;
                            *jk = true;
                            break;
                        }
                    }*/

                    /*while i < itrs && net.reduce().unwrap() {
                        i += 1;
                        /*let main_def = Def {
                            safe: true,      // safe means absence of dup nodes
                            root: self.root,      // points to the root node
                            rbag: self.rbag_nonref.to_vec().into_iter().chain(self.rbag_ref.to_vec()).map(|u| Pair(u)).collect(), // reducible redexes
                            nodes: self.nodes.read().unwrap().to_vec().into_iter().map(|u| Pair(u)).collect(), 
                        };

                        let program = parser::Program::from_core_books(&main_def, &self.book);
                        println!("Program: {:?}", program.map(|program| program.main));*/
                        /*let root = self.root;//subst_var(self.root);

                        // convert nodes to abstract syntax tree
                        let nodes: Vec<Pair> = self.nodes.read().unwrap().iter().map(|atomic| Pair(atomic.load(Ordering::Relaxed))).collect();
                        /*for n in &nodes {
                            let (port_1, port_2) = n.to_ports();
                            println!("Tag: {} Value: {}\n", port_1.get_tag(), port_1.get_value());
                        }*/
                        let tree = parser::Book::from_nodes("main".into(), root, &nodes[..], &[]).unwrap().net.tree;
                        println!("Tree {:?}", tree);*/
                    };*/

                    // perform remaining variable substitutions
                    let nodes = self.nodes.read().unwrap();
                    let num_nodes = nodes.amount_used();
                    let nodes_per_thread = (num_nodes + num_cores - 1) / num_cores;

                    let start_idx = thread_idx * nodes_per_thread;
                    let end_idx = (start_idx + nodes_per_thread).min(num_nodes);

                    for i in start_idx..end_idx {
                        let pair = Pair(nodes.get(i).unwrap());
                        let (port_1, port_2) = pair.to_ports();

                        let (port_1, port_2) = (subst_var(port_1), subst_var(port_2));

                        nodes.set(i, Pair::from_ports(port_1, port_2).0);
                    }
                });
            }
        });

        // subst vars for root
        let root = subst_var(self.root);
        /*for (i, val) in self.subst.to_vec().into_iter().enumerate() {
            let port = Port(val as u32);
            println!("i {} tag {} addr {}", i, port.get_tag(), port.get_value());
        }*/
        //println!("root: {} {}", self.root.get_value(), root.get_value());

        // convert nodes to abstract syntax tree
        let nodes: Vec<Pair> = self.nodes.read().unwrap().iter().map(|atomic| Pair(atomic.load(Ordering::Relaxed))).collect();
        /*for n in &nodes {
            let (port_1, port_2) = n.to_ports();
            println!("Tag: {} Value: {}\n", port_1.get_tag(), port_1.get_value());
        }*/
        let tree = parser::Book::from_nodes("main".into(), root, &nodes[..], &[]).unwrap().net.tree;

        //println!("Done!!");
        tree
        //(tree, is_done)
    }
}


struct Net<'a> {
    nodes: EntityStore<'a>, 
    vars: EntityStore<'a>, 
    subst: &'a Dictionary, 
    //rbag: &'a Stack, 
    rbag_nonref: &'a Stack, 
    rbag_ref: &'a Stack, 
    book: &'a Book, 
}

impl<'a> Net<'a> {
    pub fn reduce(&mut self) -> Option<bool> {
        let redex = match self.pop_redex() {
            Some(r) => r, 
            None => return Some(false), // all work is done, 
        };
        /*let redex = match self.rbag.pop() {
            Some(r) => Pair(r), 
            None => return Some(false), // all work is done
        };*/
            
        let (port_1, port_2) = redex.to_ports();
        let (tag_1, tag_2) = (port_1.get_tag(), port_2.get_tag());

        //let program = parser::Program::from_core_books()

        //println!("Port1: tag {} val: {} Port 2 tag {} val {}", port_1.get_tag(), port_1.get_value(), port_2.get_tag(), port_2.get_value());
        /*let node_a = if port_1.is_binary() {
            Pair(self.nodes.get_entity(EntityId(port_a.get_value() as usize)))
        } else {
            port_1
        };
        let node_b = if port_2.is_binary() {
            Pair(self.nodes.get_entity(EntityId(port_b.get_value() as usize)))
        } else {
            port_2
        };*/
        /*if port_1.is_type(PortType::Numeric) {
            println!("Port1: type {:?} val: {:?}", port_1.get_type(), Numeric::from_value(port_1.get_value()).unwrap());
        }
        if port_2.is_type(PortType::Numeric) {
            println!("Port 2 type {:?} val {:?}", port_2.get_type(), Numeric::from_value(port_2.get_value()).unwrap());
        }
        {
            println!("Port1: type {:?} val: {} Port 2 type {:?} val {}\n\n", port_1.get_type(), port_1.get_value(), port_2.get_type(), port_2.get_value());
        }*/
        

        const call: u8 = 0;
        const void: u8 = 1;
        const erase: u8 = 2;
        const commute: u8 = 3;
        const annihilate: u8 = 4;
        const operator: u8 = 5;
        const switch: u8 = 6;
        const link: u8 = 7; // I think this is impossible

        const dispatch_table: [[u8; 8]; 8] = [
            // eraser, reference, numeric, constructor, duplicator, operator, switch, variable
            /*eraser*/ [void, void, void, erase, erase, erase, erase, link], 
            /*reference*/ [void, void, void, call, erase, call, call, link], 
            /*numeric*/ [void, void, void, erase, erase, operator, switch, link], 
            /*constructor*/ [erase, call, erase, annihilate, commute, commute, commute, link], 
            /*duplicator*/ [erase, erase, erase, commute, annihilate, commute, commute, link], 
            /*operator*/ [erase, call, operator, commute, commute, annihilate, commute, link], 
            /*switch*/ [erase, call, switch, commute, commute, commute, annihilate, link], 
            /*variable*/ [link, link, link, link, link, link, link, link], 
        ];

        let function = dispatch_table[tag_1 as usize][tag_2 as usize];

        //println!("Function: {}", function);

        match function {
            call => self.reduce_call(redex)?, 
            void => self.reduce_void(redex)?, 
            erase => self.reduce_erase(redex)?, 
            commute => self.reduce_commute(redex)?, 
            annihilate => self.reduce_annihilate(redex)?, 
            operator => self.reduce_operator(redex)?, 
            switch => self.reduce_switch(redex)?, 
            link => self.link(port_1, port_2)?, 
            _ => panic!(), 
        }

        Some(true)
    }

    fn pop_redex(&self) -> Option<Pair> {
        match self.rbag_nonref.pop() {
            Some(r) => Some(Pair(r)), 
            None => match self.rbag_ref.pop() {
                Some(r) => {
                    /*let (port_1, port_2) = Pair(r).to_ports();
                    let port = if port_1.is_type(PortType::Reference) {
                        port_2
                    } else {
                        port_1
                    };
                    let node = Pair(self.nodes.get_entity(EntityId(port.get_value() as usize)).unwrap());
                    let (port_a, port_b) = node.to_ports();
                    println!("Port a: {:?} port b: {:?}", port_a.get_type().unwrap(), port_b.get_type().unwrap());
                    if port_a.is_type(PortType::Eraser) && port_b.is_type(PortType::Eraser) {
                        return None;
                    }
                    println!("Reference! the bastard!"); */
                    Some(Pair(r))
                }, 
                None => None, // all work is done, 
            }
        }
    }

    fn reduce_call(&mut self, redex: Pair) -> Option<()> {
        //println!("Start Call");
        // get addresses of interacting nodes
        let (port_1, port_2) = redex.to_ports();
        let (port_reference, port_2) = if port_1.is_type(PortType::Reference)  {
            (port_1, port_2)
        } else {
            (port_2, port_1)
        };
        let (tag_1, tag_2) = (port_1.get_tag(), port_2.get_tag());
        let reference_idx = port_reference.get_value();

        // remove interacting nodes

        // get foreign ports

        // add definition
        //println!("Start expand");
        let port_def = self.expand_definition(reference_idx as usize)?;
        //println!("End expand");

        // create new vars

        // creates new nodes with vars

        // add new nodes

        // form links between foreign ports and new ports
        self.link(port_def, port_2)?;

        //println!("End Call");

        Some(())
    }

    fn reduce_void(&mut self, redex: Pair) -> Option<()> {
        Some(())
    }

    fn reduce_erase(&mut self, redex: Pair) -> Option<()> {
        // get addresses of interacting nodes
        let (port_1, port_2) = redex.to_ports();
        let (port_nullary, port_binary) = if port_1.is_binary() {
            (port_2, port_1)
        } else {
            (port_1, port_2)
        };
        let tag_binary = port_binary.get_tag();
        let address_binary = port_binary.get_value();

        // call optimization if safe to do so
        if port_nullary.is_type(PortType::Reference) {
            let reference_idx = port_nullary.get_value();
            if self.book.defs[reference_idx as usize].safe {
                return self.reduce_call(redex);
            }
        }

        // remove interacting nodes
        let node_binary = Pair(self.nodes.remove_entity(EntityId(address_binary as usize))?);

        // get foreign ports
        let (port_a, port_b) = node_binary.to_ports();

        // create new vars

        // creates new nodes with vars

        // add new nodes

        // form links between foreign ports and new ports
        self.link(port_a, port_nullary)?;
        self.link(port_b, port_nullary)?;

        Some(())
    }

    fn reduce_commute(&mut self, redex: Pair) -> Option<()> {
        // get addresses of interacting nodes
        let (port_1, port_2) = redex.to_ports();
        let (tag_1, tag_2) = (port_1.get_tag(), port_2.get_tag());
        let (address_1, address_2) = (port_1.get_value(), port_2.get_value());

        // remove interacting nodes
        let node_1 = Pair(self.nodes.remove_entity(EntityId(address_1 as usize))?);
        let node_2 = Pair(self.nodes.remove_entity(EntityId(address_2 as usize))?);

        // get foreign ports
        let (port_a, port_b) = node_1.to_ports();
        let (port_c, port_d) = node_2.to_ports();

        // create new vars
        let x = self.create_var()?;
        let y = self.create_var()?;
        let w = self.create_var()?;
        let z = self.create_var()?;

        // creates new nodes with vars
        let node_1 = Pair::from_ports(x, y);
        let node_2 = Pair::from_ports(z, w);
        let node_3 = Pair::from_ports(x, z);
        let node_4 = Pair::from_ports(y, w);

        // add new nodes
        let port_1 = self.add_node(tag_2, node_1)?;
        let port_2 = self.add_node(tag_2, node_2)?;
        let port_3 = self.add_node(tag_1, node_3)?;
        let port_4 = self.add_node(tag_1, node_4)?;

        // form links between foreign ports and new ports
        self.link(port_a, port_1)?;
        self.link(port_b, port_2)?;
        self.link(port_c, port_3)?;
        self.link(port_d, port_4)?;

        Some(())
    }

    fn reduce_annihilate(&mut self, redex: Pair) -> Option<()> {
        // get addresses of interacting nodes
        let (port_1, port_2) = redex.to_ports();
        let tag = port_1.get_tag(); // tags should be the same
        let (address_1, address_2) = (port_1.get_value(), port_2.get_value());

        // remove interacting nodes
        let node_1 = Pair(self.nodes.remove_entity(EntityId(address_1 as usize))?);
        let node_2 = Pair(self.nodes.remove_entity(EntityId(address_2 as usize))?);

        // get foreign ports
        let (port_a, port_b) = node_1.to_ports();
        let (port_c, port_d) = node_2.to_ports();

        //println!("Annihilate! Start!");

        // create new vars
        
        // creates new nodes with vars

        // add new nodes

        // form links between foreign ports and new ports
        self.link(port_a, port_c)?;
        //println!("Annihilate! Start!");
        self.link(port_b, port_d)?;
        //println!("Annihilate! Start!");

        Some(())
    }

    fn reduce_operator(&mut self, redex: Pair) -> Option<()> {
        // get addresses of interacting nodes
        let (port_1, port_2) = redex.to_ports();
        let (port_numeric, port_operator) = if port_2.is_type(PortType::Operator) {
            (port_1, port_2)
        } else {
            (port_2, port_1)
        };
        let tag_operator = port_operator.get_tag();
        let (tag_1, tag_2) = (port_1.get_tag(), port_2.get_tag());
        let address_operator = port_operator.get_value();
        let (address_1, address_2) = (port_1.get_value(), port_2.get_value());

        // remove interacting nodes
        let node_operator = Pair(self.nodes.remove_entity(EntityId(address_operator as usize))?);

        // get foreign ports
        let (port_m, port_a) = node_operator.to_ports();

        if port_m.is_type(PortType::Numeric) {
            // create new vars

            // creates new nodes with vars

            // add new nodes
            let port_op = apply_operator(port_numeric, port_m);

            // form links between foreign ports and new ports
            self.link(port_op, port_a)?;
        } else {
            let (port_a, port_b) = (port_m, port_a);

            // create new vars

            // creates new nodes with vars
            let node_1 = Pair::from_ports(port_numeric, port_b);

            // add new nodes
            let port_1 = self.add_node(tag_operator, node_1)?;

            // form links between foreign ports and new ports
            self.link(port_a, port_1)?;

        }

        Some(())
    }

    fn reduce_switch(&mut self, redex: Pair) -> Option<()> {
        // get addresses of interacting nodes
        let (port_1, port_2) = redex.to_ports();
        let (port_numeric, port_switch) = if port_2.is_type(PortType::Switch) {
            (port_1, port_2)
        } else {
            (port_2, port_1)
        };
        let (tag_1, tag_2) = (port_1.get_tag(), port_2.get_tag());
        let address_switch = port_switch.get_value();
        let (address_1, address_2) = (port_1.get_value(), port_2.get_value());

        // remove interacting nodes
        let node_1 = Pair(self.nodes.remove_entity(EntityId(address_switch as usize))?);

        // get foreign ports
        let (port_a, port_b) = node_1.to_ports();

        let numeric = Numeric::from_value(port_numeric.get_value())?;
        if let Numeric::Number(n) = numeric {

            let num: i64 = match n {
                Number::U24(n) => n.into(), 
                Number::I24(n) => n.into(), 
                _ => {return None;}
            };

            if num == 0 {
                // creates new nodes with vars
                let node_1 = Pair::from_ports(port_b, Port::from_type_value(PortType::Eraser, 0));

                // add new nodes
                let port_1 = self.add_node(3, node_1)?;

                // form links between foreign ports and new ports
                self.link(port_a, port_1)?;
            } else {
                // create one less numeric port
                let numeric_1_less = match n {
                    Number::U24(n) => Numeric::Number(Number::U24(n - 1)), 
                    Number::I24(n) => Numeric::Number(Number::I24(n - 1)), 
                    _ => panic!(), 
                };
                let port_numeric_1_less = Port::from_type_value(PortType::Numeric, numeric_1_less.to_value());

                // creates new nodes with vars
                let node_1 = Pair::from_ports(port_numeric_1_less, port_b);
                let port_1 = self.add_node(3, node_1)?;

                let node_2 = Pair::from_ports(Port::from_type_value(PortType::Eraser, 0), port_1);
                let port_2 = self.add_node(3, node_2)?;

                // form links between foreign ports and new ports
                self.link(port_a, port_2)?;
            }

            Some(())
        } else {
            None
        }
    }

    fn link(&self, mut port_1: Port, mut port_2: Port) -> Option<()> {
        loop {
            if !port_1.is_type(PortType::Variable) {
                swap(&mut port_1, &mut port_2);
            }

            //println!("WTF");

            if !port_1.is_type(PortType::Variable) {
                //println!("WTF");
                // neither are variables, hence they are principal ports
                self.add_redex(Pair::from_ports(port_1, port_2));
                //self.rbag.push(Pair::from_ports(port_1, port_2).0);
                return Some(());
            }

            //println!("WTF");

            // port_1 is a variable
            let var_idx = port_1.get_value() as usize;
            //println!("WHY????");
            let orig_subst = self.fetch_set_substitution(var_idx, port_2);

            //println!("WTF");

            if let Some(orig_subst) = orig_subst {
                port_1 = orig_subst;
                self.remove_substitution(var_idx);
            } else {
                break;
            }
        }

        Some(())
    }

    fn remove_substitution(&self, var_idx: usize) {
        self.subst.set(var_idx, Port::get_invalid_port().0.into());
    }

    fn fetch_set_substitution(&self, var_idx: usize, to: Port) -> Option<Port> {
        //println!("Please                        var idx {}", var_idx);
        let original = self.subst.swap(var_idx, to.0.into());
        //println!("Swapped");
        if original != Port::get_invalid_port().0 as u64 {
            Some(Port(original as u32))
        } else {
            None
        }
    }

    fn add_node(&mut self, tag: Tag, node: Pair) -> Option<Port> {
        let id = self.nodes.add_entity(node.0);
        Value::try_from(id.0).ok().map(|value| 
            Port::from_tag_value(tag, value)
        )
    }

    fn add_redex(&self, redex: Pair) {
        let (port_1, port_2) = redex.to_ports();
        if port_1.is_type(PortType::Reference) || port_2.is_type(PortType::Reference) {
            //println!("WTF");
            self.rbag_ref.push(redex.0);
        } else {
            //println!("WTF");
            self.rbag_nonref.push(redex.0);
        }
    }

    fn expand_definition(&mut self, idx: usize) -> Option<Port> {
        let def = self.book.defs.get(idx)?;
        /*let mut rbag = def.rbag.clone();
        let mut nodes = def.nodes.clone();*/

        // copy nodes
        let (start_id, end_id) = self.nodes.add_entities_contiguous(def.nodes.iter().map(|pair| pair.0));

        let max_var_id: i64 = def.nodes.iter().map(|pair| {
            let (port_1, port_2) = pair.to_ports();
            let var1 = if port_1.is_type(PortType::Variable) {
                port_1.get_value() as i64
            } else {
                -1
            };
            let var2 = if port_2.is_type(PortType::Variable) {
                port_2.get_value() as i64
            } else {
                -1
            };

            var1.max(var2)
        }).max().unwrap_or(-1);
        let max_var_id = if max_var_id >= 0 {
            Some(max_var_id as usize)
        } else {
            None
        };

        // create vars
        let starting_new_var_id = if let Some(max_var_id) = max_var_id {
            let num_vars = max_var_id + 1;
            Some( self.vars.add_entities_contiguous((0..num_vars).into_iter().map(|_| 0)).0.0 as u32 )
        } else {
            None
        };

        let update_port = |port: Port| {
            let tag = port.get_tag();
            if port.is_binary() {
                let new_value = port.get_value() + start_id.0 as u32;
                Port::from_tag_value(tag, new_value)
            } else if port.is_type(PortType::Variable) {
                let var_id = port.get_value();
                let new_value = var_id + starting_new_var_id.unwrap();
                Port::from_tag_value(tag, new_value)
            } else {
                port
            }
        };

        // update ports
        for id in start_id.0..end_id.0 {
            let node = Pair(self.nodes.get_entity(EntityId(id))?);
            let (port_1, port_2) = node.to_ports();
            let (port_1, port_2) = (update_port(port_1), update_port(port_2));
            self.nodes.set_entity(EntityId(id), Pair::from_ports(port_1, port_2).0)?;
        }

        // add redexes
        for r in &def.rbag {
            let (port_1, port_2) = r.to_ports();
            let (port_1, port_2) = (update_port(port_1), update_port(port_2));
            self.add_redex(Pair::from_ports(port_1, port_2));
            //self.rbag.push(Pair::from_ports(port_1, port_2).0);
        }

        Some( update_port(def.root) )
    }

    fn create_var(&mut self) -> Option<Port> {
        let var = self.vars.add_entity(0);
        Value::try_from(var.0).ok().map(|value| 
            Port::from_type_value(PortType::Variable, value)
        )

        /*let var = self.vars.add_entity(());
        Value::try_from(var.0).ok().map(|value| 
            Port::from_tag_value(7, value)
        )*/
    }
}











/*
#[derive(Debug, Copy, Clone)]
struct EntityId(usize);

struct EntityStore<T> {
    entities: Vec<Option<T>>, 
    free_entities: Vec<usize>, 
}

impl<T> EntityStore<T> {
    pub fn new() -> Self {
        Self {
            entities: Vec::new(), 
            free_entities: Vec::new(), 
        }
    }

    pub fn num_entities(&self) -> usize {
        self.entities.len() - self.free_entities
    }

    pub fn get_entity(&self, id: EntityId) -> Option<&T> {
        self.entities.get(id.0)?.as_ref()
    }

    pub fn get_entity_mut(&mut self, id: EntityId) -> Option<&mut T> {
        self.entities.get_mut(id.0)?.as_mut()
    }

    pub fn next_contiguous_id(&self) -> EntityId {
        EntityId(self.entities.len())
    }

    pub fn add_entities_contiguous(&mut self, xs: &[T]) -> (EntityId, EntityId) {
        let start_id = self.entities.len();
        let end_id = start_id + xs.len();

        self.entities.extend_from_slice(xs);

        (EntityId(start_id), EntityId(end_id))
    }

    pub fn add_entity(&mut self, x: T) -> EntityId {
        if let Some(id) = self.free_entities.pop() {
            self.entities[id] = Some(x);
            EntityId(id)
        } else {
            let id = self.entities.len();
            self.entities.push(Some(x));
            EntityId(id)
        }
    }

    pub fn remove_entity(&mut self, id: EntityId) -> Option<T> {
        if !self.is_entity_free(id)? {
            self.free_entities.push(id.0);
            let entity = std::mem::replace(&mut self.entities[id.0], None);
            entity
        } else {
            None
        }
    }

    pub fn is_entity_free(&self, id: EntityId) -> Option<bool> {
        let id = id.0;
        if id < self.entities.len() {
            Some( self.entities[id].is_none() )
        } else {
            None
        }
    }
}*/






