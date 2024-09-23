#[derive(Debug, Copy, Clone)]
struct PortID {
    combinator: EntityId, 
    port: u8, 
}

impl PortID {
    pub fn is_primary(&self) -> bool {
        self.port == 0
    }
}

#[derive(Debug, Copy, Clone)]
struct Connection {
    to: Option<PortID>, 
}

#[derive(Debug, Clone)]
struct Nullary {
    primary: Connection, 
}

#[derive(Debug, Clone)]
struct Binary {
    primary: Connection, 
    a: Connection, 
    b: Connection, 
}

enum Either<A, B> {
    Left(A), 
    Right(B), 
}

#[derive(Debug, Clone)]
enum Combinator {
    Root(Nullary), 
    Erase(Nullary), 
    Print(Nullary), 
    Zero(Nullary), 
    One(Nullary), 

    Construct(Binary), 
    Duplicate(Binary), 
}

impl Combinator {
    pub fn get_inner(&self) -> Either<&Nullary, &Binary> {
        match self {
            Self::Root(n) => Either::Left(n), 
            Self::Erase(n) => Either::Left(n), 
            Self::Print(n) => Either::Left(n), 
            Self::Zero(n) => Either::Left(n), 
            Self::One(n) => Either::Left(n), 

            Self::Construct(b) => Either::Right(b), 
            Self::Duplicate(b) => Either::Right(b), 
        }
    }

    pub fn get_inner_mut(&mut self) -> Either<&mut Nullary, &mut Binary> {
        match self {
            Self::Root(n) => Either::Left(n), 
            Self::Erase(n) => Either::Left(n), 
            Self::Print(n) => Either::Left(n), 
            Self::Zero(n) => Either::Left(n), 
            Self::One(n) => Either::Left(n), 

            Self::Construct(b) => Either::Right(b), 
            Self::Duplicate(b) => Either::Right(b), 
        }
    }

    pub fn num_ports(&self) -> u8 {
        match self.get_inner() {
            Either::Left(_) => 1, 
            Either::Right(_) => 3, 
        }
    }

    pub fn get_connection(&self, port: u8) -> Option<&Connection> {
        match self.get_inner() {
            Either::Left(nullary) => {
                if port == 0 {
                    Some(&nullary.primary)
                } else {
                    None
                }
            }, 
            Either::Right(binary) => {
                if port == 0 {
                    Some(&binary.primary)
                } else if port == 1 {
                    Some(&binary.a)
                } else if port == 2 {
                    Some(&binary.b)
                } else {
                    None
                }
            }
        }
    }

    pub fn get_connection_mut(&mut self, port: u8) -> Option<&mut Connection> {
        match self.get_inner_mut() {
            Either::Left(nullary) => {
                if port == 0 {
                    Some(&mut nullary.primary)
                } else {
                    None
                }
            }, 
            Either::Right(binary) => {
                if port == 0 {
                    Some(&mut binary.primary)
                } else if port == 1 {
                    Some(&mut binary.a)
                } else if port == 2 {
                    Some(&mut binary.b)
                } else {
                    None
                }
            }
        }
    }
}

struct ICRuntime {
    combinators: EntityStore<Combinator>, 
    primary_connections: Vec<PortID>
}

impl ICRuntime {
    pub fn new() -> Self {
        ICRuntime {
            combinators: EntityStore::new(), 
            primary_connections: Vec::new(), 
        }
    }
    
    fn handle_interaction(&mut self) -> Option<()> {
        let port_id = self.primary_connections.pop()?;

        let port_id1 = port_id;
        let port_id2 = self.combinators.get_entity(port_id1.combinator)?.get_connection(port_id1.port)?.to.unwrap();

        let combinator1 = self.combinators.get_entity(port_id1.combinator)?;
        let combinator2 = self.combinators.get_entity(port_id2.combinator)?;

        // root node shouldn't interact
        if let Combinator::Root(_) = combinator1 {
            return Some(());
        }

        if let Combinator::Root(_) = combinator2 {
            return Some(());
        }

        let inner1 = combinator1.get_inner();
        let inner2 = combinator2.get_inner();

        match (inner1, inner2) {
            (Either::Left(nullary1), Either::Left(nullary2)) => {self.handle_nullary_interaction(port_id1, port_id2);}, 
            (Either::Left(nullary), Either::Right(binary)) => {self.handle_nullary_binary_interaction(port_id1, port_id2);}, 
            (Either::Right(binary), Either::Left(nullary)) => {self.handle_nullary_binary_interaction(port_id2, port_id1);}, 
            (Either::Right(binary1), Either::Right(binary2)) => {
                match (combinator1, combinator2) {
                    (Combinator::Construct(_), Combinator::Construct(_)) => {self.handle_same_binary_interaction(port_id1, port_id2);}, 
                    (Combinator::Duplicate(_), Combinator::Duplicate(_)) => {self.handle_same_binary_interaction(port_id1, port_id2);}, 
                    (Combinator::Construct(_), Combinator::Duplicate(_)) => {self.handle_different_binary_interaction(port_id1, port_id2);}, 
                    (Combinator::Duplicate(_), Combinator::Construct(_)) => {self.handle_different_binary_interaction(port_id1, port_id2);}, 
                    (_, _) => {panic!();}
                }
            }, 
        };

        Some( () )
    }

    fn handle_nullary_interaction(&mut self, port_id1: PortID, port_id2: PortID) -> Option<()> {
        let combinator1 = self.combinators.remove_entity(port_id1.combinator)?;
        let combinator2 = self.combinators.remove_entity(port_id2.combinator)?;

        // handle output
        if let Combinator::Print(_) = combinator1 {
            match combinator2 {
                Combinator::Zero(_) => {println!("0");}, 
                Combinator::One(_) => {println!("1");}, 
                _ => {}, 
            }
        } else if let Combinator::Print(_) = combinator2 {
            match combinator1 {
                Combinator::Zero(_) => {println!("0");}, 
                Combinator::One(_) => {println!("1");}, 
                _ => {}, 
            }
        };

        Some( () )
    }

    fn handle_nullary_binary_interaction(&mut self, port_id1: PortID, port_id2: PortID) -> Option<()> {
        // remove interacting combinators
        let combinator_nullary = self.combinators.remove_entity(port_id1.combinator)?;
        let combinator_binary = self.combinators.remove_entity(port_id2.combinator)?;

        // duplicate nullary combinator
        let mut nullary1 = combinator_nullary.clone();
        let mut nullary2 = combinator_nullary;

        // get ports connected to ports of the binary combinator
        let port_a = combinator_binary.get_connection(1)?.to?;
        let port_b = combinator_binary.get_connection(2)?.to?;

        // connect the nullaries to the foreign ports
        nullary1.get_connection_mut(0)?.to = Some(port_a);
        nullary2.get_connection_mut(0)?.to = Some(port_b);

        // add the new nullaries
        let nullary1_id = self.combinators.add_entity(nullary1);
        let nullary2_id = self.combinators.add_entity(nullary2);

        // connect the foreign ports to the new nullaries
        self.combinators.get_entity_mut(port_a.combinator)?.get_connection_mut(port_a.port)?.to = Some(PortID {
            combinator: nullary1_id, 
            port: 0, 
        });

        self.combinators.get_entity_mut(port_b.combinator)?.get_connection_mut(port_b.port)?.to = Some(PortID {
            combinator: nullary2_id, 
            port: 0, 
        });

        // discover new primary connections
        if port_a.is_primary() {
            self.primary_connections.push(port_a);
        }

        if port_b.is_primary() {
            self.primary_connections.push(port_b);
        }

        Some( () )
    }

    fn handle_different_binary_interaction(&mut self, port_id1: PortID, port_id2: PortID) -> Option<()> {
        // remove interacting combinators
        let combinator1 = self.combinators.remove_entity(port_id1.combinator)?;
        let combinator2 = self.combinators.remove_entity(port_id2.combinator)?;

        // get the foreign ports
        let port_a = combinator1.get_connection(1)?.to?;
        let port_b = combinator1.get_connection(2)?.to?;

        let port_c = combinator2.get_connection(1)?.to?;
        let port_d = combinator2.get_connection(2)?.to?;

        // create new combinators
        let mut binary1 = combinator2.clone();
        let mut binary2 = combinator2;

        let mut binary3 = combinator1.clone();
        let mut binary4 = combinator1;

        // add the new combinators
        let binary1_id = self.combinators.add_entity(binary1.clone());
        let binary2_id = self.combinators.add_entity(binary2.clone());
        let binary3_id = self.combinators.add_entity(binary3.clone());
        let binary4_id = self.combinators.add_entity(binary4.clone());
        
        // wire the primary ports of the new combinators to the foreign ports
        binary1.get_connection_mut(0)?.to = Some(port_a);
        binary2.get_connection_mut(0)?.to = Some(port_b);
        binary3.get_connection_mut(0)?.to = Some(port_c);
        binary4.get_connection_mut(0)?.to = Some(port_d);

        // wire the secondary ports of the new combinators to each other
        binary1.get_connection_mut(1)?.to = Some(PortID {
            combinator: binary3_id, 
            port: 1, 
        });
        binary1.get_connection_mut(2)?.to = Some(PortID {
            combinator: binary4_id, 
            port: 1, 
        });

        binary2.get_connection_mut(1)?.to = Some(PortID {
            combinator: binary3_id, 
            port: 2, 
        });
        binary2.get_connection_mut(2)?.to = Some(PortID {
            combinator: binary4_id, 
            port: 2, 
        });

        binary3.get_connection_mut(1)?.to = Some(PortID {
            combinator: binary1_id, 
            port: 1, 
        });
        binary3.get_connection_mut(2)?.to = Some(PortID {
            combinator: binary2_id, 
            port: 1, 
        });

        binary4.get_connection_mut(1)?.to = Some(PortID {
            combinator: binary1_id, 
            port: 2, 
        });
        binary4.get_connection_mut(2)?.to = Some(PortID {
            combinator: binary2_id, 
            port: 2, 
        });

        // update the binary entities
        *self.combinators.get_entity_mut(binary1_id)? = binary1;
        *self.combinators.get_entity_mut(binary2_id)? = binary2;
        *self.combinators.get_entity_mut(binary3_id)? = binary3;
        *self.combinators.get_entity_mut(binary4_id)? = binary4;

        // wire the foreign ports to the binaries
        self.combinators.get_entity_mut(port_a.combinator)?.get_connection_mut(port_a.port)?.to = Some(PortID {
            combinator: binary1_id, 
            port: 0, 
        });

        self.combinators.get_entity_mut(port_b.combinator)?.get_connection_mut(port_b.port)?.to = Some(PortID {
            combinator: binary2_id, 
            port: 0, 
        });

        self.combinators.get_entity_mut(port_c.combinator)?.get_connection_mut(port_c.port)?.to = Some(PortID {
            combinator: binary3_id, 
            port: 0, 
        });

        self.combinators.get_entity_mut(port_d.combinator)?.get_connection_mut(port_d.port)?.to = Some(PortID {
            combinator: binary4_id, 
            port: 0, 
        });

        // discover new primary connections
        if port_a.is_primary() {
            self.primary_connections.push(port_a);
        }

        if port_b.is_primary() {
            self.primary_connections.push(port_b);
        }

        if port_c.is_primary() {
            self.primary_connections.push(port_c);
        }

        if port_d.is_primary() {
            self.primary_connections.push(port_d);
        }

        Some( () )
    }

    fn handle_same_binary_interaction(&mut self, port_id1: PortID, port_id2: PortID) -> Option<()> {
        // remove interacting combinators
        let combinator1 = self.combinators.remove_entity(port_id1.combinator)?;
        let combinator2 = self.combinators.remove_entity(port_id2.combinator)?;

        // get the foreign ports
        let port_a = combinator1.get_connection(1)?.to?;
        let port_b = combinator1.get_connection(2)?.to?;

        let port_c = combinator2.get_connection(1)?.to?;
        let port_d = combinator2.get_connection(2)?.to?;

        // wire the foreign ports
        self.combinators.get_entity_mut(port_a.combinator)?.get_connection_mut(port_a.port)?.to = Some(port_c);
        self.combinators.get_entity_mut(port_b.combinator)?.get_connection_mut(port_b.port)?.to = Some(port_d);
        self.combinators.get_entity_mut(port_c.combinator)?.get_connection_mut(port_c.port)?.to = Some(port_a);
        self.combinators.get_entity_mut(port_d.combinator)?.get_connection_mut(port_d.port)?.to = Some(port_b);

        // discover new primary connections
        if port_a.is_primary() && port_c.is_primary() {
            self.primary_connections.push(port_a);
        }

        if port_b.is_primary() && port_d.is_primary() {
            self.primary_connections.push(port_b);
        }

        Some( () )
    }
    
    pub fn add_combinator(&mut self, combinator: Combinator) -> Option<EntityId> {
        let num_connections = combinator.num_ports();

        // ensure all ports to be connected are free
        for i in 0..num_connections {
            let connection = combinator.get_connection(i).unwrap();
            if let Some(port_id) = connection.to {
                if !self.is_port_free(port_id)? {
                    return None;
                }
            }
        }

        // add combinator
        let combinator_id = self.combinators.add_entity(combinator.clone());

        // connect pre-existing combinators to new combinator
        for i in 0..num_connections {
            let connection = combinator.get_connection(i).unwrap();
            if let Some(port_id) = connection.to {
                let port = &mut self.combinators
                    .get_entity_mut(port_id.combinator).unwrap()
                    .get_connection_mut(port_id.port).unwrap().to;

                let to_port = PortID {
                    combinator: combinator_id, 
                    port: i, 
                };

                *port = Some(to_port);

                // discover primary connections
                if to_port.is_primary() && port_id.is_primary() {
                    self.primary_connections.push(to_port);
                }
            }
        }

        Some(combinator_id)
    }

    pub fn wire_free_ports(&mut self, port_id1: PortID, port_id2: PortID) -> Option<()> {
        if self.is_port_free(port_id1)? && self.is_port_free(port_id2)? {
            // connect port_id1 to port_id2
            self.combinators
                .get_entity_mut(port_id1.combinator).unwrap()
                .get_connection_mut(port_id1.port).unwrap()
                .to = Some(port_id2);

            // connect port_id2 to port_id1
            self.combinators
                .get_entity_mut(port_id2.combinator).unwrap()
                .get_connection_mut(port_id2.port).unwrap()
                .to = Some(port_id1);

            // discover primary connections
            if port_id1.is_primary() && port_id2.is_primary() {
                self.primary_connections.push(port_id1);
            }

            Some(())
        } else {
            None
        }
    }

    pub fn is_port_free(&self, id: PortID) -> Option<bool> {
        self.combinators
            .get_entity(id.combinator)
            .and_then(|c| c.get_connection(id.port))
            .map(|c| c.to.is_none())
    }

    pub fn get_connection(&self, id: PortID) -> Option<&Connection> {
        self.combinators
            .get_entity(id.combinator)
            .and_then(|c| c.get_connection(id.port))
    }
}






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

    pub fn get_entity(&self, id: EntityId) -> Option<&T> {
        self.entities.get(id.0)?.as_ref()
    }

    pub fn get_entity_mut(&mut self, id: EntityId) -> Option<&mut T> {
        self.entities.get_mut(id.0)?.as_mut()
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
}








use ic::lexical::*;
use ic::parser::*;

use ic::runtime::Runtime;

//use ic::unrust::parse::{self, Program};

use std::io::{self, Write};

fn main() -> io::Result<()> {
    ////let code = "@main = (x x) @other = (y y)";
    /*let code = "@main = x & (y y) ~ ((z z) x)";

    let expected_result = "@main = (x x)";
    let expected_tokens = lexical_parse(&expected_result).unwrap();
    let expected_program = Program::parse(&expected_tokens[..]).unwrap();
    let expected_node = expected_program.main.net.tree;


    let tokens = lexical_parse(&code).unwrap();
    println!("Tokens: {:?}", tokens);
    let program = Program::parse(&tokens[..]).unwrap();
    println!("Program: {:?}", program);

    let (main, book) = program.to_defs();
    
    let runtime = Runtime::new(&main, book);
    let node = runtime.run();

    println!("Agree?: {}", expected_node.is_equal_to(&node));

    println!("Result: {:?}", node);*/

    /*let unrust_code = "fn main() { 69.0 }";
    let program: Result<parse::Program<parse::Expression>, _> = parse::Program::from_str(&unrust_code);
    let program = match program {
        Ok(program) => program, 
        Err(pe) => {
            println!("{:?}", &unrust_code[pe.index..]);
            println!("{:?}", pe);
            return Err(pe);
        }
    };
    println!("{:?}", program);
    Ok(())*/

    print!("Please enter your code: ");
    io::stdout().flush()?;

    let mut code = String::new();
    io::stdin().read_line(&mut code)?;
    let code = code.trim().to_string();

    let tokens = lexical_parse(&code).unwrap();
    println!("\nTokens: {:?}", tokens);
    let program = Program::parse(&tokens[..]).unwrap();
    println!("\nProgram: {:?}", program);

    let (main, book) = program.to_defs();
    
    let runtime = Runtime::new(&main, book);
    let node = runtime.run();

    println!("\nResult: {:?}", node);

    Ok(())
}






/*fn main() {
    let mut runtime = ICRuntime::new();

    let root = Combinator::Root(
        Nullary {
            primary: Connection {
                to: None, 
            }
        }
    );
    let root_id = runtime.add_combinator(root).unwrap();


    let eraser = Combinator::Erase(
        Nullary {
            primary: Connection {
                to: None, 
            }
        }
    );
    let eraser_id = runtime.add_combinator(eraser).unwrap();

    let con1 = Combinator::Construct(
        Binary {
            primary: Connection {
                to: None, 
            }, 
            a: Connection {
                to: Some(PortID {
                    combinator: eraser_id, 
                    port: 0, 
                })
            }, 
            b: Connection {
                to: None, 
            }
        }
    );
    let con1_id = runtime.add_combinator(con1).unwrap();

    let con2 = Combinator::Construct(
        Binary {
            primary: Connection {
                to: None, 
            }, 
            a: Connection {
                to: Some(PortID {
                    combinator: con1_id, 
                    port: 2, 
                })
            }, 
            b: Connection {
                to: Some(PortID {
                    combinator: con1_id, 
                    port: 0, 
                }), 
            }
        }
    );
    let con2_id = runtime.add_combinator(con2).unwrap();

    let con3 = Combinator::Construct(
        Binary {
            primary: Connection {
                to: Some(PortID {
                    combinator: con2_id, 
                    port: 0, 
                }), 
            }, 
            a: Connection {
                to: None, 
            }, 
            b: Connection {
                to: Some(PortID {
                    combinator: root_id, 
                    port: 0, 
                }), 
            },  
        }
    );
    let con3_id = runtime.add_combinator(con3).unwrap();

    let con4 = Combinator::Construct(
        Binary {
            primary: Connection {
                to: Some(PortID {
                    combinator: con3_id, 
                    port: 1, 
                }), 
            }, 
            a: Connection {
                to: None, 
            }, 
            b: Connection {
                to: None, 
            }
        }
    );
    let con4_id = runtime.add_combinator(con4).unwrap();

    runtime.wire_free_ports(PortID {combinator: con4_id, port: 1}, PortID {combinator: con4_id, port: 2});

    while let Some(_) = runtime.handle_interaction() {
        println!("Performed an interaction!");
    }

    println!("Hello, world!");
}*/
