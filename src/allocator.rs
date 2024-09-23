use std::sync::RwLock;
use std::sync::atomic::Ordering;
use std::sync::atomic::{AtomicU64, AtomicUsize};

use std::iter;

pub type MemId = usize;

pub struct BumpAllocator {
    data: Vec<AtomicU64>, 
    amount_used: AtomicUsize, 
    amount_desired: AtomicUsize, 
}

impl BumpAllocator {
    pub fn new(n_preallocated: usize) -> Self {
        Self {
            data: (0..n_preallocated).into_iter().map(|_| AtomicU64::new(0)).collect(), 
            amount_used: 0.into(), 
            amount_desired: 0.into(), 
        }
    }

    pub fn from_slice(s: &[u64]) -> Self {
        Self {
            data: s.into_iter().map(|&x| x.into()).collect(), 
            amount_used: s.len().into(), 
            amount_desired: s.len().into(), 
        }
    }

    pub fn iter(&self) -> impl Iterator<Item=&AtomicU64> + Clone + ExactSizeIterator {
        let n = self.amount_used();
        self.data.iter().take(n)
    }

    pub fn to_vec(&self) -> Vec<u64> {
        self.data.iter().map(|atomic| atomic.load(Ordering::Relaxed)).collect()
    }

    /*pub fn get(&self, id: MemId) -> Option<&AtomicU64> {
        self.data.get(id)
    }*/

    pub fn swap(&self, id: MemId, val: u64) -> Option<u64> {
        self.data.get(id).map(|atomic| atomic.swap(val, Ordering::Relaxed))
    }

    pub fn get(&self, id: MemId) -> Option<u64> {
        Some( self.data.get(id)?.load(Ordering::Relaxed) )
    }

    pub fn set(&self, id: MemId, val: u64) -> Option<()> {
        self.data.get(id)?.store(val, Ordering::Relaxed);
        Some(())
    }

    pub fn amount_used(&self) -> usize {
        self.amount_used.load(Ordering::Relaxed)
    }

    pub fn allocate_u64s(&self, num: usize) -> Option<(MemId, MemId)> {
        self.amount_desired.fetch_add(num, Ordering::Relaxed);

        let amount_used_orig = self.amount_used.fetch_add(num, Ordering::Relaxed);
        let amount_used = amount_used_orig + num;

        if amount_used <= self.data.len() {
            let start_id = amount_used_orig;
            let end_id = amount_used;
            Some( (start_id, end_id) )
        } else {
            self.amount_used.fetch_sub(num, Ordering::Relaxed);
            None
        }
    }

    pub fn allocate_u64s_from_iter(&self, vals: impl Iterator<Item=u64> + ExactSizeIterator) -> Option<(MemId, MemId)> {
        let (start_id, end_id) = self.allocate_u64s(vals.len())?;
        for (i, val) in vals.enumerate() {
            self.data[start_id + i].store(val, Ordering::Relaxed);
        }

        Some( (start_id, end_id) )
    }

    pub fn reallocate(&mut self) {
        if self.amount_desired.load(Ordering::Relaxed) >= self.data.len() {
            //let new_len = self.data.len() * 2;
            let additional = self.data.len().max(100);
            self.data = self.data.iter()
                .map(|a| AtomicU64::new(a.load(Ordering::Relaxed)))
                .chain((0..additional).into_iter().map(|_| AtomicU64::new(0)))
                .collect();
        }
    }
}



pub struct Dictionary {
    allocator: RwLock<BumpAllocator>, 
    default: u64, 
}


impl Dictionary {
    pub fn new(n_preallocated: usize, default: u64) -> Self {
        let allocator = BumpAllocator::new(n_preallocated);
        allocator.allocate_u64s_from_iter((0..n_preallocated).into_iter().map(|_| default));

        Self {
            allocator: allocator.into(), 
            default: default, 
        }
    }

    pub fn to_vec(&self) -> Vec<u64> {
        self.allocator.read().unwrap().to_vec()
    }

    pub fn from_slice(s: &[u64], default: u64) -> Self {
        Self {
            allocator: BumpAllocator::from_slice(s).into(), 
            default: default, 
        }
    }

    pub fn swap(&self, idx: usize, value: u64) -> u64 {
        let v = self.allocator.read().unwrap().swap(idx, value);
        if let Some(v) = v {
            v
        } else {
            self.set(idx, value);
            self.default
        }
    }

    pub fn get_default(&self) -> u64 {
        self.default
    }

    pub fn get(&self, idx: usize) -> u64 {
        self.allocator.read().unwrap().get(idx).unwrap_or(self.default)
    }

    pub fn set(&self, idx: usize, value: u64) {
        loop {
            let extra = (idx + 1).saturating_sub( self.allocator.read().unwrap().amount_used() );
            let id_range = self.allocator.read().unwrap().allocate_u64s_from_iter((0..extra).into_iter().map(|_| self.default));
            if let Some( (start_id, end_id) ) = id_range {
                self.allocator.read().unwrap().set(end_id - 1, value);
                return;
            } else {
                // need to allocate more memory
                self.allocator.write().unwrap().reallocate();
            }
        }
    }
}


pub struct Stack {
    allocator: RwLock<BumpAllocator>, 
    num_elements: AtomicUsize, 
    next_element: AtomicUsize, 
}

impl Stack {
    pub fn new(n_preallocated: usize) -> Self {
        Self {
            allocator: BumpAllocator::new(n_preallocated).into(), 
            num_elements: 0.into(), 
            next_element: 0.into(), 
        }
    }

    pub fn from_slice(s: &[u64]) -> Self {
        Self {
            allocator: BumpAllocator::from_slice(s).into(), 
            num_elements: s.len().into(), 
            next_element: 0.into(), 
        }
    }

    pub fn to_vec(&self) -> Vec<u64> {
        let start = self.next_element.load(Ordering::Relaxed);
        let end = self.num_elements.load(Ordering::Relaxed);

        self.allocator.read().unwrap().to_vec()[start..end].to_vec()
    }

    pub fn push(&self, val: u64) {
        loop {
            let id_range = self.allocator.read().unwrap().allocate_u64s_from_iter(iter::once(val));
            if let Some( (start_id, end_id) ) = id_range {
                self.num_elements.fetch_add(1, Ordering::Relaxed);
                return;
            } else {
                // need to allocate more memory
                self.allocator.write().unwrap().reallocate();
            }
        }
    }

    pub fn pop(&self) -> Option<u64> {
        loop {
            let next_element = self.next_element.load(Ordering::Relaxed);
            let num_elements = self.num_elements.load(Ordering::Relaxed);
    
            if next_element < num_elements {
                if let Ok(_) = self.next_element.compare_exchange(next_element, next_element + 1, Ordering::Relaxed, Ordering::Relaxed) {
                    return self.allocator.read().unwrap().get(next_element);
                } else {
                    continue;
                }
            } else {
                return None;
            }
        }
    }
}


pub struct EntityId(pub usize);

#[derive(Clone)]
pub struct EntityStore<'a> {
    allocator: &'a RwLock<BumpAllocator>, 
    free_ids: Vec<MemId>, 
}

impl<'a> EntityStore<'a> {
    pub fn new(allocator: &'a RwLock<BumpAllocator>) -> Self {
        Self {
            allocator: allocator, 
            free_ids: Vec::new(), 
        }
    }

    /*pub fn clone(&self) -> Self {
        Self {
            allocator: BumpAllocator::from_slice(&self.allocator.to_vec()[..]).into(), 
            free_ids: self.free_ids.clone(), 
        }
    }*/

    pub fn num_entities(&self) -> usize {
        self.allocator.read().unwrap().amount_used() - self.free_ids.len()
    }

    pub fn get_entity(&self, id: EntityId) -> Option<u64> {
        self.allocator.read().unwrap().get(id.0)
    }

    pub fn set_entity(&self, id: EntityId, val: u64) -> Option<()> {
        self.allocator.read().unwrap().set(id.0, val)
    }

    /*pub fn get_entity(&self, id: EntityId) -> Option<&T> {
        self.entities.get(id.0)?.as_ref()
    }

    pub fn get_entity_mut(&mut self, id: EntityId) -> Option<&mut T> {
        self.entities.get_mut(id.0)?.as_mut()
    }*/

    /*pub fn next_contiguous_id(&self) -> EntityId {
        EntityId(self.allocator.read().amount_used())
    }*/

    pub fn add_entities_contiguous(&self, xs: impl Iterator<Item=u64> + Clone + ExactSizeIterator) -> (EntityId, EntityId) {
        loop {
            let id_range = self.allocator.read().unwrap().allocate_u64s_from_iter(xs.clone());
            if let Some( (start_id, end_id) ) = id_range {
                return (EntityId(start_id), EntityId(end_id))
            } else {
                // need to allocate more memory
                self.allocator.write().unwrap().reallocate();
            }
        }

        panic!();
    }

    pub fn add_entity(&mut self, x: u64) -> EntityId {
        if let Some(id) = self.free_ids.pop() {
            self.allocator.read().unwrap().set(id, x);
            EntityId(id)
        } else {
            self.add_entities_contiguous(iter::once(x)).0
        }
    }

    pub fn remove_entity(&mut self, id: EntityId) -> Option<u64> {
        self.free_ids.push(id.0);
        self.allocator.read().unwrap().get(id.0)
        /*if !self.is_entity_free(id)? {
            self.free_entities.push(id.0);
            let entity = std::mem::replace(&mut self.entities[id.0], None);
            entity
        } else {
            None
        }*/
    }

    /*pub fn is_entity_free(&self, id: EntityId) -> Option<bool> {
        let id = id.0;
        if id < self.entities.len() {
            Some( self.entities[id].is_none() )
        } else {
            None
        }
    }*/
}