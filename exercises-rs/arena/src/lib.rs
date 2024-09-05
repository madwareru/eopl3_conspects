use std::collections::HashSet;
use std::marker::PhantomData;

pub struct HandleId<T>(usize, PhantomData<T>);
impl<T> Clone for HandleId<T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}
impl<T> Copy for HandleId<T> {}

pub trait TempsCount {
    type CountType: Copy;
    fn temps_count(&self) -> Self::CountType;
}

pub trait ReleaseTemps: TempsCount {
    fn release_temporaries(&mut self, target_count: Self::CountType);
}

pub trait TypedArena<T>: Sized + ReleaseTemps {
    fn alloc(&mut self, value: T) -> HandleId<T>;

    fn alloc_temp(
        &mut self,
        init: impl FnOnce() -> T,
        reset: impl FnOnce(&mut T) -> (),
    ) -> HandleId<T>;

    fn dealloc(&mut self, handle: HandleId<T>) -> bool;

    fn get(&self, handle: HandleId<T>) -> Option<&T>;

    fn get_mut(&mut self, handle: HandleId<T>) -> Option<&mut T>;
}

pub struct FlatArena<T> {
    elements: Vec<T>,
    deleted: HashSet<usize>,
    free_list: Vec<HandleId<T>>,
    temp_allocations: Vec<HandleId<T>>,
}

impl<T> FlatArena<T> {
    pub fn new() -> Self {
        Self {
            elements: Vec::new(),
            deleted: HashSet::new(),
            free_list: Vec::new(),
            temp_allocations: Vec::new(),
        }
    }

    fn alloc_from_free_list(&mut self) -> Option<HandleId<T>> {
        let handle = self.free_list.pop()?;
        self.deleted.remove(&handle.0);
        Some(handle)
    }
}

impl<T> TypedArena<T> for FlatArena<T> {
    fn alloc(&mut self, value: T) -> HandleId<T> {
        match self.alloc_from_free_list() {
            None => {
                let next_id = HandleId(self.elements.len(), PhantomData);
                self.elements.push(value);
                next_id
            }
            Some(id) => {
                self.elements[id.0] = value;
                id
            }
        }
    }

    fn alloc_temp(
        &mut self,
        init: impl FnOnce() -> T,
        reset: impl FnOnce(&mut T) -> (),
    ) -> HandleId<T> {
        let id = match self.alloc_from_free_list() {
            None => {
                let value = init();
                let next_id = HandleId(self.elements.len(), PhantomData);
                self.elements.push(value);
                next_id
            }
            Some(id) => {
                reset(&mut self.elements[id.0]);
                id
            }
        };
        self.temp_allocations.push(id);
        id
    }

    fn dealloc(&mut self, handle: HandleId<T>) -> bool {
        handle.0 < self.elements.len() && self.deleted.insert(handle.0) && {
            self.free_list.push(handle);
            true
        }
    }

    fn get(&self, handle: HandleId<T>) -> Option<&T> {
        match self.deleted.get(&handle.0) {
            None => self.elements.get(handle.0),
            _ => None,
        }
    }

    fn get_mut(&mut self, handle: HandleId<T>) -> Option<&mut T> {
        match self.deleted.get(&handle.0) {
            None => self.elements.get_mut(handle.0),
            _ => None,
        }
    }
}

impl<T> TempsCount for FlatArena<T> {
    type CountType = usize;
    fn temps_count(&self) -> Self::CountType {
        self.temp_allocations.len()
    }
}

impl<T> ReleaseTemps for FlatArena<T> {
    fn release_temporaries(&mut self, target_count: Self::CountType) {
        let mut temp_allocs = std::mem::take(&mut self.temp_allocations);
        for handle_id in temp_allocs.drain(target_count..) {
            self.dealloc(handle_id);
        }
        self.temp_allocations = temp_allocs;
    }
}
