use std::collections::HashSet;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

pub struct HandleId<T>(usize, PhantomData<T>);
impl<T> Clone for HandleId<T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}
impl<T> Copy for HandleId<T> {}

pub trait ReleaseTemps {
    type CountType : Copy;

    fn release_temporaries(&mut self, target_count: Self::CountType);

    fn temps_count(&self) -> Self::CountType;

    fn auto_release(&mut self) -> AutoReleaseTemps<Self> where Self: Sized {
        AutoReleaseTemps(self.temps_count(), self)
    }
}

pub struct AutoReleaseTemps<'a, T: ReleaseTemps>(T::CountType, &'a mut T);

impl<'a, T: ReleaseTemps> Drop for AutoReleaseTemps<'a, T> {
    fn drop(&mut self) {
        self.1.release_temporaries(self.0)
    }
}

impl<'a, T: ReleaseTemps> Deref for AutoReleaseTemps<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<'a, T: ReleaseTemps> DerefMut for AutoReleaseTemps<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.1
    }
}

pub trait TypedArena<T> : Sized + ReleaseTemps {
    fn alloc(&mut self, value: T) -> HandleId<T>;

    fn alloc_temp(
        &mut self,
        init: impl FnOnce() -> T,
        reset: impl FnOnce(&mut T) -> ()
    ) -> HandleId<T>;

    fn dealloc(&mut self, handle: HandleId<T>) -> bool;

    fn get(&self, handle: HandleId<T>) -> Option<&T>;

    fn get_mut(&mut self, handle: HandleId<T>) -> Option<&mut T>;
}

pub struct FlatArena<T>
{
    elements: Vec<T>,
    deleted: HashSet<usize>,
    free_list: Vec<HandleId<T>>,
    temp_allocations: Vec<HandleId<T>>
}

impl<T> FlatArena<T> {
    pub fn new() -> Self {
        Self {
            elements: Vec::new(),
            deleted: HashSet::new(),
            free_list: Vec::new(),
            temp_allocations: Vec::new()
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
        reset: impl FnOnce(&mut T) -> ()
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
        if self.deleted.contains(&handle.0) || handle.0 > self.elements.len() - 1 {
            false
        } else {
            self.deleted.insert(handle.0);
            self.free_list.push(handle);
            true
        }
    }

    fn get(&self, handle: HandleId<T>) -> Option<&T> {
        if self.deleted.contains(&handle.0) || handle.0 > self.elements.len() - 1 {
            None
        } else {
            self.elements.get(handle.0)
        }
    }

    fn get_mut(&mut self, handle: HandleId<T>) -> Option<&mut T> {
        if self.deleted.contains(&handle.0) || handle.0 > self.elements.len() - 1 {
            None
        } else {
            self.elements.get_mut(handle.0)
        }
    }
}

impl<T> ReleaseTemps for FlatArena<T> {
    type CountType = usize;

    fn release_temporaries(&mut self, target_count: Self::CountType) {
        let mut temp_allocs = std::mem::take(&mut self.temp_allocations);
        for handle_id in temp_allocs.drain(target_count..) {
            self.dealloc(handle_id);
        }
        self.temp_allocations = temp_allocs;
    }

    fn temps_count(&self) -> Self::CountType {
        self.temp_allocations.len()
    }
}

#[macro_export]
macro_rules! define_arena(
    ($name:ident($t0:ty, $t1:ty)) => {
        pub struct $name(crate::shared::arena::FlatArena<$t0>, crate::shared::arena::FlatArena<$t1>);

        impl $name {
            pub fn new() -> Self {
                Self(crate::shared::arena::FlatArena::new(), crate::shared::arena::FlatArena::new())
            }
        }

        impl crate::shared::arena::ReleaseTemps for $name {
            type CountType = (usize, usize);

            fn release_temporaries(&mut self, target_count: Self::CountType) {
                self.0.release_temporaries(target_count.0);
                self.1.release_temporaries(target_count.1);
            }

            fn temps_count(&self) -> Self::CountType {
                (
                    self.0.temps_count(),
                    self.1.temps_count()
                )
            }
        }

        impl crate::shared::arena::TypedArena<$t0> for $name {
            fn alloc(&mut self, value: $t0) -> crate::shared::arena::HandleId<$t0> {
                self.0.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t0,
                reset: impl FnOnce(&mut $t0) -> ()
            ) -> HandleId<$t0> {
                self.0.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t0>) -> bool {
                self.0.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t0>) -> Option<&$t0> {
                self.0.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t0>) -> Option<&mut $t0> {
                self.0.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t1> for $name {
            fn alloc(&mut self, value: $t1) -> crate::shared::arena::HandleId<$t1> {
                self.1.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t1,
                reset: impl FnOnce(&mut $t1) -> ()
            ) -> HandleId<$t1> {
                self.1.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t1>) -> bool {
                self.1.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t1>) -> Option<&$t1> {
                self.1.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t1>) -> Option<&mut $t1> {
                self.1.get_mut(handle)
            }
        }
    };
    ($name:ident($t0:ty, $t1:ty, $t2:ty)) => {
        pub struct $name(
            crate::shared::arena::FlatArena<$t0>,
            crate::shared::arena::FlatArena<$t1>,
            crate::shared::arena::FlatArena<$t2>
        );

        impl $name {
            pub fn new() -> Self {
                Self(
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                )
            }
        }

        impl crate::shared::arena::ReleaseTemps for $name {
            type CountType = (usize, usize, usize);

            fn release_temporaries(&mut self, target_count: Self::CountType) {
                self.0.release_temporaries(target_count.0);
                self.1.release_temporaries(target_count.1);
                self.2.release_temporaries(target_count.2);
            }

            fn temps_count(&self) -> Self::CountType {
                (
                    self.0.temps_count(),
                    self.1.temps_count(),
                    self.2.temps_count()
                )
            }
        }

        impl crate::shared::arena::TypedArena<$t0> for $name {
            fn alloc(&mut self, value: $t0) -> crate::shared::arena::HandleId<$t0> {
                self.0.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t0,
                reset: impl FnOnce(&mut $t0) -> ()
            ) -> HandleId<$t0> {
                self.0.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t0>) -> bool {
                self.0.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t0>) -> Option<&$t0> {
                self.0.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t0>) -> Option<&mut $t0> {
                self.0.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t1> for $name {
            fn alloc(&mut self, value: $t1) -> crate::shared::arena::HandleId<$t1> {
                self.1.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t1,
                reset: impl FnOnce(&mut $t1) -> ()
            ) -> HandleId<$t1> {
                self.1.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t1>) -> bool {
                self.1.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t1>) -> Option<&$t1> {
                self.1.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t1>) -> Option<&mut $t1> {
                self.1.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t2> for $name {
            fn alloc(&mut self, value: $t2) -> crate::shared::arena::HandleId<$t2> {
                self.2.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t2,
                reset: impl FnOnce(&mut $t2) -> ()
            ) -> HandleId<$t2> {
                self.2.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t2>) -> bool {
                self.2.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t2>) -> Option<&$t2> {
                self.2.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t2>) -> Option<&mut $t2> {
                self.2.get_mut(handle)
            }
        }
    };
    ($name:ident($t0:ty, $t1:ty, $t2:ty, $t3:ty)) => {
        pub struct $name(
            crate::shared::arena::FlatArena<$t0>,
            crate::shared::arena::FlatArena<$t1>,
            crate::shared::arena::FlatArena<$t2>,
            crate::shared::arena::FlatArena<$t3>
        );

        impl $name {
            pub fn new() -> Self {
                Self(
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                )
            }
        }

        impl crate::shared::arena::ReleaseTemps for $name {
            type CountType = (usize, usize, usize, usize);

            fn release_temporaries(&mut self, target_count: Self::CountType) {
                self.0.release_temporaries(target_count.0);
                self.1.release_temporaries(target_count.1);
                self.2.release_temporaries(target_count.2);
                self.3.release_temporaries(target_count.3);
            }

            fn temps_count(&self) -> Self::CountType {
                (
                    self.0.temps_count(),
                    self.1.temps_count(),
                    self.2.temps_count(),
                    self.3.temps_count()
                )
            }
        }

        impl crate::shared::arena::TypedArena<$t0> for $name {
            fn alloc(&mut self, value: $t0) -> crate::shared::arena::HandleId<$t0> {
                self.0.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t0,
                reset: impl FnOnce(&mut $t0) -> ()
            ) -> HandleId<$t0> {
                self.0.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t0>) -> bool {
                self.0.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t0>) -> Option<&$t0> {
                self.0.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t0>) -> Option<&mut $t0> {
                self.0.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t1> for $name {
            fn alloc(&mut self, value: $t1) -> crate::shared::arena::HandleId<$t1> {
                self.1.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t1,
                reset: impl FnOnce(&mut $t1) -> ()
            ) -> HandleId<$t1> {
                self.1.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t1>) -> bool {
                self.1.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t1>) -> Option<&$t1> {
                self.1.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t1>) -> Option<&mut $t1> {
                self.1.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t2> for $name {
            fn alloc(&mut self, value: $t2) -> crate::shared::arena::HandleId<$t2> {
                self.2.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t2,
                reset: impl FnOnce(&mut $t2) -> ()
            ) -> HandleId<$t2> {
                self.2.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t2>) -> bool {
                self.2.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t2>) -> Option<&$t2> {
                self.2.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t2>) -> Option<&mut $t2> {
                self.2.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t3> for $name {
            fn alloc(&mut self, value: $t3) -> crate::shared::arena::HandleId<$t3> {
                self.3.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t3,
                reset: impl FnOnce(&mut $t3) -> ()
            ) -> HandleId<$t3> {
                self.3.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t3>) -> bool {
                self.3.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t3>) -> Option<&$t3> {
                self.3.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t3>) -> Option<&mut $t3> {
                self.3.get_mut(handle)
            }
        }
    };
    ($name:ident(
        $t0:ty,
        $t1:ty,
        $t2:ty,
        $t3:ty,
        $t4:ty
    )) => {
        pub struct $name(
            crate::shared::arena::FlatArena<$t0>,
            crate::shared::arena::FlatArena<$t1>,
            crate::shared::arena::FlatArena<$t2>,
            crate::shared::arena::FlatArena<$t3>,
            crate::shared::arena::FlatArena<$t4>
        );

        impl $name {
            pub fn new() -> Self {
                Self(
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                )
            }
        }

        impl crate::shared::arena::ReleaseTemps for $name {
            type CountType = (usize, usize, usize, usize, usize);

            fn release_temporaries(&mut self, target_count: Self::CountType) {
                self.0.release_temporaries(target_count.0);
                self.1.release_temporaries(target_count.1);
                self.2.release_temporaries(target_count.2);
                self.3.release_temporaries(target_count.3);
                self.4.release_temporaries(target_count.4);
            }

            fn temps_count(&self) -> Self::CountType {
                (
                    self.0.temps_count(),
                    self.1.temps_count(),
                    self.2.temps_count(),
                    self.3.temps_count(),
                    self.4.temps_count()
                )
            }
        }

        impl crate::shared::arena::TypedArena<$t0> for $name {
            fn alloc(&mut self, value: $t0) -> crate::shared::arena::HandleId<$t0> {
                self.0.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t0,
                reset: impl FnOnce(&mut $t0) -> ()
            ) -> HandleId<$t0> {
                self.0.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t0>) -> bool {
                self.0.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t0>) -> Option<&$t0> {
                self.0.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t0>) -> Option<&mut $t0> {
                self.0.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t1> for $name {
            fn alloc(&mut self, value: $t1) -> crate::shared::arena::HandleId<$t1> {
                self.1.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t1,
                reset: impl FnOnce(&mut $t1) -> ()
            ) -> HandleId<$t1> {
                self.1.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t1>) -> bool {
                self.1.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t1>) -> Option<&$t1> {
                self.1.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t1>) -> Option<&mut $t1> {
                self.1.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t2> for $name {
            fn alloc(&mut self, value: $t2) -> crate::shared::arena::HandleId<$t2> {
                self.2.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t2,
                reset: impl FnOnce(&mut $t2) -> ()
            ) -> HandleId<$t2> {
                self.2.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t2>) -> bool {
                self.2.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t2>) -> Option<&$t2> {
                self.2.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t2>) -> Option<&mut $t2> {
                self.2.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t3> for $name {
            fn alloc(&mut self, value: $t3) -> crate::shared::arena::HandleId<$t3> {
                self.3.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t3,
                reset: impl FnOnce(&mut $t3) -> ()
            ) -> HandleId<$t3> {
                self.3.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t3>) -> bool {
                self.3.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t3>) -> Option<&$t3> {
                self.3.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t3>) -> Option<&mut $t3> {
                self.3.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t4> for $name {
            fn alloc(&mut self, value: $t4) -> crate::shared::arena::HandleId<$t4> {
                self.4.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t4,
                reset: impl FnOnce(&mut $t4) -> ()
            ) -> HandleId<$t4> {
                self.4.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t4>) -> bool {
                self.4.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t4>) -> Option<&$t4> {
                self.4.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t4>) -> Option<&mut $t4> {
                self.4.get_mut(handle)
            }
        }
    };
    ($name:ident(
        $t0:ty,
        $t1:ty,
        $t2:ty,
        $t3:ty,
        $t4:ty,
        $t5:ty
    )) => {
        pub struct $name(
            crate::shared::arena::FlatArena<$t0>,
            crate::shared::arena::FlatArena<$t1>,
            crate::shared::arena::FlatArena<$t2>,
            crate::shared::arena::FlatArena<$t3>,
            crate::shared::arena::FlatArena<$t4>,
            crate::shared::arena::FlatArena<$t5>
        );

        impl $name {
            pub fn new() -> Self {
                Self(
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                )
            }
        }

        impl crate::shared::arena::ReleaseTemps for $name {
            type CountType = (usize, usize, usize, usize, usize, usize);

            fn release_temporaries(&mut self, target_count: Self::CountType) {
                self.0.release_temporaries(target_count.0);
                self.1.release_temporaries(target_count.1);
                self.2.release_temporaries(target_count.2);
                self.3.release_temporaries(target_count.3);
                self.4.release_temporaries(target_count.4);
                self.5.release_temporaries(target_count.5);
            }

            fn temps_count(&self) -> Self::CountType {
                (
                    self.0.temps_count(),
                    self.1.temps_count(),
                    self.2.temps_count(),
                    self.3.temps_count(),
                    self.4.temps_count(),
                    self.5.temps_count()
                )
            }
        }

        impl crate::shared::arena::TypedArena<$t0> for $name {
            fn alloc(&mut self, value: $t0) -> crate::shared::arena::HandleId<$t0> {
                self.0.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t0,
                reset: impl FnOnce(&mut $t0) -> ()
            ) -> HandleId<$t0> {
                self.0.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t0>) -> bool {
                self.0.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t0>) -> Option<&$t0> {
                self.0.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t0>) -> Option<&mut $t0> {
                self.0.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t1> for $name {
            fn alloc(&mut self, value: $t1) -> crate::shared::arena::HandleId<$t1> {
                self.1.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t1,
                reset: impl FnOnce(&mut $t1) -> ()
            ) -> HandleId<$t1> {
                self.1.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t1>) -> bool {
                self.1.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t1>) -> Option<&$t1> {
                self.1.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t1>) -> Option<&mut $t1> {
                self.1.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t2> for $name {
            fn alloc(&mut self, value: $t2) -> crate::shared::arena::HandleId<$t2> {
                self.2.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t2,
                reset: impl FnOnce(&mut $t2) -> ()
            ) -> HandleId<$t2> {
                self.2.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t2>) -> bool {
                self.2.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t2>) -> Option<&$t2> {
                self.2.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t2>) -> Option<&mut $t2> {
                self.2.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t3> for $name {
            fn alloc(&mut self, value: $t3) -> crate::shared::arena::HandleId<$t3> {
                self.3.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t3,
                reset: impl FnOnce(&mut $t3) -> ()
            ) -> HandleId<$t3> {
                self.3.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t3>) -> bool {
                self.3.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t3>) -> Option<&$t3> {
                self.3.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t3>) -> Option<&mut $t3> {
                self.3.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t4> for $name {
            fn alloc(&mut self, value: $t4) -> crate::shared::arena::HandleId<$t4> {
                self.4.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t4,
                reset: impl FnOnce(&mut $t4) -> ()
            ) -> HandleId<$t4> {
                self.4.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t4>) -> bool {
                self.4.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t4>) -> Option<&$t4> {
                self.4.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t4>) -> Option<&mut $t4> {
                self.4.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t5> for $name {
            fn alloc(&mut self, value: $t5) -> crate::shared::arena::HandleId<$t5> {
                self.5.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t5,
                reset: impl FnOnce(&mut $t5) -> ()
            ) -> HandleId<$t5> {
                self.5.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t5>) -> bool {
                self.5.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t5>) -> Option<&$t5> {
                self.5.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t5>) -> Option<&mut $t5> {
                self.5.get_mut(handle)
            }
        }
    };
    ($name:ident(
        $t0:ty,
        $t1:ty,
        $t2:ty,
        $t3:ty,
        $t4:ty,
        $t5:ty,
        $t6:ty
    )) => {
        pub struct $name(
            crate::shared::arena::FlatArena<$t0>,
            crate::shared::arena::FlatArena<$t1>,
            crate::shared::arena::FlatArena<$t2>,
            crate::shared::arena::FlatArena<$t3>,
            crate::shared::arena::FlatArena<$t4>,
            crate::shared::arena::FlatArena<$t5>,
            crate::shared::arena::FlatArena<$t6>
        );

        impl $name {
            pub fn new() -> Self {
                Self(
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                    crate::shared::arena::FlatArena::new(),
                )
            }
        }

        impl crate::shared::arena::ReleaseTemps for $name {
            type CountType = (usize, usize, usize, usize, usize, usize, usize);

            fn release_temporaries(&mut self, target_count: Self::CountType) {
                self.0.release_temporaries(target_count.0);
                self.1.release_temporaries(target_count.1);
                self.2.release_temporaries(target_count.2);
                self.3.release_temporaries(target_count.3);
                self.4.release_temporaries(target_count.4);
                self.5.release_temporaries(target_count.5);
                self.6.release_temporaries(target_count.6);
            }

            fn temps_count(&self) -> Self::CountType {
                (
                    self.0.temps_count(),
                    self.1.temps_count(),
                    self.2.temps_count(),
                    self.3.temps_count(),
                    self.4.temps_count(),
                    self.5.temps_count(),
                    self.6.temps_count()
                )
            }
        }

        impl crate::shared::arena::TypedArena<$t0> for $name {
            fn alloc(&mut self, value: $t0) -> crate::shared::arena::HandleId<$t0> {
                self.0.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t0,
                reset: impl FnOnce(&mut $t0) -> ()
            ) -> HandleId<$t0> {
                self.0.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t0>) -> bool {
                self.0.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t0>) -> Option<&$t0> {
                self.0.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t0>) -> Option<&mut $t0> {
                self.0.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t1> for $name {
            fn alloc(&mut self, value: $t1) -> crate::shared::arena::HandleId<$t1> {
                self.1.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t1,
                reset: impl FnOnce(&mut $t1) -> ()
            ) -> HandleId<$t1> {
                self.1.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t1>) -> bool {
                self.1.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t1>) -> Option<&$t1> {
                self.1.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t1>) -> Option<&mut $t1> {
                self.1.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t2> for $name {
            fn alloc(&mut self, value: $t2) -> crate::shared::arena::HandleId<$t2> {
                self.2.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t2,
                reset: impl FnOnce(&mut $t2) -> ()
            ) -> HandleId<$t2> {
                self.2.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t2>) -> bool {
                self.2.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t2>) -> Option<&$t2> {
                self.2.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t2>) -> Option<&mut $t2> {
                self.2.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t3> for $name {
            fn alloc(&mut self, value: $t3) -> crate::shared::arena::HandleId<$t3> {
                self.3.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t3,
                reset: impl FnOnce(&mut $t3) -> ()
            ) -> HandleId<$t3> {
                self.3.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t3>) -> bool {
                self.3.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t3>) -> Option<&$t3> {
                self.3.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t3>) -> Option<&mut $t3> {
                self.3.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t4> for $name {
            fn alloc(&mut self, value: $t4) -> crate::shared::arena::HandleId<$t4> {
                self.4.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t4,
                reset: impl FnOnce(&mut $t4) -> ()
            ) -> HandleId<$t4> {
                self.4.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t4>) -> bool {
                self.4.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t4>) -> Option<&$t4> {
                self.4.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t4>) -> Option<&mut $t4> {
                self.4.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t5> for $name {
            fn alloc(&mut self, value: $t5) -> crate::shared::arena::HandleId<$t5> {
                self.5.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t5,
                reset: impl FnOnce(&mut $t5) -> ()
            ) -> HandleId<$t5> {
                self.5.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t5>) -> bool {
                self.5.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t5>) -> Option<&$t5> {
                self.5.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t5>) -> Option<&mut $t5> {
                self.5.get_mut(handle)
            }
        }
        impl crate::shared::arena::TypedArena<$t6> for $name {
            fn alloc(&mut self, value: $t6) -> crate::shared::arena::HandleId<$t6> {
                self.6.alloc(value)
            }

            fn alloc_temp(
                &mut self,
                init: impl FnOnce() -> $t6,
                reset: impl FnOnce(&mut $t6) -> ()
            ) -> HandleId<$t6> {
                self.6.alloc_temp(init, reset)
            }

            fn dealloc(&mut self, handle: crate::shared::arena::HandleId<$t6>) -> bool {
                self.6.dealloc(handle)
            }

            fn get(&self, handle: crate::shared::arena::HandleId<$t6>) -> Option<&$t6> {
                self.6.get(handle)
            }

            fn get_mut(&mut self, handle: crate::shared::arena::HandleId<$t6>) -> Option<&mut $t6> {
                self.6.get_mut(handle)
            }
        }
    }
);