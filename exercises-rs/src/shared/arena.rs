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

pub trait TempsCount {
    type CountType : Copy;
    fn temps_count(&self) -> Self::CountType;
}

pub trait ReleaseTemps : TempsCount {
    fn release_temporaries(&mut self, target_count: Self::CountType);

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

#[macro_export]
macro_rules! declare_arena {
    ($name:ident($n0:tt: $t0:ty, $($n:tt: $t:ty),+)) => {
        pub struct $name(
            crate::shared::arena::FlatArena<$t0>,
            $(crate::shared::arena::FlatArena<$t>),+
        );
        impl $name {
            pub fn new() -> Self {
                Self(
                    crate::shared::arena::FlatArena::<$t0>::new(),
                    $(crate::shared::arena::FlatArena::<$t>::new()),+
                )
            }
        }

        macro_rules! replace_with_usize { ($x:tt) => { usize }; }
        impl crate::shared::arena::TempsCount for $name {
            type CountType = (usize, $(replace_with_usize!($n)),+);
            fn temps_count(&self) -> Self::CountType {
                (
                    self.$n0.temps_count(),
                    $(self.$n.temps_count()),+
                )
            }
        }

        impl crate::shared::arena::ReleaseTemps for $name {
            fn release_temporaries(&mut self, target_count: Self::CountType) {
                self.$n0.release_temporaries(target_count.$n0);
                $(self.$n.release_temporaries(target_count.$n);)+
            }
        }

        macro_rules! impl_arena {
            ($num:tt, $typ:ty) => {
                impl crate::shared::arena::TypedArena<$typ> for $name {
                    fn alloc(
                        &mut self,
                        value: $typ
                    ) -> crate::shared::arena::HandleId<$typ> { self.$num.alloc(value) }

                    fn alloc_temp(
                        &mut self,
                        init: impl FnOnce() -> $typ,
                        reset: impl FnOnce(&mut $typ) -> ()
                    ) -> HandleId<$typ> { self.$num.alloc_temp(init, reset) }

                    fn dealloc(
                        &mut self,
                        handle: crate::shared::arena::HandleId<$typ>
                    ) -> bool { self.$num.dealloc(handle) }

                    fn get(
                        &self,
                        handle: crate::shared::arena::HandleId<$typ>
                    ) -> Option<&$typ> { self.$num.get(handle) }

                    fn get_mut(
                        &mut self,
                        handle: crate::shared::arena::HandleId<$typ>
                    ) -> Option<&mut $typ> { self.$num.get_mut(handle) }
                }
            };
        }
        impl_arena!($n0, $t0); $(impl_arena!($n, $t);)+
    };
}