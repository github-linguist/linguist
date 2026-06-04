use core::dict::Felt252DictTrait;
use core::nullable::{match_nullable, FromNullableResult, NullableTrait};
use core::integer;

trait VecTrait<V, T> {
    fn new() -> V;
    fn get(ref self: V, index: usize) -> Option<T>;
    fn at(ref self: V, index: usize) -> T;
    fn push(ref self: V, value: T) -> ();
    fn set(ref self: V, index: usize, value: T);
    fn len(self: @V) -> usize;
}

struct NullableVec<T> {
    data: Felt252Dict<Nullable<T>>,
    len: usize
}

impl DestructNullableVec<T, +Drop<T>> of Destruct<NullableVec<T>> {
    fn destruct(self: NullableVec<T>) nopanic {
        self.data.squash();
    }
}

impl NullableVecImpl<T, +Drop<T>, +Copy<T>> of VecTrait<NullableVec<T>, T> {
    fn new() -> NullableVec<T> {
        NullableVec { data: Default::default(), len: 0 }
    }

    fn get(ref self: NullableVec<T>, index: usize) -> Option<T> {
        if index < self.len() {
            Option::Some(self.data.get(index.into()).deref())
        } else {
            Option::None
        }
    }

    fn at(ref self: NullableVec<T>, index: usize) -> T {
        assert!(index < self.len(), "Index out of bounds");
        self.data.get(index.into()).deref()
    }

    fn push(ref self: NullableVec<T>, value: T) -> () {
        self.data.insert(self.len.into(), NullableTrait::new(value));
        self.len = core::integer::u32_wrapping_add(self.len, 1_usize);
    }
    fn set(ref self: NullableVec<T>, index: usize, value: T) {
        assert!(index < self.len(), "Index out of bounds");
        self.data.insert(index.into(), NullableTrait::new(value));
    }

    fn len(self: @NullableVec<T>) -> usize {
        *self.len
    }
}
