use super::FromPositional;
use serde::{
    de::{value::SeqDeserializer, IntoDeserializer},
    Deserialize,
};

macro_rules! ptr {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<'de, T> FromPositional<'de> for $ty where T: FromPositional<'de> {
                fn from_positional<II, I, E>(
                    deserializer: SeqDeserializer<II, E>,
                ) -> Result<Self, E>
                where
                    Self: Sized,
                    II: Iterator<Item = I>,
                    I: IntoDeserializer<'de, E>,
                    E: serde::de::Error
                {
                    T::from_positional(deserializer).map(Into::into)
                }
            }
        )*
    };
}

ptr!(Box<T>, std::sync::Arc<T>, std::rc::Rc<T>);

macro_rules! iter {
    ($($ty:ty $(: $($bound:path)+)?),* $(,)?) => {
        $(
            impl<'de, T> FromPositional<'de> for $ty
            where
                T: Deserialize<'de> $($(+ $bound)*)?,
            {
                fn from_positional<II, I, E>(
                    deserializer: SeqDeserializer<II, E>,
                ) -> Result<Self, E>
                where
                    Self: Sized,
                    II: Iterator<Item = I>,
                    I: IntoDeserializer<'de, E>,
                    E: serde::de::Error,
                {
                    Deserialize::deserialize(deserializer)
                }
            }
        )*
    };
}

iter!(
    Vec<T>,
    Box<[T]>,
    std::collections::LinkedList<T>,
    std::collections::VecDeque<T>,
    std::collections::BinaryHeap<T>: Ord,
    std::collections::BTreeSet<T>: Ord,
    std::collections::HashSet<T>: Eq std::hash::Hash,
);

macro_rules! tuple {
    ($($ty:ident),* $(,)?) => {
        impl<'de, $($ty),*> FromPositional<'de> for ($($ty,)*)
        where
        $($ty: Deserialize<'de>),*
        {
            fn from_positional<I, T, E>(deserializer: SeqDeserializer<I, E>) -> Result<Self, E>
            where
                Self: Sized,
                I: Iterator<Item = T>,
                T: IntoDeserializer<'de, E>,
                E: serde::de::Error,
            {
                Deserialize::deserialize(deserializer)
            }
        }
    };
}

super::for_tuples!(tuple);
