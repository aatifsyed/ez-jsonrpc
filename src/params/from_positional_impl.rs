use super::DeserializePositional;
use serde::{de::Error as _, Deserialize};
use std::fmt;

macro_rules! ptr {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<'de, T> DeserializePositional<'de> for $ty where T: DeserializePositional<'de> {
                fn de_positional<D: serde::de::SeqAccess<'de>>(
                    deserializer: D,
                ) -> Result<Self, D::Error>
                where
                    Self: Sized,
                {
                    T::de_positional(deserializer).map(Into::into)
                }
            }
        )*
    };
}

ptr!(Box<T>, std::sync::Arc<T>, std::rc::Rc<T>);

macro_rules! iter {
    ($($ty:ty $(: $($bound:path)+)?),* $(,)?) => {
        $(
            impl<'de, T> DeserializePositional<'de> for $ty
            where
                T: Deserialize<'de> $($(+ $bound)*)?,
            {
                fn de_positional<D: serde::de::SeqAccess<'de>>(
                    deserializer: D,
                ) -> Result<Self, D::Error>
                where
                    Self: Sized,
                {
                    Self::deserialize(serde::de::value::SeqAccessDeserializer::new(deserializer))
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
        impl<'de, $($ty),*> DeserializePositional<'de> for ($($ty,)*)
        where
        $($ty: Deserialize<'de>),*
        {
            fn de_positional<D: serde::de::SeqAccess<'de>>(
                mut deserializer: D,
            ) -> Result<Self, D::Error>
            where
                Self: Sized,
            {
                struct Expected;
                impl serde::de::Expected for Expected {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        const ARITY: usize = <[()]>::len(&[$({stringify!($ty);},)*]);
                        f.write_fmt(format_args!("a sequence of length {}", ARITY))
                    }
                }
                #[allow(unused_mut)]
                let mut ct = 0;
                let ret = ($({
                    match deserializer.next_element::<$ty>()? {
                        Some(it) => { ct +=1; it },
                        None => return Err(D::Error::invalid_length(ct, &Expected)),
                    }
                },)*);
                let Ok(None) = deserializer.next_element::<serde::de::IgnoredAny>() else {
                    return Err(D::Error::invalid_length(ct + 1, &Expected))
                };
                Ok(ret)
            }
        }
    };
}

super::for_tuples!(tuple);
