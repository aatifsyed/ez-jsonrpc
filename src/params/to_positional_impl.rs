use super::SerializePositional;
use serde::{ser::SerializeSeq, Serialize};

macro_rules! ptr {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<T: ?Sized> SerializePositional for $ty where T: SerializePositional {
                fn ser_positional<S: SerializeSeq>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                    T::ser_positional(self, serializer)
                }
            }
        )*
    };
}

ptr!(&T, &mut T, Box<T>, std::sync::Arc<T>, std::rc::Rc<T>);

macro_rules! iter {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<T> SerializePositional for $ty where T: Serialize {
                fn ser_positional<S: SerializeSeq>(&self, mut serializer: S) -> Result<S::Ok, S::Error> {
                    for it in self {
                        serializer.serialize_element(it)?
                    }
                    serializer.end()
                }
            }
        )*
    };
}

iter!(
    Vec<T>,
    [T],
    std::collections::LinkedList<T>,
    std::collections::VecDeque<T>,
    std::collections::BinaryHeap<T>,
    std::collections::BTreeSet<T>,
    std::collections::HashSet<T>,
);

impl<const N: usize, T> SerializePositional for [T; N]
where
    T: Serialize,
{
    fn ser_positional<S: SerializeSeq>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.as_slice().ser_positional(serializer)
    }
}

macro_rules! tuple {
    ($($ty:ident),* $(,)?) => {
        impl<$($ty),*> SerializePositional for ($($ty,)*)
        where
        $($ty: Serialize),*
        {
            fn ser_positional<S: SerializeSeq>(&self,
                #[allow(unused_mut)]
                mut serializer: S
            ) -> Result<S::Ok, S::Error> {
                #[allow(non_snake_case)]
                let ($($ty,)*) = self;
                $(
                    serializer.serialize_element($ty)?;
                )*
                serializer.end()
            }
        }
    };
}

super::for_tuples!(tuple);
