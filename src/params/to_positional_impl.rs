use super::ToPositional;
use serde::{ser::SerializeSeq, Serialize};

macro_rules! ptr {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<T: ?Sized> ToPositional for $ty where T: ToPositional {
                fn to_positional<S: SerializeSeq>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                    T::to_positional(self, serializer)
                }
            }
        )*
    };
}

ptr!(&T, &mut T, Box<T>, std::sync::Arc<T>, std::rc::Rc<T>);

macro_rules! iter {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<T> ToPositional for $ty where T: Serialize {
                fn to_positional<S: SerializeSeq>(&self, mut serializer: S) -> Result<S::Ok, S::Error> {
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

impl<const N: usize, T> ToPositional for [T; N]
where
    T: Serialize,
{
    fn to_positional<S: SerializeSeq>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.as_slice().to_positional(serializer)
    }
}

macro_rules! tuple {
    ($($ty:ident),* $(,)?) => {
        impl<$($ty),*> ToPositional for ($($ty,)*)
        where
        $($ty: Serialize),*
        {
            fn to_positional<S: SerializeSeq>(&self,
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
