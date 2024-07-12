//! Extra support for parameter (de)serialization
mod assert_named;
mod from_named_impl;
mod from_positional_impl;
mod ser;
mod to_named_impl;
mod to_positional_impl;

use crate::{map, RequestParameters};
use serde::de::{
    value::{MapDeserializer, SeqDeserializer},
    IntoDeserializer,
};
use std::fmt;

#[doc(inline)]
pub use {
    assert_named::AssertNamed,
    ser::{Error, Serializer},
};

pub trait ToPositional {
    fn to_positional<S: serde::ser::SerializeSeq>(&self, serializer: S) -> Result<S::Ok, S::Error>;
}

pub trait FromPositional<'de> {
    fn from_positional<D: serde::de::SeqAccess<'de>>(deserializer: D) -> Result<Self, D::Error>
    where
        Self: Sized;
}

pub trait ToNamed {
    fn to_named<S: serde::ser::SerializeMap>(&self, serializer: S) -> Result<S::Ok, S::Error>;
}

pub trait FromNamed<'de> {
    fn from_named<D: serde::de::MapAccess<'de>>(deserializer: D) -> Result<Self, D::Error>
    where
        Self: Sized;
}

impl<'de, T, E: serde::de::Error> IntoDeserializer<'de, E> for RequestParameters<T>
where
    T: IntoDeserializer<'de, E>,
{
    type Deserializer = Deserializer<'de, T, E>;

    fn into_deserializer(self) -> Self::Deserializer {
        Deserializer {
            inner: match self {
                RequestParameters::ByPosition(it) => {
                    _Deserializer::Seq(SeqDeserializer::new(it.into_iter()))
                }
                RequestParameters::ByName(it) => {
                    _Deserializer::Map(MapDeserializer::new(it.into_iter()))
                }
            },
        }
    }
}

/// [`Deserializer`](serde::Deserializer) implementation for [`RequestParameters`].
pub struct Deserializer<'de, T, E> {
    inner: _Deserializer<'de, T, E>,
}

impl<'de, T, E> Deserializer<'de, T, E> {
    /// Check for remaining elements.
    pub fn end(self) -> Result<(), E>
    where
        E: serde::de::Error,
    {
        let Self { inner } = self;
        match inner {
            _Deserializer::Seq(it) => it.end(),
            _Deserializer::Map(it) => it.end(),
        }
    }
}

impl<'de, T, E> fmt::Debug for Deserializer<'de, T, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Deserializer").finish_non_exhaustive()
    }
}

enum _Deserializer<'de, T, E> {
    Seq(SeqDeserializer<std::vec::IntoIter<T>, E>),
    Map(MapDeserializer<'de, map::IntoIter<T>, E>),
}

impl<'de, T, E> serde::Deserializer<'de> for Deserializer<'de, T, E>
where
    E: serde::de::Error,
    T: IntoDeserializer<'de, E>,
{
    type Error = E;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        let Self { inner } = self;
        match inner {
            _Deserializer::Seq(it) => it.deserialize_any(visitor),
            _Deserializer::Map(it) => it.deserialize_any(visitor),
        }
    }

    serde::forward_to_deserialize_any! {
        i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any bool
    }
}

macro_rules! for_tuples {
    ($callback:ident) => {
        $callback!();
        $callback!(T0);
        $callback!(T0, T1);
        $callback!(T0, T1, T2);
        $callback!(T0, T1, T2, T3);
        $callback!(T0, T1, T2, T3, T4);
        $callback!(T0, T1, T2, T3, T4, T5);
        $callback!(T0, T1, T2, T3, T4, T5, T6);
        $callback!(T0, T1, T2, T3, T4, T5, T6, T7);
        $callback!(T0, T1, T2, T3, T4, T5, T6, T7, T8);
        $callback!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9);
        $callback!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
        $callback!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
        $callback!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);
        $callback!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T14);
        $callback!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T14, T15);
        $callback!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T14, T15, T16);
    };
}
pub(crate) use for_tuples;

#[cfg(test)]
mod tests {
    use super::*;

    use fmt::Debug;
    use serde::{de::DeserializeOwned, Deserialize, Serialize};
    use serde_json::json;

    #[derive(Deserialize, Serialize, PartialEq, Debug)]
    struct Foo {
        name: String,
        count: u32,
    }

    #[track_caller]
    fn do_test<T: DeserializeOwned + Serialize + PartialEq + Debug>(
        item: T,
        expected: RequestParameters,
    ) {
        assert_eq!(
            &T::deserialize(expected.clone().into_deserializer()).expect("couldn't deserialize"),
            &item,
            "deserialized mismatch"
        );
        assert_eq!(
            item.serialize(Serializer).expect("couldn't serialize"),
            expected,
            "serialized mismatch"
        )
    }

    #[test]
    fn test() {
        do_test(
            (String::from("hello"), 1),
            RequestParameters::ByPosition(vec![json!("hello"), json!(1)]),
        );
        do_test(
            Foo {
                name: "string".into(),
                count: 1,
            },
            RequestParameters::ByName(crate::Map::from_iter([
                (String::from("name"), json!("string")),
                (String::from("count"), json!(1)),
            ])),
        );
    }
}
