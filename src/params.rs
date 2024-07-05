//! Extra support for parameter (de)serialization
use std::fmt;

use crate::{map, RequestParameters};
use serde::{
    de::value::{MapDeserializer, SeqDeserializer},
    Deserializer,
};

impl<'de, T, E: serde::de::Error> serde::de::IntoDeserializer<'de, E> for RequestParameters<T>
where
    T: serde::de::IntoDeserializer<'de, E>,
{
    type Deserializer = IntoDeserializer<'de, T, E>;

    fn into_deserializer(self) -> Self::Deserializer {
        IntoDeserializer {
            inner: match self {
                RequestParameters::ByPosition(it) => {
                    _IntoDeserializer::Seq(SeqDeserializer::new(it.into_iter()))
                }
                RequestParameters::ByName(it) => {
                    _IntoDeserializer::Map(MapDeserializer::new(it.into_iter()))
                }
            },
        }
    }
}

/// [`IntoDeserializer`](serde::de::IntoDeserializer) implementation for [`RequestParameters`].
pub struct IntoDeserializer<'de, T, E> {
    inner: _IntoDeserializer<'de, T, E>,
}

impl<'de, T, E> IntoDeserializer<'de, T, E> {
    /// Check for remaining elements.
    pub fn end(self) -> Result<(), E>
    where
        E: serde::de::Error,
    {
        let Self { inner } = self;
        match inner {
            _IntoDeserializer::Seq(it) => it.end(),
            _IntoDeserializer::Map(it) => it.end(),
        }
    }
}

impl<'de, T, E> fmt::Debug for IntoDeserializer<'de, T, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RequestParametersIntoDeserializer")
            .finish_non_exhaustive()
    }
}

enum _IntoDeserializer<'de, T, E> {
    Seq(SeqDeserializer<std::vec::IntoIter<T>, E>),
    Map(MapDeserializer<'de, map::IntoIter<T>, E>),
}

impl<'de, T, E> Deserializer<'de> for IntoDeserializer<'de, T, E>
where
    E: serde::de::Error,
    T: serde::de::IntoDeserializer<'de, E>,
{
    type Error = E;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        let Self { inner } = self;
        match inner {
            _IntoDeserializer::Seq(it) => it.deserialize_any(visitor),
            _IntoDeserializer::Map(it) => it.deserialize_any(visitor),
        }
    }

    serde::forward_to_deserialize_any! {
        i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any bool
    }
}

#[test]
fn into_deserializer() {
    assert_eq!(
        <(String, u32)>::deserialize(
            RequestParameters::ByPosition(vec![json!("string"), json!(1)]).into_deserializer(),
        )
        .unwrap(),
        (String::from("string"), 1)
    );
    #[derive(serde::Deserialize, PartialEq, Debug)]
    struct Foo {
        name: String,
        count: u32,
    }
    assert_eq!(
        <Foo>::deserialize(
            RequestParameters::ByName(crate::Map::from_iter([
                (String::from("name"), json!("string")),
                (String::from("count"), json!(1))
            ]))
            .into_deserializer(),
        )
        .unwrap(),
        Foo {
            name: "string".into(),
            count: 1
        }
    );
}

#[cfg(test)]
use {
    serde::{de::IntoDeserializer as _, Deserialize as _},
    serde_json::json,
};
