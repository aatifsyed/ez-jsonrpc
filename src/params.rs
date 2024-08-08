//! Extra support for parameter (de)serialization
mod assert_named;
mod from_named_impl;
mod from_positional_impl;
mod ser;
mod to_named_impl;
mod to_positional_impl;

use std::marker::PhantomData;

#[doc(inline)]
pub use {
    assert_named::AssertNamed,
    ser::{Error, Serializer},
};

pub trait SerializePositional {
    fn ser_positional<S: serde::ser::SerializeSeq>(&self, serializer: S)
        -> Result<S::Ok, S::Error>;
}

pub trait DeserializePositional<'de>: Sized {
    fn de_positional<D: serde::de::SeqAccess<'de>>(deserializer: D) -> Result<Self, D::Error>;
}

pub trait DeserializePositionalSeed<'de>: Sized {
    type Value;
    fn de_positional_seed<D: serde::de::SeqAccess<'de>>(
        self,
        deserializer: D,
    ) -> Result<Self::Value, D::Error>;
}

impl<'de, T> DeserializePositionalSeed<'de> for PhantomData<T>
where
    T: DeserializePositional<'de>,
{
    type Value = T;

    fn de_positional_seed<D: serde::de::SeqAccess<'de>>(
        self,
        deserializer: D,
    ) -> Result<Self::Value, D::Error> {
        T::de_positional(deserializer)
    }
}

pub trait SerializeNamed {
    fn ser_named<S: serde::ser::SerializeMap>(&self, serializer: S) -> Result<S::Ok, S::Error>;
}

pub trait DeserializeNamed<'de>: Sized {
    fn de_named<D: serde::de::MapAccess<'de>>(deserializer: D) -> Result<Self, D::Error>;
}

pub trait DeserializeNamedSeed<'de>: Sized {
    type Value;
    fn de_named_seed<D: serde::de::MapAccess<'de>>(
        self,
        deserializer: D,
    ) -> Result<Self::Value, D::Error>;
}

impl<'de, T> DeserializeNamedSeed<'de> for PhantomData<T>
where
    T: DeserializeNamed<'de>,
{
    type Value = T;
    fn de_named_seed<D: serde::de::MapAccess<'de>>(
        self,
        deserializer: D,
    ) -> Result<Self::Value, D::Error> {
        T::de_named(deserializer)
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
    use std::fmt;

    use super::*;

    use ez_jsonrpc_types::RequestParameters;
    use fmt::Debug;
    use serde::{
        de::{DeserializeOwned, IntoDeserializer as _},
        Deserialize, Serialize,
    };
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
            RequestParameters::ByName(crate::types::Map::from_iter([
                (String::from("name"), json!("string")),
                (String::from("count"), json!(1)),
            ])),
        );
    }
}
