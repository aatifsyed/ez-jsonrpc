//! A transcription of types from the [`JSON-RPC 2.0` Specification](https://www.jsonrpc.org/specification).
//!
//! > When quoted, the specification will appear as blockquoted text, like so.
//!
//! # Design
//! - By default, all structs are owned and spec conformant.
//!   This makes simple usage much easier than e.g [`jsonrpc`](https://docs.rs/jsonrpc/0.18.0/jsonrpc/index.html)
//!   or [`jsonrpsee`](https://docs.rs/jsonrpsee/0.24.7/jsonrpsee/index.html).
//! - Most interesting fields are parameterisable in the [`template`] module,
//!   which you can opt-in to for zero copy/sharing.

use serde_json::{Number, Value};

pub mod map;
pub mod template;

/// A JSON-RPC 2.0 request.
///
/// ```
/// # use _doctest::*;
/// # use ez_jsonrpc_types::*;
/// assert_roundtrip! {
///     Request {
///         method: String::from("hello"),
///         id: None,
///         params: None,
///     },
///     {
///         "jsonrpc": "2.0",
///         "method": "hello",
///     };
///     Request {
///         method: String::from("hello"),
///         id: Some(Id::Number(1.into())),
///         params: Some(RequestParameters::ByPosition(vec![])),
///     },
///     {
///         "jsonrpc": "2.0",
///         "method": "hello",
///         "id": 1,
///         "params": [],
///     };
/// }
/// ```
///
/// See [`template::Request`] for specification wording.
pub type Request = template::Request;
/// JSON-RPC 2.0 request parameters (by-name or by-position).
///
/// ```
/// # use _doctest::*;
/// # use ez_jsonrpc_types::*;
/// assert_roundtrip! {
///     RequestParameters::ByPosition(vec![json!("hello"), json!("world")]),
///     ["hello", "world"];
///
///     RequestParameters::ByName(Map::from([(String::from("p0"), json!("hello"))])),
///     {
///         "p0": "hello"
///     };
/// }
/// ```
///
/// See [`template::RequestParameters`] for specification wording.
pub type RequestParameters = template::RequestParameters;
/// JSON-RPC 2.0 request correlator.
///
/// ```
/// # use _doctest::*;
/// # use ez_jsonrpc_types::*;
/// assert_roundtrip! {
///     Id::Null, null;
///     Id::String(String::from("deadbeef")), "deadbeef";
///     Id::Number(serde_json::Number::from(42)), 42;
/// }
/// ```
///
/// See [`template::Id`] for specification wording.
pub type Id = template::Id;
/// JSON-RPC 2.0 response.
///
/// ```
/// # use _doctest::*;
/// # use ez_jsonrpc_types::*;
/// assert_roundtrip! {
///     Response {
///         result: Ok(json!(":)")),
///         id: Id::from(100)
///     },
///     {
///         "jsonrpc": "2.0",
///         "result": ":)",
///         "id": 100,
///     };
///     Response {
///         result: Err(Error {
///             code: 404,
///             message: String::from("not found"),
///             data: None
///         }),
///         id: Id::from(200)
///     },
///     {
///         "jsonrpc": "2.0",
///         "error": {
///             "code": 404,
///             "message": "not found",
///         },
///         "id": 200,
///     };
/// }
/// ```
///
/// See [`template::Response`] for specification wording.
pub type Response = template::Response;
/// A map from strings to JSON [`Value`]s.
///
/// Like [`serde_json::Map`], can be configured to preserve ordering at compile time.
pub type Map = map::Map;
/// JSON-RPC 2.0 Error object.
///
/// ```
/// # use _doctest::*;
/// # use ez_jsonrpc_types::*;
/// assert_roundtrip! {
///     Error {
///         code: 404,
///         message: String::from("not found"),
///         data: None
///     },
///     {
///         "code": 404,
///         "message": "not found",
///     };
///     Error::invalid_request("can't shave this yak!", json!({"wants shaving": false})),
///     {
///         "code": -32600, // from the spec
///         "message": "can't shave this yak!",
///         "data": {
///             "wants shaving": false,
///         },
///     }
/// }
/// ```
///
/// Also implements standard error traits,
/// so you can propogate with `?` if you desire.
/// ```
/// # use ez_jsonrpc_types::*;
/// let e = Error::method_not_found(":(", None);
/// let e: Box<dyn std::error::Error> = Box::new(e);
/// assert_eq!(
///     e.to_string(),
///     "code -32601 ( The method does not exist / is not available.): `:(`",
/// );
/// ```
///
/// See [`template::Error`] for specification wording.
pub type Error = template::Error;
/// See [`template::MaybeBatchedRequest`] for specification wording.
pub type MaybeBatchedRequest = template::MaybeBatchedRequest;
/// See [`template::MaybeBatchedResponse`] for specification wording.
pub type MaybeBatchedResponse = template::MaybeBatchedResponse;

pub type Result<T = Value> = std::result::Result<T, Error>;

impl From<serde_json::Map<String, Value>> for Map {
    fn from(value: serde_json::Map<String, Value>) -> Self {
        value.into_iter().collect()
    }
}
impl From<Map> for serde_json::Map<String, Value> {
    fn from(value: Map) -> Self {
        value.into_iter().collect()
    }
}

impl<const N: usize> From<[(String, Value); N]> for Map {
    fn from(value: [(String, Value); N]) -> Self {
        Self::from_iter(value)
    }
}

impl From<Number> for Id {
    fn from(value: Number) -> Self {
        Self::Number(value)
    }
}
impl From<String> for Id {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<&str> for Id {
    fn from(value: &str) -> Self {
        Self::String(value.into())
    }
}

macro_rules! int2id {
    ($($ty:ty),* $(,)?) => {
        $(
            impl From<$ty> for Id {
                fn from(value: $ty) -> Self {
                    Self::Number(value.into())
                }
            }
        )*
    };
}

int2id! {
    i8, i16, i32, i64, isize,
    u8, u16, u32, u64, usize,
}

mod params {
    use std::marker::PhantomData;

    use serde::de::{
        self,
        value::{MapDeserializer, SeqDeserializer},
    };

    struct Shim<T>(T);
    impl<'de, T: serde::Deserializer<'de>> de::IntoDeserializer<'de, T::Error> for Shim<T> {
        type Deserializer = T;
        fn into_deserializer(self) -> Self::Deserializer {
            self.0
        }
    }

    impl<'de, ValueT> de::Deserializer<'de> for crate::template::RequestParameters<ValueT>
    where
        ValueT: de::Deserializer<'de>,
    {
        type Error = ValueT::Error;

        fn deserialize_any<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
            match self {
                crate::template::RequestParameters::ByPosition(vec) => {
                    visitor.visit_seq(SeqDeserializer::new(vec.into_iter().map(Shim)))
                }
                crate::template::RequestParameters::ByName(map) => visitor.visit_map(
                    MapDeserializer::new(map.into_iter().map(|(k, v)| (k, Shim(v)))),
                ),
            }
        }

        serde::forward_to_deserialize_any! {
            i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
            bytes byte_buf option unit unit_struct newtype_struct seq tuple
            tuple_struct map struct enum identifier ignored_any bool
        }
    }

    impl<'de, ValueT, E: de::Error> de::Deserializer<'de>
        for &'de crate::template::RequestParameters<ValueT>
    where
        &'de ValueT: de::Deserializer<'de, Error = E>,
    {
        type Error = E;

        fn deserialize_any<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
            match self {
                crate::template::RequestParameters::ByPosition(vec) => {
                    visitor.visit_seq(SeqDeserializer::new(vec.iter().map(Shim)))
                }
                crate::template::RequestParameters::ByName(map) => visitor.visit_map(
                    MapDeserializer::new(map.iter().map(|(k, v)| (&**k, Shim(v)))),
                ),
            }
        }

        serde::forward_to_deserialize_any! {
            i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
            bytes byte_buf option unit unit_struct newtype_struct seq tuple
            tuple_struct map struct enum identifier ignored_any bool
        }
    }

    impl<'de, ValueT, E: de::Error> de::IntoDeserializer<'de, E>
        for crate::template::RequestParameters<ValueT>
    where
        ValueT: de::IntoDeserializer<'de, E>,
    {
        type Deserializer = IntoDeserializer<crate::template::RequestParameters<ValueT>, E>;

        fn into_deserializer(self) -> Self::Deserializer {
            IntoDeserializer {
                inner: self,
                error: PhantomData,
            }
        }
    }

    impl<'de, ValueT, E: de::Error> de::IntoDeserializer<'de, E>
        for &'de crate::template::RequestParameters<ValueT>
    where
        &'de ValueT: de::IntoDeserializer<'de, E>,
    {
        type Deserializer = IntoDeserializer<&'de crate::template::RequestParameters<ValueT>, E>;

        fn into_deserializer(self) -> Self::Deserializer {
            IntoDeserializer {
                inner: self,
                error: PhantomData,
            }
        }
    }

    pub struct IntoDeserializer<InnerT, E> {
        inner: InnerT,
        error: PhantomData<fn() -> E>,
    }

    impl<'de, ValueT, E: de::Error> de::Deserializer<'de>
        for IntoDeserializer<crate::template::RequestParameters<ValueT>, E>
    where
        ValueT: de::IntoDeserializer<'de, E>,
    {
        type Error = E;

        fn deserialize_any<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
            match self.inner {
                crate::template::RequestParameters::ByPosition(vec) => {
                    visitor.visit_seq(SeqDeserializer::new(vec.into_iter()))
                }
                crate::template::RequestParameters::ByName(map) => {
                    visitor.visit_map(MapDeserializer::new(map.into_iter()))
                }
            }
        }

        serde::forward_to_deserialize_any! {
            i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
            bytes byte_buf option unit unit_struct newtype_struct seq tuple
            tuple_struct map struct enum identifier ignored_any bool
        }
    }

    impl<'de, ValueT, E: de::Error> de::Deserializer<'de>
        for IntoDeserializer<&'de crate::template::RequestParameters<ValueT>, E>
    where
        &'de ValueT: de::IntoDeserializer<'de, E>,
    {
        type Error = E;

        fn deserialize_any<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
            match self.inner {
                crate::template::RequestParameters::ByPosition(vec) => {
                    visitor.visit_seq(SeqDeserializer::new(vec.iter()))
                }
                crate::template::RequestParameters::ByName(map) => {
                    visitor.visit_map(MapDeserializer::new(map.iter().map(|(k, v)| (&**k, v))))
                }
            }
        }

        serde::forward_to_deserialize_any! {
            i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
            bytes byte_buf option unit unit_struct newtype_struct seq tuple
            tuple_struct map struct enum identifier ignored_any bool
        }
    }
}
