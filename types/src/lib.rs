//! A transcription of types from the [`JSON-RPC 2.0` Specification](https://www.jsonrpc.org/specification).
//!
//! > When quoted, the specification will appear as blockquoted text, like so.
//!
//! # Design
//! - All structs are owned (i.e, there is no borrowing of data from the [`Deserializer`](serde::Deserializer)),
//!   to facilitate ergonomics.
//! - Appearances of dynamic JSON [`Value`]s are parameterised out, to allow
//!   deferred serialization using, i.e [RawValue](https://docs.rs/serde_json/latest/serde_json/value/struct.RawValue.html).

pub mod map;

use std::{
    borrow::Cow,
    fmt::{self, Display},
    hash::Hash,
    ops::RangeInclusive,
    str::FromStr,
};

use serde::{
    de::{Error as _, Unexpected},
    Deserialize, Serialize,
};
use serde_json::{Number, Value};

#[doc(inline)]
pub use map::Map;
#[doc(inline)]
pub use params::Deserializer;

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

/// A `JSON-RPC 2.0` request object.
///
/// Note that the `"jsonrpc": "2.0"` member is transparently checked during
/// deserialization, and added during serialization.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Request<T = Value> {
    /// > A String containing the name of the method to be invoked.
    /// > Method names that begin with the word rpc followed by a period character
    /// > (U+002E or ASCII 46) are reserved for rpc-internal methods and extensions
    /// > and MUST NOT be used for anything else.
    pub method: String,
    /// > A Structured value that holds the parameter values to be used during the
    /// > invocation of the method.
    /// > This member MAY be omitted.
    pub params: Option<RequestParameters<T>>,
    /// > An identifier established by the Client that MUST contain a String,
    /// > Number, or NULL value if included.
    /// > If it is not included it is assumed to be a notification.
    /// > The value SHOULD normally not be Null and Numbers SHOULD NOT contain fractional parts
    pub id: Option<Id>,
}

impl<T> Serialize for Request<T>
where
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(Serialize)]
        struct Serde<'a, T> {
            jsonrpc: V2,
            method: &'a str,
            #[serde(skip_serializing_if = "Option::is_none")]
            params: Option<&'a RequestParameters<T>>,
            #[serde(skip_serializing_if = "Option::is_none")]
            id: Option<&'a Id>,
        }
        let Self { method, params, id } = self;
        Serde {
            jsonrpc: V2,
            method,
            params: params.as_ref(),
            id: id.as_ref(),
        }
        .serialize(serializer)
    }
}

impl<'de, T> Deserialize<'de> for Request<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct Serde<T> {
            #[allow(unused)]
            jsonrpc: V2,
            method: String,
            params: Option<RequestParameters<T>>,
            #[serde(deserialize_with = "deserialize_some", default)]
            id: Option<Id>,
        }
        Serde::deserialize(deserializer).map(
            |Serde {
                 method, params, id, ..
             }| Request { method, params, id },
        )
    }
}

impl<T> Request<T> {
    pub const fn is_notification(&self) -> bool {
        self.id.is_none()
    }
}

#[test]
fn request() {
    do_test::<Request>(
        Request {
            method: "myMethod".into(),
            params: None,
            id: None,
        },
        json!({
            "jsonrpc": "2.0",
            "method": "myMethod",
        }),
    );
    do_test::<Request>(
        Request {
            method: "myMethod".into(),
            params: None,
            id: Some(Id::Null),
        },
        json!({
            "jsonrpc": "2.0",
            "method": "myMethod",
            "id": null
        }),
    );
    do_test::<Request>(
        Request {
            method: "myMethod".into(),
            params: Some(RequestParameters::ByPosition(vec![
                Value::Null,
                Value::String("hello".into()),
            ])),
            id: None,
        },
        json!({
            "jsonrpc": "2.0",
            "method": "myMethod",
            "params": [null, Value::from("hello")]
        }),
    );
    do_test::<Request>(
        Request {
            method: "myMethod".into(),
            params: Some(RequestParameters::ByName(
                [
                    (String::from("hello"), Value::Null),
                    (String::from("world"), Value::from(1)),
                ]
                .into_iter()
                .collect(),
            )),
            id: None,
        },
        json!({
            "jsonrpc": "2.0",
            "method": "myMethod",
            "params": {
                "hello": null,
                "world": 1
            }
        }),
    );
}

/// > A String specifying the version of the JSON-RPC protocol.
/// > MUST be exactly "2.0".
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct V2;

impl<'de> Deserialize<'de> for V2 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct SerdeCow<'a>(#[serde(borrow)] Cow<'a, str>);
        let SerdeCow(cow) = SerdeCow::deserialize(deserializer)?;
        match &*cow {
            "2.0" => Ok(Self),
            other => Err(D::Error::invalid_value(Unexpected::Str(other), &"2.0")),
        }
    }
}

impl Serialize for V2 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str("2.0")
    }
}

/// > If present, parameters for the rpc call MUST be provided as a Structured value.
/// > Either by-position through an Array or by-name through an Object.
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(
    untagged,
    expecting = "an `Array` of by-position paramaters, or an `Object` of by-name parameters"
)]
pub enum RequestParameters<T = Value> {
    /// > params MUST be an Array, containing the values in the Server expected order.
    ByPosition(Vec<T>),
    /// > params MUST be an Object, with member names that match the Server
    /// > expected parameter names.
    /// > The absence of expected names MAY result in an error being generated.
    /// > The names MUST match exactly, including case, to the method's expected parameters.
    ByName(Map<T>),
}

impl<T> RequestParameters<T> {
    pub fn len(&self) -> usize {
        match self {
            RequestParameters::ByPosition(it) => it.len(),
            RequestParameters::ByName(it) => it.len(),
        }
    }
    pub fn is_empty(&self) -> bool {
        match self {
            RequestParameters::ByPosition(it) => it.is_empty(),
            RequestParameters::ByName(it) => it.is_empty(),
        }
    }
}

/// See [`Request::id`].
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Hash, Deserialize, Default)]
#[serde(untagged, expecting = "a string, a number, or null")]
pub enum Id {
    String(String),
    Number(Number),
    #[default]
    Null,
}

impl FromStr for Id {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

/// A `JSON-RPC 2.0` response object.
///
/// Note that the `"jsonrpc": "2.0"` member is transparently checked during
/// deserialization, and added during serialization.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Response<T = Value, E = Value> {
    /// > "result":
    /// >
    /// > This member is REQUIRED on success.
    /// > This member MUST NOT exist if there was an error invoking the method.
    /// > The value of this member is determined by the method invoked on the Server.
    /// >
    /// > "error":
    /// >
    /// > This member is REQUIRED on error.
    /// > This member MUST NOT exist if there was no error triggered during invocation.
    pub result: Result<T, Error<E>>,
    /// > This member is REQUIRED.
    /// > It MUST be the same as the value of the id member in the Request Object.
    /// > If there was an error in detecting the id in the Request object
    /// > (e.g. Parse error/Invalid Request), it MUST be Null.
    pub id: Id,
}

impl<T> Default for Response<T>
where
    T: Default,
{
    fn default() -> Self {
        Self {
            result: Ok(T::default()),
            id: Default::default(),
        }
    }
}

/// Distinguish between absent and present but null.
///
/// See <https://github.com/serde-rs/serde/issues/984#issuecomment-314143738>
fn deserialize_some<'de, T, D>(deserializer: D) -> Result<Option<T>, D::Error>
where
    T: Deserialize<'de>,
    D: serde::de::Deserializer<'de>,
{
    Deserialize::deserialize(deserializer).map(Some)
}

impl<T, E> Serialize for Response<T, E>
where
    T: Serialize,
    E: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(Serialize)]
        struct Serde<'a, T, E> {
            jsonrpc: V2,
            #[serde(skip_serializing_if = "Option::is_none")]
            result: Option<Option<&'a T>>,
            #[serde(skip_serializing_if = "Option::is_none")]
            error: Option<&'a Error<E>>,
            id: &'a Id,
        }
        let Self { result, id } = self;

        let helper = match result {
            Ok(ok) => Serde {
                jsonrpc: V2,
                result: Some(Some(ok)),
                error: None,
                id,
            },
            Err(e) => Serde {
                jsonrpc: V2,
                result: None,
                error: Some(e),
                id,
            },
        };
        helper.serialize(serializer)
    }
}

impl<'de, T, E> Deserialize<'de> for Response<T, E>
where
    T: Deserialize<'de>,
    E: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(bound(deserialize = "T: Deserialize<'de>, E: Deserialize<'de>"))]
        struct Serde<T, E> {
            #[allow(unused)]
            jsonrpc: V2,
            #[serde(default, deserialize_with = "deserialize_some")]
            result: Option<Option<T>>,
            #[serde(default, deserialize_with = "deserialize_some")]
            error: Option<Error<E>>,
            id: Id,
        }
        let Serde {
            jsonrpc: _,
            error,
            result,
            id,
        } = Serde::deserialize(deserializer)?;
        match (result, error) {
            (Some(Some(ok)), None) => Ok(Response { result: Ok(ok), id }),
            (None, Some(err)) => Ok(Response {
                result: Err(err),
                id,
            }),

            (Some(_), Some(_)) => Err(D::Error::custom(
                "only ONE of `error` and `result` may be present",
            )),
            (None, None) => Err(D::Error::custom("must have an `error` or `result` member")),

            // we expect this case to error
            (Some(None), None) => Ok(Response {
                result: Ok(T::deserialize(serde::de::value::UnitDeserializer::new())?),
                id,
            }),
        }
    }
}

#[test]
fn response() {
    do_test::<Response<(), ()>>(
        Response {
            result: Ok(()),
            id: Id::Null,
        },
        json!({
            "jsonrpc": "2.0",
            "result": null,
            "id": null
        }),
    );
    do_test::<Response>(
        Response {
            result: Ok(Value::Null),
            id: Id::Null,
        },
        json!({
            "jsonrpc": "2.0",
            "result": null,
            "id": null
        }),
    );
}

/// A `JSON-RPC 2.0` error object.
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Default, Deserialize)]
pub struct Error<T = Value> {
    /// > A Number that indicates the error type that occurred.
    /// > This MUST be an integer.
    ///
    /// See the associated constants for error types defined by the specification.
    pub code: i64,
    /// > A String providing a short description of the error.
    /// > The message SHOULD be limited to a concise single sentence.
    pub message: String,
    /// > A Primitive or Structured value that contains additional information about the error.
    /// > This may be omitted.
    /// > The value of this member is defined by the Server
    /// > (e.g. detailed error information, nested errors etc.).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<T>,
}

macro_rules! error_code_and_ctor {
    (
        $(
            $(#[doc = $doc:literal])*
            $const_name:ident / $ctor_name:ident = $number:literal;
        )*
    ) => {

        impl Error {
            $(
                $(#[doc = $doc])*
                pub const $const_name: i64 = $number;
            )*

        }

        impl<T> Error<T> {

            $(
                #[doc = concat!("Convenience method for creating a new error with code [`Self::", stringify!($const_name), "`]")]
                pub fn $ctor_name(message: impl Display, data: impl Into<Option<T>>) -> Self {
                    Self::new(Error::$const_name, message, data)
                }
            )*

            /// If [`Self::code`] is one of the predefined errors in the spec,
            /// get its associated error message.
            pub const fn spec_message(&self) -> Option<&'static str> {
                match self.code {
                    $(
                        Error::$const_name => {
                            const LIMBS: &[&'static str] = &[
                                $($doc),*
                                ];
                                const LIMB: &str = LIMBS[0];
                                const MESSAGE: &str = {
                                    let (_quot, rest) = LIMB.as_bytes().split_at(2);
                                    match std::str::from_utf8(rest) {
                                        Ok(it) => it,
                                        Err(_) => panic!()
                                    }
                                };
                                Some(MESSAGE)
                            },
                        )*
                        _ => None
                    }
                }
            }
        }
    }

error_code_and_ctor! {
    /// > Invalid JSON was received by the server.
    /// > An error occurred on the server while parsing the JSON text.
    PARSE_ERROR / parse_error = -32700;
    /// > The JSON sent is not a valid Request object.
    INVALID_REQUEST / invalid_request = -32600;
    /// > The method does not exist / is not available.
    METHOD_NOT_FOUND / method_not_found = -32601;
    /// > Invalid method parameter(s).
    INVALID_PARAMS / invalid_params = -32602;
    /// > Internal JSON-RPC error.
    INTERNAL_ERROR / internal_error = -32603;
}

impl Error {
    /// > Reserved for implementation-defined server-errors.
    pub const SERVER_ERROR_RANGE: RangeInclusive<i64> = -32099..=-32000;
}

impl<T> Error<T> {
    /// Convenience method for creating a new error.
    pub fn new(code: i64, message: impl Display, data: impl Into<Option<T>>) -> Self {
        Self {
            code,
            message: message.to_string(),
            data: data.into(),
        }
    }
}

impl<T> fmt::Display for Error<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("code {}", self.code))?;
        if let Some(e) = self.spec_message() {
            f.write_fmt(format_args!(" ({})", e))?
        };
        if !self.message.is_empty() {
            f.write_fmt(format_args!(": {}", self.message))?
        }
        Ok(())
    }
}

impl<T> std::error::Error for Error<T> where T: fmt::Debug {}

#[derive(Serialize, Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(
    untagged,
    expecting = "a single response object, or an Array of batched response objects"
)]
/// A response to a [`MaybeBatchedRequest`].
pub enum MaybeBatchedResponse<T> {
    Single(Response<T>),
    Batch(Vec<Response<T>>),
}

/// > To send several Request objects at the same time, the Client MAY send an Array filled with Request objects.
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(
    untagged,
    expecting = "a single request object, or an Array of batched request objects"
)]
pub enum MaybeBatchedRequest<T> {
    Single(Request<T>),
    Batch(Vec<Request<T>>),
}

#[cfg(test)]
use serde_json::json;

#[cfg(test)]
#[track_caller]
fn do_test<T>(expected: T, json: Value)
where
    T: PartialEq + core::fmt::Debug + serde::de::DeserializeOwned + Serialize,
{
    assert_eq!(
        expected,
        serde_json::from_value(json.clone()).expect("deserialization failed"),
        "deserialization mismatch"
    );
    assert_eq!(
        serde_json::to_value(expected).expect("serialization failed"),
        json,
        "serialization mismatch"
    );
}

mod params {
    use crate::{map, RequestParameters};
    use serde::de::{
        value::{MapDeserializer, SeqDeserializer},
        IntoDeserializer,
    };
    use std::fmt;

    /// [`Deserializer`](serde::Deserializer) implementation for [`RequestParameters`].
    ///
    /// i.e given a [`RequestParameters`], you may deserialize an
    /// `impl` [`Deserialize`](serde::Deserialize) from it.
    ///
    /// ```
    /// # use ez_jsonrpc_types::RequestParameters;
    /// # use serde_json::json;
    /// # use serde::{de::IntoDeserializer as _, Deserialize as _};
    /// let params = RequestParameters::ByPosition(vec![json!(1), json!("two")]);
    /// let deserialized = <(i64, String)>::deserialize(params.into_deserializer()).unwrap();
    /// assert_eq!(deserialized, (1, String::from("two")));
    /// ```
    pub struct Deserializer<'de, T, E> {
        inner: _Deserializer<'de, T, E>,
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
}
