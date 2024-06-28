//! A transcription of types from the [`JSON-RPC 2.0` Specification](https://www.jsonrpc.org/specification).
//!
//! > When quoted, the specification will appear as blockquoted text, like so.

use std::{borrow::Cow, fmt::Display, ops::RangeInclusive, str::FromStr};

use serde::{
    de::{Error as _, Unexpected},
    Deserialize, Deserializer, Serialize,
};
use serde_json::{Map, Number, Value};

/// A `JSON-RPC 2.0` request object.
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Default)]
pub struct Request {
    /// > A String specifying the version of the JSON-RPC protocol.
    /// > MUST be exactly "2.0".
    pub jsonrpc: V2,
    /// > A String containing the name of the method to be invoked.
    /// > Method names that begin with the word rpc followed by a period character
    /// > (U+002E or ASCII 46) are reserved for rpc-internal methods and extensions
    /// > and MUST NOT be used for anything else.
    pub method: String,
    /// > A Structured value that holds the parameter values to be used during the
    /// > invocation of the method.
    /// > This member MAY be omitted.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<RequestParameters>,
    /// > An identifier established by the Client that MUST contain a String,
    /// > Number, or NULL value if included.
    /// > If it is not included it is assumed to be a notification.
    /// > The value SHOULD normally not be Null and Numbers SHOULD NOT contain fractional parts
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<Id>,
}

impl Request {
    pub fn is_notification(&self) -> bool {
        self.id.is_none()
    }
    /// Perform straightforward parameter deserialization.
    pub fn deserialize_params<'de, T>(self) -> serde_json::Result<T>
    where
        T: Deserialize<'de>,
    {
        struct RequestParametersDeserializer(Option<RequestParameters>);

        impl<'de> Deserializer<'de> for RequestParametersDeserializer {
            type Error = serde_json::Error;

            fn deserialize_any<V: serde::de::Visitor<'de>>(
                self,
                visitor: V,
            ) -> Result<V::Value, Self::Error> {
                match self.0 {
                    Some(RequestParameters::ByName(it)) => {
                        serde::de::value::MapDeserializer::new(it.into_iter())
                            .deserialize_any(visitor)
                    }
                    Some(RequestParameters::ByPosition(it)) => {
                        serde::de::value::SeqDeserializer::new(it.into_iter())
                            .deserialize_any(visitor)
                    }
                    None => serde::de::value::UnitDeserializer::new().deserialize_any(visitor),
                }
            }

            serde::forward_to_deserialize_any! {
                bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
                bytes byte_buf option unit unit_struct newtype_struct seq tuple
                tuple_struct map struct enum identifier ignored_any
            }
        }

        T::deserialize(RequestParametersDeserializer(self.params))
    }
}

impl<'de> Deserialize<'de> for Request {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct Helper {
            jsonrpc: V2,
            method: String,
            #[serde(default, deserialize_with = "deserialize_some")]
            params: Option<Option<RequestParameters>>,
            #[serde(default, deserialize_with = "deserialize_some")]
            id: Option<Option<Id>>,
        }
        let Helper {
            jsonrpc,
            method,
            params,
            id,
        } = Helper::deserialize(deserializer)?;
        Ok(Self {
            jsonrpc,
            method,
            params: match params {
                Some(Some(params)) => Some(params),
                // Be lenient in what we accept
                // Some(None) => return Err(D::Error::custom("`params` may not be `null`")),
                Some(None) => None,
                None => None,
            },
            id: match id {
                Some(Some(id)) => Some(id),
                Some(None) => Some(Id::Null),
                None => None,
            },
        })
    }
}

/// A witness of the literal string "2.0"
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct V2;

impl<'de> Deserialize<'de> for V2 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        match &*Cow::<str>::deserialize(deserializer)? {
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
pub enum RequestParameters {
    /// > params MUST be an Array, containing the values in the Server expected order.
    ByPosition(Vec<Value>),
    /// > params MUST be an Object, with member names that match the Server
    /// > expected parameter names.
    /// > The absence of expected names MAY result in an error being generated.
    /// > The names MUST match exactly, including case, to the method's expected parameters.
    ByName(Map<String, Value>),
}

impl RequestParameters {
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Response {
    /// > A String specifying the version of the JSON-RPC protocol.
    /// > MUST be exactly "2.0".
    pub jsonrpc: V2,
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
    pub result: Result<Value, Error>,
    /// > This member is REQUIRED.
    /// > It MUST be the same as the value of the id member in the Request Object.
    /// > If there was an error in detecting the id in the Request object
    /// > (e.g. Parse error/Invalid Request), it MUST be Null.
    pub id: Id,
}

impl Default for Response {
    fn default() -> Self {
        Self {
            jsonrpc: Default::default(),
            result: Ok(Default::default()),
            id: Default::default(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct RawResponseDeSer {
    jsonrpc: V2,
    #[serde(default, deserialize_with = "deserialize_some")]
    result: Option<Option<Value>>,
    #[serde(default)]
    error: Option<Error>,
    id: Id,
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

impl Serialize for Response {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let Self {
            jsonrpc,
            result,
            id,
        } = self.clone();
        let helper = match result {
            Ok(result) => RawResponseDeSer {
                jsonrpc,
                result: Some(Some(result)),
                error: None,
                id,
            },
            Err(error) => RawResponseDeSer {
                jsonrpc,
                result: None,
                error: Some(error),
                id,
            },
        };
        helper.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Response {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let RawResponseDeSer {
            jsonrpc,
            error,
            result,
            id,
        } = RawResponseDeSer::deserialize(deserializer)?;
        match (result, error) {
            (Some(ok), None) => Ok(Response {
                jsonrpc,
                result: Ok(ok.unwrap_or_default()),
                id,
            }),
            (None, Some(err)) => Ok(Response {
                jsonrpc,
                result: Err(err),
                id,
            }),
            (Some(_), Some(_)) => Err(D::Error::custom(
                "only ONE of `error` and `result` may be present",
            )),
            (None, None) => Err(D::Error::custom("must have an `error` or `result` member")),
        }
    }
}

/// A `JSON-RPC 2.0` error object.
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Default)]
pub struct Error {
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
    pub data: Option<Value>,
}

macro_rules! error_code_and_ctor {
    (
        $(
            $(#[doc = $doc:literal])*
            $const_name:ident / $ctor_name:ident = $number:literal;
        )*
    ) => {
        $(
            $(#[doc = $doc])*
            pub const $const_name: i64 = $number;
        )*

        $(
            #[doc = concat!("Convenience method for creating a new error with code [`Self::", stringify!($const_name), "`]")]
            pub fn $ctor_name(message: impl Display, data: impl Into<Option<Value>>) -> Self {
                Self::new(Self::$const_name, message, data)
            }
        )*
    };
}

impl Error {
    error_code_and_ctor! {
            /// > Invalid JSON was received by the server. An error occurred on the server while parsing the JSON text.
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

    /// > Reserved for implementation-defined server-errors.
    pub const SERVER_ERROR_RANGE: RangeInclusive<i64> = -32099..=-32000;

    /// Convenience method for creating a new error.
    pub fn new(code: i64, message: impl Display, data: impl Into<Option<Value>>) -> Self {
        Self {
            code,
            message: message.to_string(),
            data: data.into(),
        }
    }
}

impl<'de> Deserialize<'de> for Error {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct Helper {
            code: i64,
            message: String,
            #[serde(default, deserialize_with = "deserialize_some")]
            data: Option<Option<Value>>,
        }
        let Helper {
            code,
            message,
            data,
        } = Helper::deserialize(deserializer)?;
        Ok(Self {
            code,
            message,
            data: match data {
                Some(Some(value)) => Some(value),
                Some(None) => Some(Value::Null),
                None => None,
            },
        })
    }
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(
    untagged,
    expecting = "a single response object, or an Array of batched response objects"
)]
/// A response to a [`MaybeBatchedRequest`].
pub enum MaybeBatchedResponse {
    Single(Response),
    Batch(Vec<Response>),
}

/// > To send several Request objects at the same time, the Client MAY send an Array filled with Request objects.
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(
    untagged,
    expecting = "a single request object, or an Array of batched request objects"
)]
pub enum MaybeBatchedRequest {
    Single(Request),
    Batch(Vec<Request>),
}
