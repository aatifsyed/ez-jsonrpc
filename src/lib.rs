//! A transcription of types from the [`JSON-RPC 2.0` Specification](https://www.jsonrpc.org/specification).
//!
//! > When quoted, the specification will appear as blockquoted text, like so.

use std::{borrow::Cow, fmt::Display, ops::RangeInclusive, str::FromStr};

use serde::{
    de::{Error as _, Unexpected},
    Deserialize, Serialize,
};
use serde_json::{Map, Number, Value};

/// A `JSON-RPC 2.0` request object.
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Default)]
pub struct Request<'a, T = RequestParameters> {
    /// > A String specifying the version of the JSON-RPC protocol.
    /// > MUST be exactly "2.0".
    pub jsonrpc: V2,
    /// > A String containing the name of the method to be invoked.
    /// > Method names that begin with the word rpc followed by a period character
    /// > (U+002E or ASCII 46) are reserved for rpc-internal methods and extensions
    /// > and MUST NOT be used for anything else.
    pub method: Cow<'a, str>,
    /// > A Structured value that holds the parameter values to be used during the
    /// > invocation of the method.
    /// > This member MAY be omitted.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<T>,
    /// > An identifier established by the Client that MUST contain a String,
    /// > Number, or NULL value if included.
    /// > If it is not included it is assumed to be a notification.
    /// > The value SHOULD normally not be Null and Numbers SHOULD NOT contain fractional parts
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<Id<'a>>,
}

impl<T> Request<'_, T> {
    pub fn is_notification(&self) -> bool {
        self.id.is_none()
    }
}

impl<'a, 'de> Deserialize<'de> for Request<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct Helper<'a> {
            jsonrpc: V2,
            method: Cow<'a, str>,
            #[serde(default, deserialize_with = "deserialize_some")]
            params: Option<Option<RequestParameters>>,
            #[serde(default, deserialize_with = "deserialize_some")]
            id: Option<Option<Id<'a>>>,
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

#[test]
fn request() {
    do_test(
        Request {
            jsonrpc: V2,
            method: "myMethod".into(),
            params: None,
            id: None,
        },
        json!({
            "jsonrpc": "2.0",
            "method": "myMethod",
        }),
    );
    do_test(
        Request {
            jsonrpc: V2,
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
pub enum Id<'a> {
    String(Cow<'a, str>),
    Number(Number),
    #[default]
    Null,
}

impl FromStr for Id<'static> {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl<'a> Id<'a> {
    pub fn into_owned(self) -> Id<'static> {
        match self {
            Id::String(it) => Id::String(Cow::Owned(it.into_owned())),
            Id::Number(it) => Id::Number(it),
            Id::Null => Id::Null,
        }
    }
}

/// A `JSON-RPC 2.0` response object.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Response<'a, T = Value, E = Value> {
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
    pub result: Result<T, Error<'a, E>>,
    /// > This member is REQUIRED.
    /// > It MUST be the same as the value of the id member in the Request Object.
    /// > If there was an error in detecting the id in the Request object
    /// > (e.g. Parse error/Invalid Request), it MUST be Null.
    pub id: Id<'a>,
}

impl<'a> Default for Response<'a> {
    fn default() -> Self {
        Self {
            jsonrpc: Default::default(),
            result: Ok(Default::default()),
            id: Default::default(),
        }
    }
}

impl<'a, T, E> Response<'a, T, E> {
    pub fn into_owned(self) -> Response<'static, T, E> {
        let Self {
            jsonrpc,
            result,
            id,
        } = self;
        Response {
            jsonrpc,
            result: result.map_err(Error::into_owned),
            id: id.into_owned(),
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(bound(deserialize = "T: Deserialize<'de>, E: Deserialize<'de>"))]
struct RawResponseDeSer<'a, T, E> {
    jsonrpc: V2,
    #[serde(
        default,
        deserialize_with = "deserialize_some",
        skip_serializing_if = "Option::is_none"
    )]
    result: Option<Option<T>>,
    #[serde(
        default,
        deserialize_with = "deserialize_some",
        skip_serializing_if = "Option::is_none"
    )]
    error: Option<Error<'a, E>>,
    id: Cow<'a, Id<'a>>,
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

impl<'a, T, E> Serialize for Response<'a, T, E>
where
    T: Serialize,
    E: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let Self {
            jsonrpc,
            result,
            id,
        } = self;

        let helper = match result {
            Ok(result) => RawResponseDeSer {
                jsonrpc: *jsonrpc,
                result: Some(Some(result)),
                error: None,
                id: Cow::Borrowed(id),
            },
            Err(Error {
                code,
                message,
                data,
            }) => RawResponseDeSer {
                jsonrpc: *jsonrpc,
                result: None,
                error: Some(Error {
                    code: *code,
                    message: Cow::Borrowed(message),
                    data: data.as_ref(),
                }),
                id: Cow::Borrowed(id),
            },
        };
        helper.serialize(serializer)
    }
}

impl<'a, 'de, T, E> Deserialize<'de> for Response<'a, T, E>
where
    T: Deserialize<'de>,
    E: Deserialize<'de>,
{
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
            (Some(Some(ok)), None) => Ok(Response {
                jsonrpc,
                result: Ok(ok),
                id: id.into_owned(),
            }),
            (None, Some(err)) => Ok(Response {
                jsonrpc,
                result: Err(err),
                id: id.into_owned(),
            }),

            (Some(_), Some(_)) => Err(D::Error::custom(
                "only ONE of `error` and `result` may be present",
            )),
            (None, None) => Err(D::Error::custom("must have an `error` or `result` member")),

            // we expect this case to error
            (Some(None), None) => Ok(Response {
                jsonrpc,
                result: Ok(T::deserialize(serde::de::value::UnitDeserializer::new())?),
                id: id.into_owned(),
            }),
        }
    }
}

#[test]
fn test() {
    do_test::<Response<(), ()>>(
        Response {
            jsonrpc: V2,
            result: Ok(()),
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
pub struct Error<'a, T = Value> {
    /// > A Number that indicates the error type that occurred.
    /// > This MUST be an integer.
    ///
    /// See the associated constants for error types defined by the specification.
    pub code: i64,
    /// > A String providing a short description of the error.
    /// > The message SHOULD be limited to a concise single sentence.
    pub message: Cow<'a, str>,
    /// > A Primitive or Structured value that contains additional information about the error.
    /// > This may be omitted.
    /// > The value of this member is defined by the Server
    /// > (e.g. detailed error information, nested errors etc.).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<T>,
}

impl<'a, T> Error<'a, T> {
    pub fn into_owned(self) -> Error<'static, T> {
        let Self {
            code,
            message,
            data,
        } = self;
        Error {
            code,
            message: Cow::Owned(message.into_owned()),
            data,
        }
    }
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

impl<'a> Error<'a> {
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
            message: message.to_string().into(),
            data: data.into(),
        }
    }
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(
    bound(deserialize = "'de: 'a"),
    untagged,
    expecting = "a single response object, or an Array of batched response objects"
)]
/// A response to a [`MaybeBatchedRequest`].
pub enum MaybeBatchedResponse<'a> {
    Single(Response<'a>),
    Batch(Vec<Response<'a>>),
}

/// > To send several Request objects at the same time, the Client MAY send an Array filled with Request objects.
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(
    bound(deserialize = "'de: 'a"),
    untagged,
    expecting = "a single request object, or an Array of batched request objects"
)]
pub enum MaybeBatchedRequest<'a> {
    Single(Request<'a>),
    Batch(Vec<Request<'a>>),
}

#[cfg(test)]
use serde_json::json;

#[cfg(test)]
#[track_caller]
fn do_test<T>(expected: T, json: Value)
where
    T: PartialEq + core::fmt::Debug + serde::de::DeserializeOwned + Serialize,
{
    assert_eq!(expected, serde_json::from_value(json.clone()).unwrap());
    assert_eq!(serde_json::to_value(expected).unwrap(), json);
}
