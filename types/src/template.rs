//! Generic structs where you can customize, e.g zerocopy deserialization.
//!
//! You SHOULD take care that your custom types also match the specification when (de)serialized
//!
//! ```
//! let source = r#"
//!     { "jsonrpc": "2.0", "method": "hello" }
//! "#;
//! let request = serde_json::from_str::<ez_jsonrpc_types::template::Request<&str>>(source).unwrap();
//!                                       // `method` borrows from the input ^^^^
//! let method: &str = request.method; // it works!
//! assert_eq!(method, "hello");
//! ```

use crate::map::Map;
use serde::{
    de::{self, Error as _, Unexpected},
    Deserialize, Serialize,
};
use serde_json::{Number, Value};
use std::{borrow::Cow, fmt, hash::Hash, ops::RangeInclusive, str::FromStr};

/// A `JSON-RPC 2.0` request object.
///
/// Note that the `"jsonrpc": "2.0"` member is transparently checked during
/// deserialization, and added during serialization.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Request<MethodT = String, IdT = Id, RequestParametersT = RequestParameters<Value>> {
    /// > A String containing the name of the method to be invoked.
    /// > Method names that begin with the word rpc followed by a period character
    /// > (U+002E or ASCII 46) are reserved for rpc-internal methods and extensions
    /// > and MUST NOT be used for anything else.
    pub method: MethodT,
    /// > A Structured value that holds the parameter values to be used during the
    /// > invocation of the method.
    /// > This member MAY be omitted.
    pub params: Option<RequestParametersT>,
    /// > An identifier established by the Client that MUST contain a String,
    /// > Number, or NULL value if included.
    /// > If it is not included it is assumed to be a notification.
    /// > The value SHOULD normally not be Null and Numbers SHOULD NOT contain fractional parts
    pub id: Option<IdT>,
}

impl<MethodT: Default, IdT, RequestParametersT> Default
    for Request<MethodT, IdT, RequestParametersT>
{
    fn default() -> Self {
        Self {
            method: Default::default(),
            params: Default::default(),
            id: Default::default(),
        }
    }
}

impl<MEthodT, ParametersT, IdT> Serialize for Request<MEthodT, ParametersT, IdT>
where
    MEthodT: Serialize,
    ParametersT: Serialize,
    IdT: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(Serialize)]
        struct _Request<MethodT, IdT, RequestParametersT> {
            jsonrpc: V2,
            method: MethodT,
            #[serde(skip_serializing_if = "Option::is_none")]
            params: Option<RequestParametersT>,
            #[serde(skip_serializing_if = "Option::is_none")]
            id: Option<IdT>,
        }
        let Self { method, params, id } = self;
        _Request {
            jsonrpc: V2,
            method,
            params: params.as_ref(),
            id: id.as_ref(),
        }
        .serialize(serializer)
    }
}

impl<'de, MethodT, IdT, RequestParametersT> Deserialize<'de>
    for Request<MethodT, IdT, RequestParametersT>
where
    MethodT: Deserialize<'de>,
    IdT: Deserialize<'de>,
    RequestParametersT: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(bound(deserialize = "
            IdT: Deserialize<'de>,
            MethodT: Deserialize<'de>,
            RequestParametersT: Deserialize<'de>
        "))]
        struct _Request<MethodT, IdT, RequestParametersT> {
            jsonrpc: V2,
            method: MethodT,
            params: Option<RequestParametersT>,
            #[serde(deserialize_with = "deserialize_some", default)]
            id: Option<IdT>,
        }
        let _Request {
            method,
            params,
            id,
            jsonrpc: V2,
        } = _Request::deserialize(deserializer)?;
        Ok(Self { method, params, id })
    }
}

impl<T> Request<T> {
    pub const fn is_notification(&self) -> bool {
        self.id.is_none()
    }
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
pub enum RequestParameters<ValueT = Value> {
    /// > params MUST be an Array, containing the values in the Server expected order.
    ByPosition(Vec<ValueT>),
    /// > params MUST be an Object, with member names that match the Server
    /// > expected parameter names.
    /// > The absence of expected names MAY result in an error being generated.
    /// > The names MUST match exactly, including case, to the method's expected parameters.
    ByName(Map<ValueT>),
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

#[doc(inline)]
pub use crate::params::IntoDeserializer;

/// See [`Request::id`].
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Hash, Deserialize)]
#[serde(untagged, expecting = "a string, a number, or null")]
pub enum Id<StringT = String, NumberT = Number> {
    String(StringT),
    Number(NumberT),
    Null,
}

impl<StringT, NumberT> Default for Id<StringT, NumberT> {
    fn default() -> Self {
        Self::Null
    }
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
pub struct Response<ValueT = Value, ValueE = Value, StringT = String, IdT = Id> {
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
    pub result: Result<ValueT, Error<ValueE, StringT>>,
    /// > This member is REQUIRED.
    /// > It MUST be the same as the value of the id member in the Request Object.
    /// > If there was an error in detecting the id in the Request object
    /// > (e.g. Parse error/Invalid Request), it MUST be Null.
    pub id: IdT,
}

impl<'de, ValueT, ValueE, StringT, IdT> Deserialize<'de> for Response<ValueT, ValueE, StringT, IdT>
where
    ValueT: Deserialize<'de>,
    ValueE: Deserialize<'de>,
    StringT: Deserialize<'de>,
    IdT: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(bound(deserialize = "
            ValueT: Deserialize<'de>,
            ValueE: Deserialize<'de>,
            StringT: Deserialize<'de>,
            IdT: Deserialize<'de>,
        "))]
        struct _Response<ValueT, ValueE, StringT, IdT> {
            jsonrpc: V2,
            #[serde(default, deserialize_with = "deserialize_some")]
            result: Option<Option<ValueT>>,
            #[serde(default, deserialize_with = "deserialize_some")]
            error: Option<Error<ValueE, StringT>>,
            id: IdT,
        }
        let _Response {
            jsonrpc: V2,
            result,
            error,
            id,
        } = _Response::deserialize(deserializer)?;

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
                result: Ok(ValueT::deserialize(
                    serde::de::value::UnitDeserializer::new(),
                )?),
                id,
            }),
        }
    }
}

impl<ValueT, ValueE, StringT, IdT> Serialize for Response<ValueT, ValueE, StringT, IdT>
where
    ValueT: Serialize,
    ValueE: Serialize,
    StringT: Serialize,
    IdT: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(Serialize)]
        struct _Response<ValueT, ValueE, StringT, IdT> {
            jsonrpc: V2,
            #[serde(skip_serializing_if = "Option::is_none")]
            result: Option<Option<ValueT>>,
            #[serde(skip_serializing_if = "Option::is_none")]
            error: Option<Error<ValueE, StringT>>,
            id: IdT,
        }
        let Self { result, id } = self;
        match result {
            Ok(it) => _Response {
                jsonrpc: V2,
                result: Some(Some(it)),
                error: None,
                id,
            },
            Err(Error {
                code,
                message,
                data,
            }) => _Response {
                jsonrpc: V2,
                result: None,
                error: Some(Error {
                    code: *code,
                    message,
                    data: data.as_ref(),
                }),
                id,
            },
        }
        .serialize(serializer)
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

/// A `JSON-RPC 2.0` error object.
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct Error<ValueT = Value, StringT = String> {
    /// > A Number that indicates the error type that occurred.
    /// > This MUST be an integer.
    ///
    /// See the associated constants for error types defined by the specification.
    pub code: i64,
    /// > A String providing a short description of the error.
    /// > The message SHOULD be limited to a concise single sentence.
    pub message: StringT,
    /// > A Primitive or Structured value that contains additional information about the error.
    /// > This may be omitted.
    /// > The value of this member is defined by the Server
    /// > (e.g. detailed error information, nested errors etc.).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<ValueT>,
}

impl<ValueT, StringT: Default> Default for Error<ValueT, StringT> {
    fn default() -> Self {
        Self {
            code: Default::default(),
            message: Default::default(),
            data: Default::default(),
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

        impl Error {
            $(
                $(#[doc = $doc])*
                pub const $const_name: i64 = $number;
            )*

        }

        impl Error {

            $(
                #[doc = concat!("Convenience method for creating a new error with code [`Self::", stringify!($const_name), "`]")]
                pub fn $ctor_name(message: impl fmt::Display, data: impl Into<Option<Value>>) -> Self {
                    Self::new(Error::$const_name, message, data)
                }
            )*
        }

        impl<ValueT, StringT> Error<ValueT, StringT> {
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

impl Error {
    /// Convenience method for creating a new error.
    pub fn new(code: i64, message: impl fmt::Display, data: impl Into<Option<Value>>) -> Self {
        Self {
            code,
            message: message.to_string(),
            data: data.into(),
        }
    }
}

impl<ValueT, StringT> fmt::Display for Error<ValueT, StringT>
where
    StringT: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("code {}", self.code))?;
        if let Some(e) = self.spec_message() {
            f.write_fmt(format_args!(" ({})", e))?
        };
        f.write_fmt(format_args!(": `{}`", self.message))
    }
}

impl<ValueT, StringT> std::error::Error for Error<ValueT, StringT>
where
    StringT: fmt::Display + fmt::Debug,
    ValueT: fmt::Debug,
{
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(
    untagged,
    expecting = "a single response object, or an Array of batched response objects"
)]
/// A response to a [`MaybeBatchedRequest`].
pub enum MaybeBatchedResponse<ValueT = Value, ValueE = Value, StringT = String, IdT = Id> {
    Single(Response<ValueT, ValueE, StringT, IdT>),
    Batch(Vec<Response<ValueT, ValueE, StringT, IdT>>),
}

/// > To send several Request objects at the same time, the Client MAY send an Array filled with Request objects.
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(
    untagged,
    expecting = "a single request object, or an Array of batched request objects"
)]
pub enum MaybeBatchedRequest<MethodT = String, IdT = Id, RequestParametersT = RequestParameters> {
    Single(Request<MethodT, IdT, RequestParametersT>),
    Batch(Vec<Request<MethodT, IdT, RequestParametersT>>),
}
