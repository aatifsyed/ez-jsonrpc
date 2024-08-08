use crate::types::{map::Map, RequestParameters};
use core::fmt::{self, Display};
use serde::ser::{Error as _, Impossible, Serialize};
use serde_json::{to_value, Value};

type Result<T, E = Error> = std::result::Result<T, E>;

/// Error when serializing to [`RequestParameters`] using a [`Serializer`].
#[derive(Debug)]
pub struct Error {
    inner: ErrorInner,
}

#[derive(Debug)]
enum ErrorInner {
    UnsupportedType(&'static str),
    Json(serde_json::Error),
}

impl Error {
    fn unsupported_type(t: &'static str) -> Self {
        Self {
            inner: ErrorInner::UnsupportedType(t),
        }
    }
    pub(crate) fn json(e: serde_json::Error) -> Self {
        Self {
            inner: ErrorInner::Json(e),
        }
    }
}

macro_rules! unsupported_types {
    ($($method:ident: $ty:ty);* $(;)?) => {
        $(
            #[inline]
            fn $method(self, _: $ty) -> Result<Self::Ok> {
                Err(Error::unsupported_type(stringify!($ty)))
            }
        )*
    };
}

macro_rules! tri {
    ($expr:expr) => {
        match $expr {
            Ok(it) => it,
            Err(e) => return Err(Error::json(e)),
        }
    };
}

impl serde::ser::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        Self::json(serde_json::Error::custom(msg))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner {
            ErrorInner::UnsupportedType(it) => f.write_fmt(format_args!(
                "unsupported type {}, request parameters must be an Array or an Object",
                it
            )),
            ErrorInner::Json(j) => j.fmt(f),
        }
    }
}
impl std::error::Error for Error {}

/// Serializer whose output is a [`RequestParameters`].
///
/// You may also be interested in [`ser::ByPosition`](crate::params::ser::ByPosition)
/// or [`ser::ByName`](crate::params::ser::ByName).
///
/// Errors on items which are not serializable as an `Array` or `Object`.
pub struct Serializer;

impl serde::Serializer for Serializer {
    type Ok = RequestParameters;
    type Error = Error;

    type SerializeSeq = SerializeVec;
    type SerializeTuple = SerializeVec;
    type SerializeTupleStruct = SerializeVec;
    type SerializeTupleVariant = SerializeTupleVariant;
    type SerializeMap = SerializeMap;
    type SerializeStruct = SerializeMap;
    type SerializeStructVariant = SerializeStructVariant;

    unsupported_types! {
        serialize_bool: bool;
        serialize_i8: i8;
        serialize_i16: i16;
        serialize_i32: i32;
        serialize_i64: i64;
        serialize_i128: i128;
        serialize_u8: u8;
        serialize_u16: u16;
        serialize_u32: u32;
        serialize_u64: u64;
        serialize_u128: u128;
        serialize_f32: f32;
        serialize_f64: f64;
        serialize_char: char;
        serialize_str: &str;
        serialize_bytes: &[u8];
    }

    #[inline]
    fn serialize_unit(self) -> Result<Self::Ok> {
        Err(Error::unsupported_type("unit"))
    }

    #[inline]
    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok> {
        self.serialize_unit()
    }

    #[inline]
    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok> {
        self.serialize_str(variant)
    }

    #[inline]
    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<Self::Ok>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    #[inline]
    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok>
    where
        T: ?Sized + Serialize,
    {
        let mut values = Map::new();
        values.insert(String::from(variant), tri!(to_value(value)));
        Ok(RequestParameters::ByName(values))
    }

    #[inline]
    fn serialize_none(self) -> Result<Self::Ok> {
        self.serialize_unit()
    }

    #[inline]
    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
        Ok(SerializeVec {
            vec: Vec::with_capacity(len.unwrap_or(0)),
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        Ok(SerializeTupleVariant {
            name: String::from(variant),
            vec: Vec::with_capacity(len),
        })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(SerializeMap::Map {
            map: Map::new(),
            next_key: None,
        })
    }

    fn serialize_struct(self, _: &'static str, len: usize) -> Result<Self::SerializeStruct> {
        self.serialize_map(Some(len))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        Ok(SerializeStructVariant {
            name: String::from(variant),
            map: serde_json::Map::new(),
        })
    }

    fn collect_str<T>(self, _: &T) -> Result<Self::Ok>
    where
        T: ?Sized + Display,
    {
        Err(Error::unsupported_type("string"))
    }
}

pub struct SerializeVec {
    vec: Vec<Value>,
}

pub struct SerializeTupleVariant {
    name: String,
    vec: Vec<Value>,
}

pub enum SerializeMap {
    Map {
        map: Map<Value>,
        next_key: Option<String>,
    },
}

pub struct SerializeStructVariant {
    name: String,
    map: serde_json::Map<String, Value>,
}

impl serde::ser::SerializeSeq for SerializeVec {
    type Ok = RequestParameters;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.vec.push(tri!(to_value(value)));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        Ok(RequestParameters::ByPosition(self.vec))
    }
}

impl serde::ser::SerializeTuple for SerializeVec {
    type Ok = RequestParameters;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        serde::ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok> {
        serde::ser::SerializeSeq::end(self)
    }
}

impl serde::ser::SerializeTupleStruct for SerializeVec {
    type Ok = RequestParameters;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        serde::ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok> {
        serde::ser::SerializeSeq::end(self)
    }
}

impl serde::ser::SerializeTupleVariant for SerializeTupleVariant {
    type Ok = RequestParameters;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.vec.push(tri!(to_value(value)));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        let mut object = Map::new();

        object.insert(self.name, Value::Array(self.vec));

        Ok(RequestParameters::ByName(object))
    }
}

impl serde::ser::SerializeMap for SerializeMap {
    type Ok = RequestParameters;
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        match self {
            SerializeMap::Map { next_key, .. } => {
                *next_key = Some(key.serialize(MapKeySerializer)?);
                Ok(())
            }
        }
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        match self {
            SerializeMap::Map { map, next_key } => {
                let key = next_key.take();
                // Panic because this indicates a bug in the program rather than an
                // expected failure.
                let key = key.expect("serialize_value called before serialize_key");
                map.insert(key, tri!(to_value(value)));
                Ok(())
            }
        }
    }

    fn end(self) -> Result<RequestParameters> {
        match self {
            SerializeMap::Map { map, .. } => Ok(RequestParameters::ByName(map)),
        }
    }
}

pub(crate) struct MapKeySerializer;

fn key_must_be_a_string() -> Error {
    Error::custom("key must be a string")
}

fn float_key_must_be_finite() -> Error {
    Error::custom("float key must be finite")
}

impl serde::Serializer for MapKeySerializer {
    type Ok = String;
    type Error = Error;

    type SerializeSeq = Impossible<String, Error>;
    type SerializeTuple = Impossible<String, Error>;
    type SerializeTupleStruct = Impossible<String, Error>;
    type SerializeTupleVariant = Impossible<String, Error>;
    type SerializeMap = Impossible<String, Error>;
    type SerializeStruct = Impossible<String, Error>;
    type SerializeStructVariant = Impossible<String, Error>;

    #[inline]
    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<String> {
        Ok(variant.to_owned())
    }

    #[inline]
    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<String>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_bool(self, value: bool) -> Result<String> {
        Ok(value.to_string())
    }

    fn serialize_i8(self, value: i8) -> Result<String> {
        Ok(value.to_string())
    }

    fn serialize_i16(self, value: i16) -> Result<String> {
        Ok(value.to_string())
    }

    fn serialize_i32(self, value: i32) -> Result<String> {
        Ok(value.to_string())
    }

    fn serialize_i64(self, value: i64) -> Result<String> {
        Ok(value.to_string())
    }

    fn serialize_u8(self, value: u8) -> Result<String> {
        Ok(value.to_string())
    }

    fn serialize_u16(self, value: u16) -> Result<String> {
        Ok(value.to_string())
    }

    fn serialize_u32(self, value: u32) -> Result<String> {
        Ok(value.to_string())
    }

    fn serialize_u64(self, value: u64) -> Result<String> {
        Ok(value.to_string())
    }

    fn serialize_f32(self, value: f32) -> Result<String> {
        if value.is_finite() {
            Ok(ryu::Buffer::new().format_finite(value).to_owned())
        } else {
            Err(float_key_must_be_finite())
        }
    }

    fn serialize_f64(self, value: f64) -> Result<String> {
        if value.is_finite() {
            Ok(ryu::Buffer::new().format_finite(value).to_owned())
        } else {
            Err(float_key_must_be_finite())
        }
    }

    #[inline]
    fn serialize_char(self, value: char) -> Result<String> {
        Ok({
            let mut s = String::new();
            s.push(value);
            s
        })
    }

    #[inline]
    fn serialize_str(self, value: &str) -> Result<String> {
        Ok(value.to_owned())
    }

    fn serialize_bytes(self, _value: &[u8]) -> Result<String> {
        Err(key_must_be_a_string())
    }

    fn serialize_unit(self) -> Result<String> {
        Err(key_must_be_a_string())
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<String> {
        Err(key_must_be_a_string())
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<String>
    where
        T: ?Sized + Serialize,
    {
        Err(key_must_be_a_string())
    }

    fn serialize_none(self) -> Result<String> {
        Err(key_must_be_a_string())
    }

    fn serialize_some<T>(self, _value: &T) -> Result<String>
    where
        T: ?Sized + Serialize,
    {
        Err(key_must_be_a_string())
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        Err(key_must_be_a_string())
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        Err(key_must_be_a_string())
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        Err(key_must_be_a_string())
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        Err(key_must_be_a_string())
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Err(key_must_be_a_string())
    }

    fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        Err(key_must_be_a_string())
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        Err(key_must_be_a_string())
    }

    fn collect_str<T>(self, value: &T) -> Result<String>
    where
        T: ?Sized + Display,
    {
        Ok(value.to_string())
    }
}

impl serde::ser::SerializeStruct for SerializeMap {
    type Ok = RequestParameters;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        match self {
            SerializeMap::Map { .. } => serde::ser::SerializeMap::serialize_entry(self, key, value),
        }
    }

    fn end(self) -> Result<Self::Ok> {
        match self {
            SerializeMap::Map { .. } => serde::ser::SerializeMap::end(self),
        }
    }
}

impl serde::ser::SerializeStructVariant for SerializeStructVariant {
    type Ok = RequestParameters;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.map.insert(String::from(key), tri!(to_value(value)));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        let mut object = Map::new();

        object.insert(self.name, Value::Object(self.map));

        Ok(RequestParameters::ByName(object))
    }
}
