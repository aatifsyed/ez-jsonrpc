use std::collections::BTreeMap;

use serde::{
    ser::{Error as _, Impossible},
    Serialize,
};
use serde_json::Value;

/// A [Serializer](serde::Serializer) which mimics [`serde_json::Serializer`].
///
/// # Panics
/// - if [`debug_assertions`](https://doc.rust-lang.org/std/macro.debug_assert.html) are enabled,
///   and the serialized type is not represented as a map with [`serde_json::Serializer`].
pub struct AssertNamed<S>(pub S);

macro_rules! unsupported_type {
    ($expr:expr) => {{
        let msg = concat!("unsupported type `", $expr, "`, expected a map");
        match cfg!(debug_assertions) {
            true => panic!("{}", msg),
            false => serde::ser::Error::custom(msg),
        }
    }};
}

macro_rules! unsupported_types {
    ($($method:ident: $ty:ty);* $(;)?) => {
        $(
            fn $method(self, _: $ty) -> Result<Self::Ok, Self::Error> {
                Err(unsupported_type!(stringify!($ty)))
            }
        )*
    };
}

impl<S> serde::Serializer for AssertNamed<S>
where
    S: serde::ser::SerializeMap,
{
    type Ok = S::Ok;
    type Error = S::Error;
    type SerializeSeq = Impossible<Self::Ok, Self::Error>;
    type SerializeTuple = Impossible<Self::Ok, Self::Error>;
    type SerializeTupleStruct = Impossible<Self::Ok, Self::Error>;
    type SerializeTupleVariant = SerializeTupleVariant<S>;
    type SerializeMap = S;
    type SerializeStruct = Self;
    type SerializeStructVariant = SerializeStructVariant<S>;

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

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(unsupported_type!("unit"))
    }
    fn serialize_unit_struct(self, _: &'static str) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit()
    }
    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(variant)
    }
    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }
    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        mut self,
        _: &'static str,
        _: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        // https://github.com/serde-rs/json/blob/3fd6f5f49dc1c732d9b1d7dfece4f02c0d440d39/src/ser.rs#L229
        self.0.serialize_entry(variant, value)?;
        self.0.end()
    }
    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit()
    }
    fn serialize_some<T: ?Sized + Serialize>(self, it: &T) -> Result<Self::Ok, Self::Error> {
        it.serialize(self)
    }
    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Err(unsupported_type!("sequence"))
    }
    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.serialize_seq(Some(len))
    }
    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Ok(SerializeTupleVariant {
            inner: self.0,
            buffer: Vec::with_capacity(len),
            name: variant,
        })
    }

    fn serialize_map(self, _: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(self.0)
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(self)
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Ok(SerializeStructVariant {
            inner: self.0,
            buffer: BTreeMap::new(),
            name: variant,
        })
    }
}

pub struct SerializeStructVariant<S> {
    inner: S,
    buffer: BTreeMap<&'static str, Value>,
    name: &'static str,
}
impl<S> serde::ser::SerializeStructVariant for SerializeStructVariant<S>
where
    S: serde::ser::SerializeMap,
{
    type Ok = S::Ok;
    type Error = S::Error;
    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.buffer
            .insert(key, serde_json::to_value(value).map_err(S::Error::custom)?);
        Ok(())
    }

    fn end(mut self) -> Result<Self::Ok, Self::Error> {
        self.inner.serialize_entry(self.name, &self.buffer)?;
        self.inner.end()
    }
}

pub struct SerializeTupleVariant<S> {
    inner: S,
    buffer: Vec<Value>,
    name: &'static str,
}

impl<S> serde::ser::SerializeTupleVariant for SerializeTupleVariant<S>
where
    S: serde::ser::SerializeMap,
{
    type Ok = S::Ok;
    type Error = S::Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.buffer
            .push(serde_json::to_value(value).map_err(S::Error::custom)?);
        Ok(())
    }

    fn end(mut self) -> Result<Self::Ok, Self::Error> {
        self.inner.serialize_entry(self.name, &self.buffer)?;
        self.inner.end()
    }
}

impl<S> serde::ser::SerializeMap for AssertNamed<S>
where
    S: serde::ser::SerializeMap,
{
    type Ok = S::Ok;
    type Error = S::Error;
    fn serialize_key<T: ?Sized + Serialize>(&mut self, key: &T) -> Result<(), Self::Error> {
        self.0.serialize_key(key)
    }

    fn serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.0.serialize_value(value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.0.end()
    }
}

impl<S> serde::ser::SerializeStruct for AssertNamed<S>
where
    S: serde::ser::SerializeMap,
{
    type Ok = S::Ok;
    type Error = S::Error;
    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.0.serialize_entry(key, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.0.end()
    }
}
impl<S> serde::ser::SerializeStructVariant for AssertNamed<S>
where
    S: serde::ser::SerializeMap,
{
    type Ok = S::Ok;
    type Error = S::Error;
    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.0.serialize_entry(key, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.0.end()
    }
}
