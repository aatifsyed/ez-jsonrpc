use core::fmt;
use std::io;

use serde::{ser::Impossible, Serialize};
use serde_json::ser::{CompactFormatter, Formatter, PrettyFormatter};

pub fn main() {
    #[derive(Serialize, Debug)]
    enum SerializeMe {
        UnitVariant,

        NewtypeVariantInt(usize),
        NewtypeVariantString(String),
        TupleVariant(usize, String),

        StructVariant { u: usize, s: String },
    }

    for serialize_me in [
        SerializeMe::UnitVariant,
        SerializeMe::NewtypeVariantInt(42),
        SerializeMe::NewtypeVariantString(String::from("hello")),
        SerializeMe::TupleVariant(42, String::from("حرروا فلسطين")),
        SerializeMe::StructVariant {
            u: 42,
            s: String::from("حرروا فلسطين"),
        },
    ] {
        let mut s = Serializer::pretty(vec![], Some(1.into()));
        serialize_me.serialize(&mut s).unwrap();
        let ser = String::from_utf8(s.into_inner()).unwrap();
        println!("{serialize_me:?}: {ser}");
    }
}

pub struct Serializer<W, IdT = ez_jsonrpc::Id, F = CompactFormatter> {
    wrt: W,
    id: Option<IdT>,
    fmt: F,
}

impl<W> Serializer<W> {
    pub fn new(wrt: W, id: Option<ez_jsonrpc::Id>) -> Self {
        Self {
            wrt,
            id,
            fmt: CompactFormatter,
        }
    }
    pub fn pretty<'a>(
        wrt: W,
        id: Option<ez_jsonrpc::Id>,
    ) -> Serializer<W, ez_jsonrpc::Id, PrettyFormatter<'a>> {
        Serializer {
            wrt,
            id,
            fmt: PrettyFormatter::new(),
        }
    }
}

impl<W, IdT, F> Serializer<W, IdT, F> {
    pub fn with_formatter(wrt: W, id: Option<IdT>, fmt: F) -> Self {
        Self { wrt, id, fmt }
    }

    pub fn into_inner(self) -> W {
        self.wrt
    }
}

impl<W: io::Write, IdT, F: Formatter> Serializer<W, IdT, F> {
    fn begin_outer(&mut self, method: impl Serialize) -> Result<(), Error>
    where
        IdT: Serialize,
    {
        let Self { wrt, id, fmt } = self;

        fmt.begin_object(wrt)?;

        // "jsonrpc": "2.0"
        {
            fmt.begin_object_key(wrt, true)?;
            fmt.begin_string(wrt)?;
            fmt.write_string_fragment(wrt, "jsonrpc")?;
            fmt.end_string(wrt)?;
            fmt.end_object_key(wrt)?;

            fmt.begin_object_value(wrt)?;
            fmt.begin_string(wrt)?;
            fmt.write_string_fragment(wrt, "2.0")?;
            fmt.end_string(wrt)?;
            fmt.end_object_value(wrt)?;
        }

        // "id": ...
        if let Some(id) = id.take() {
            fmt.begin_object_key(wrt, true)?;
            fmt.begin_string(wrt)?;
            fmt.write_string_fragment(wrt, "id")?;
            fmt.end_string(wrt)?;
            fmt.end_object_key(wrt)?;

            fmt.begin_object_value(wrt)?;
            id.serialize(&mut serde_json::Serializer::with_formatter(
                &mut *wrt,
                RefFormatter(fmt),
            ))?;
            fmt.end_object_value(wrt)?;
        }

        // "method": ...
        {
            fmt.begin_object_key(wrt, false)?;
            fmt.begin_string(wrt)?;
            fmt.write_string_fragment(wrt, "method")?;
            fmt.end_string(wrt)?;
            fmt.end_object_key(wrt)?;

            fmt.begin_object_value(wrt)?;
            method.serialize(&mut serde_json::Serializer::with_formatter(
                &mut *wrt,
                RefFormatter(fmt),
            ))?;
            fmt.end_object_value(wrt)?;
        }

        Ok(())
    }

    fn end_outer(&mut self) -> io::Result<()> {
        self.fmt.end_object(&mut self.wrt)
    }

    fn begin_params(&mut self) -> io::Result<()> {
        let Self { wrt, fmt, .. } = self;
        fmt.begin_object_key(wrt, false)?;
        fmt.begin_string(wrt)?;
        fmt.write_string_fragment(wrt, "params")?;
        fmt.end_string(wrt)?;

        fmt.begin_object_value(wrt)
    }
    fn end_params(&mut self) -> io::Result<()> {
        self.fmt.end_object_value(&mut self.wrt)
    }

    fn begin_positional_params<'a>(
        &'a mut self,
    ) -> Result<SerializePositional<'a, W, IdT, F>, Error> {
        self.begin_params()?;
        self.fmt.begin_array(&mut self.wrt)?;
        Ok(SerializePositional {
            inner: self,
            first: true,
        })
    }

    fn begin_named_params<'a>(&'a mut self) -> Result<SerializeNamed<'a, W, IdT, F>, Error> {
        self.begin_params()?;
        self.fmt.begin_object(&mut self.wrt)?;
        Ok(SerializeNamed {
            inner: self,
            first: true,
        })
    }
}

struct RefFormatter<'a, F>(&'a mut F);

macro_rules! forward {
    ($(
        $fn:ident($($arg:ident: $ty:ty),* $(,)?)
    );* $(;)?) => {$(
        #[inline(always)]
        fn $fn<W: ?Sized + io::Write>(&mut self, writer: &mut W, $($arg: $ty),*) -> io::Result<()> {
            self.0.$fn(writer, $($arg),*)
        }
    )*};
}

impl<'a, F: Formatter> Formatter for RefFormatter<'a, F> {
    forward! {
        write_null();
        write_bool(value: bool);
        write_i8(value: i8);
        write_i16(value: i16);
        write_i32(value: i32);
        write_i64(value: i64);
        write_i128(value: i128);
        write_u8(value: u8);
        write_u16(value: u16);
        write_u32(value: u32);
        write_u64(value: u64);
        write_u128(value: u128);
        write_f32(value: f32);
        write_f64(value: f64);
        write_number_str(value: &str);
        begin_string();
        end_string();
        write_string_fragment(fragment: &str);
        write_byte_array(value: &[u8]);
        begin_array();
        end_array();
        begin_array_value(first: bool);
        end_array_value();
        begin_object();
        end_object();
        begin_object_key(first: bool);
        end_object_key();
        begin_object_value();
        end_object_value();
        write_raw_fragment(fragment: &str);
        write_char_escape(char_escape: serde_json::ser::CharEscape);
    }
}

#[derive(Debug)]
pub enum Error {
    Json(serde_json::Error),
    Io(io::Error),
}

impl serde::ser::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Self::Json(serde_json::Error::custom(msg))
    }
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Json(e) => e.fmt(f),
            Error::Io(e) => e.fmt(f),
        }
    }
}
impl core::error::Error for Error {}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

impl From<serde_json::Error> for Error {
    fn from(value: serde_json::Error) -> Self {
        Self::Json(value)
    }
}

impl From<Error> for io::Error {
    fn from(value: Error) -> Self {
        match value {
            Error::Json(e) => e.into(),
            Error::Io(e) => e,
        }
    }
}

macro_rules! invalid {
    ($(
        $fn:ident$(<$ty_param:ident>)?($($arg:ty),* $(,)?) -> $ret:ty
    );*  $(;)?) => {$(
        fn $fn$(<$ty_param: ?Sized + Serialize>)?(self, $(_: $arg),*) -> Result<$ret, Self::Error> {
            Err(serde::ser::Error::custom(concat!("call to `", stringify!($ident), "` is incorrect")))
        }
    )*};
}

impl<'a, W: io::Write, IdT: Serialize, F: Formatter> serde::ser::Serializer
    for &'a mut Serializer<W, IdT, F>
{
    type Ok = ();
    type Error = Error;

    type SerializeSeq = Impossible<Self::Ok, Self::Error>;
    type SerializeTuple = Impossible<Self::Ok, Self::Error>;
    type SerializeMap = Impossible<Self::Ok, Self::Error>;

    invalid! {
        serialize_bool(bool) -> Self::Ok;
        serialize_i8(i8) -> Self::Ok;
        serialize_i16(i16) -> Self::Ok;
        serialize_i32(i32) -> Self::Ok;
        serialize_i64(i64) -> Self::Ok;
        serialize_u8(u8) -> Self::Ok;
        serialize_u16(u16) -> Self::Ok;
        serialize_u32(u32) -> Self::Ok;
        serialize_u64(u64) -> Self::Ok;
        serialize_f32(f32) -> Self::Ok;
        serialize_f64(f64) -> Self::Ok;
        serialize_char(char) -> Self::Ok;
        serialize_bytes(&[u8]) -> Self::Ok;
        serialize_none() -> Self::Ok;
        serialize_some<T>(&T) ->Self::Ok;
        serialize_unit() -> Self::Ok;
        serialize_seq(Option<usize>) -> Self::SerializeSeq;
        serialize_map(Option<usize>) -> Self::SerializeMap;
        serialize_tuple(usize) -> Self::SerializeTuple;
    }

    type SerializeTupleStruct = SerializePositional<'a, W, IdT, F>;
    type SerializeTupleVariant = SerializePositional<'a, W, IdT, F>;
    type SerializeStruct = SerializeNamed<'a, W, IdT, F>;
    type SerializeStructVariant = SerializeNamed<'a, W, IdT, F>;

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        self.begin_outer(v)?;
        self.end_outer()?;
        Ok(())
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.begin_outer(name)?;
        self.end_outer()?;
        Ok(())
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.begin_outer(variant)?;
        self.end_outer()?;
        Ok(())
    }

    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        self.begin_outer(name)?;
        let mut pos = self.begin_positional_params()?;
        pos.push_positional_param(value)?;
        pos.end_positional_params()
    }

    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        self.begin_outer(variant)?;
        let mut pos = self.begin_positional_params()?;
        pos.push_positional_param(value)?;
        pos.end_positional_params()
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.begin_outer(name)?;
        self.begin_positional_params()
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        self.begin_outer(variant)?;
        self.begin_positional_params()
    }

    fn serialize_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.begin_outer(name)?;
        self.begin_named_params()
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        self.begin_outer(variant)?;
        self.begin_named_params()
    }
}

pub struct SerializePositional<'a, W, IdT, F> {
    inner: &'a mut Serializer<W, IdT, F>,
    first: bool,
}

impl<'a, W: io::Write, IdT, F: Formatter> SerializePositional<'a, W, IdT, F> {
    fn push_positional_param(&mut self, value: impl Serialize) -> Result<(), Error> {
        let Self {
            inner: Serializer { wrt, fmt, .. },
            first,
        } = self;

        fmt.begin_array_value(wrt, *first)?;
        *first = false;
        value.serialize(&mut serde_json::Serializer::with_formatter(
            &mut *wrt,
            RefFormatter(fmt),
        ))?;
        fmt.end_array_value(wrt)?;
        Ok(())
    }
    fn end_positional_params(self) -> Result<(), Error> {
        self.inner.end_params()?;
        self.inner.fmt.end_array(&mut self.inner.wrt)?;
        self.inner.end_outer()?;
        Ok(())
    }
}

impl<'a, W: io::Write, IdT, F: Formatter> serde::ser::SerializeTupleStruct
    for SerializePositional<'a, W, IdT, F>
{
    type Ok = ();
    type Error = Error;
    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.push_positional_param(value)
    }
    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.end_positional_params()
    }
}

impl<'a, W: io::Write, IdT, F: Formatter> serde::ser::SerializeTupleVariant
    for SerializePositional<'a, W, IdT, F>
{
    type Ok = ();
    type Error = Error;
    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.push_positional_param(value)
    }
    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.end_positional_params()
    }
}

pub struct SerializeNamed<'a, W, IdT, F> {
    inner: &'a mut Serializer<W, IdT, F>,
    first: bool,
}

impl<'a, W: io::Write, IdT, F: Formatter> SerializeNamed<'a, W, IdT, F> {
    fn push_named_param(&mut self, key: &'static str, value: impl Serialize) -> Result<(), Error> {
        let Self {
            inner: Serializer { wrt, fmt, .. },
            first,
        } = self;

        fmt.begin_object_key(wrt, *first)?;
        *first = false;
        fmt.begin_string(wrt)?;
        for s in json_escape::escape_str(key) {
            fmt.write_raw_fragment(wrt, s)?
        }
        fmt.end_string(wrt)?;
        fmt.end_object_key(wrt)?;

        fmt.begin_object_value(wrt)?;
        value.serialize(&mut serde_json::Serializer::with_formatter(
            &mut *wrt,
            RefFormatter(fmt),
        ))?;
        fmt.end_object_value(wrt)?;

        Ok(())
    }
    fn end_named_params(self) -> Result<(), Error> {
        self.inner.fmt.end_object(&mut self.inner.wrt)?;
        self.inner.end_params()?;
        self.inner.end_outer()?;
        Ok(())
    }
}

impl<'a, W: io::Write, IdT, F: Formatter> serde::ser::SerializeStruct
    for SerializeNamed<'a, W, IdT, F>
{
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.push_named_param(key, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.end_named_params()
    }
}

impl<'a, W: io::Write, IdT, F: Formatter> serde::ser::SerializeStructVariant
    for SerializeNamed<'a, W, IdT, F>
{
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.push_named_param(key, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.end_named_params()
    }
}
