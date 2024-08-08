use ez_jsonrpc::types::params::{
    DeserializeNamed, DeserializePositional, SerializeNamed, SerializePositional,
};
use serde::{Deserialize, Serialize};

struct ExampleArgs {
    first: String,
    second: usize,
    third: YakShaver,
    maybe: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct YakShaver {
    name: String,
    count: usize,
}

impl<'de> DeserializeNamed<'de> for ExampleArgs {
    fn de_named<D: serde::de::MapAccess<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        #[serde(remote = "ExampleArgs")]
        struct This {
            first: String,
            second: usize,
            third: YakShaver,
            #[serde(skip_serializing_if = "Option::is_none")]
            maybe: Option<String>,
        }
        This::deserialize(serde::de::value::MapAccessDeserializer::new(deserializer))
    }
}

impl<'de> DeserializePositional<'de> for ExampleArgs {
    fn de_positional<D: serde::de::SeqAccess<'de>>(mut deserializer: D) -> Result<Self, D::Error> {
        let too_few = "a sequence of at least 3 items";
        let too_many = "a sequence of at most 4 items";
        let first = deserializer
            .next_element()?
            .ok_or_else(|| serde::de::Error::invalid_length(0, &too_few))?;
        let second = deserializer
            .next_element()?
            .ok_or_else(|| serde::de::Error::invalid_length(1, &too_few))?;
        let third = deserializer
            .next_element()?
            .ok_or_else(|| serde::de::Error::invalid_length(2, &too_few))?;
        let maybe = deserializer.next_element()?;
        if deserializer
            .next_element::<serde::de::IgnoredAny>()?
            .is_some()
        {
            return Err(serde::de::Error::invalid_length(4, &too_many));
        }
        Ok(Self {
            first,
            second,
            third,
            maybe,
        })
    }
}

impl SerializeNamed for ExampleArgs {
    fn ser_named<S: serde::ser::SerializeMap>(&self, mut serializer: S) -> Result<S::Ok, S::Error> {
        let Self {
            first,
            second,
            third,
            maybe,
        } = self;
        serializer.serialize_entry("first", first)?;
        serializer.serialize_entry("second", second)?;
        serializer.serialize_entry("third", third)?;
        if let Some(maybe) = maybe {
            serializer.serialize_entry("maybe", maybe)?
        };
        serializer.end()
    }
}

impl SerializePositional for ExampleArgs {
    fn ser_positional<S: serde::ser::SerializeSeq>(
        &self,
        mut serializer: S,
    ) -> Result<S::Ok, S::Error> {
        let Self {
            first,
            second,
            third,
            maybe,
        } = self;
        serializer.serialize_element(first)?;
        serializer.serialize_element(second)?;
        serializer.serialize_element(third)?;
        if let Some(maybe) = maybe {
            serializer.serialize_element(maybe)?
        }
        serializer.end()
    }
}
