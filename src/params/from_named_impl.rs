use std::{
    collections::{BTreeMap, HashMap},
    hash::{BuildHasher, Hash},
};

use serde::{de::value::MapAccessDeserializer, Deserialize};

use super::DeserializeNamed;

impl<'de, T, E> DeserializeNamed<'de> for Result<T, E>
where
    T: Deserialize<'de>,
    E: Deserialize<'de>,
{
    fn de_named<D: serde::de::MapAccess<'de>>(deserializer: D) -> Result<Self, D::Error>
    where
        Self: Sized,
    {
        Deserialize::deserialize(MapAccessDeserializer::new(deserializer))
    }
}

impl<'de, K, V, S> DeserializeNamed<'de> for HashMap<K, V, S>
where
    K: Deserialize<'de> + Hash + Eq,
    V: Deserialize<'de>,
    S: BuildHasher + Default,
{
    fn de_named<D: serde::de::MapAccess<'de>>(deserializer: D) -> Result<Self, D::Error>
    where
        Self: Sized,
    {
        Deserialize::deserialize(MapAccessDeserializer::new(deserializer))
    }
}
impl<'de, K, V> DeserializeNamed<'de> for BTreeMap<K, V>
where
    K: Deserialize<'de> + Ord,
    V: Deserialize<'de>,
{
    fn de_named<D: serde::de::MapAccess<'de>>(deserializer: D) -> Result<Self, D::Error>
    where
        Self: Sized,
    {
        Deserialize::deserialize(MapAccessDeserializer::new(deserializer))
    }
}
