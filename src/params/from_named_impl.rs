use std::{
    collections::{BTreeMap, HashMap},
    hash::{BuildHasher, Hash},
};

use serde::{de::IntoDeserializer, Deserialize};

use super::FromNamed;

impl<'de, T, E> FromNamed<'de> for Result<T, E>
where
    T: Deserialize<'de>,
    E: Deserialize<'de>,
{
    fn from_named<K, V, I: Iterator<Item = (K, V)>, ME>(
        deserializer: serde::de::value::MapDeserializer<'de, I, ME>,
    ) -> Result<Self, ME>
    where
        Self: Sized,
        K: IntoDeserializer<'de, ME>,
        V: IntoDeserializer<'de, ME>,
        ME: serde::de::Error,
    {
        Deserialize::deserialize(deserializer)
    }
}

impl<'de, K, V, S> FromNamed<'de> for HashMap<K, V, S>
where
    K: Deserialize<'de> + Hash + Eq,
    V: Deserialize<'de>,
    S: BuildHasher + Default,
{
    fn from_named<IK, IV, I: Iterator<Item = (IK, IV)>, E>(
        deserializer: serde::de::value::MapDeserializer<'de, I, E>,
    ) -> Result<Self, E>
    where
        Self: Sized,
        IK: IntoDeserializer<'de, E>,
        IV: IntoDeserializer<'de, E>,
        E: serde::de::Error,
    {
        Deserialize::deserialize(deserializer)
    }
}
impl<'de, K, V> FromNamed<'de> for BTreeMap<K, V>
where
    K: Deserialize<'de> + Ord,
    V: Deserialize<'de>,
{
    fn from_named<IK, IV, I: Iterator<Item = (IK, IV)>, E>(
        deserializer: serde::de::value::MapDeserializer<'de, I, E>,
    ) -> Result<Self, E>
    where
        Self: Sized,
        IK: IntoDeserializer<'de, E>,
        IV: IntoDeserializer<'de, E>,
        E: serde::de::Error,
    {
        Deserialize::deserialize(deserializer)
    }
}
