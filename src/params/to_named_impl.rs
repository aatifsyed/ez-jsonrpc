use std::collections::{BTreeMap, HashMap};

use super::{AssertNamed, SerializeNamed};
use serde::Serialize;

impl<T, E> SerializeNamed for Result<T, E>
where
    T: Serialize,
    E: Serialize,
{
    fn ser_named<S: serde::ser::SerializeMap>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.serialize(AssertNamed(serializer))
    }
}
impl<K, V> SerializeNamed for BTreeMap<K, V>
where
    K: Serialize,
    V: Serialize,
{
    fn ser_named<S: serde::ser::SerializeMap>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.serialize(AssertNamed(serializer))
    }
}
impl<K, V, H> SerializeNamed for HashMap<K, V, H>
where
    K: Serialize,
    V: Serialize,
{
    fn ser_named<S: serde::ser::SerializeMap>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.serialize(AssertNamed(serializer))
    }
}
