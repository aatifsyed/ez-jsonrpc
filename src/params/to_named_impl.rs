use std::collections::{BTreeMap, HashMap};

use super::{AssertNamed, ToNamed};
use serde::Serialize;

impl<T, E> ToNamed for Result<T, E>
where
    T: Serialize,
    E: Serialize,
{
    fn to_named<S: serde::ser::SerializeMap>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.serialize(AssertNamed(serializer))
    }
}
impl<K, V> ToNamed for BTreeMap<K, V>
where
    K: Serialize,
    V: Serialize,
{
    fn to_named<S: serde::ser::SerializeMap>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.serialize(AssertNamed(serializer))
    }
}
impl<K, V, H> ToNamed for HashMap<K, V, H>
where
    K: Serialize,
    V: Serialize,
{
    fn to_named<S: serde::ser::SerializeMap>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.serialize(AssertNamed(serializer))
    }
}
