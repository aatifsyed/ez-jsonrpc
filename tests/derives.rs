use std::{
    collections::BTreeMap,
    fmt::Debug,
    panic::{self, UnwindSafe},
};

use ez_jsonrpc::params::{
    self, DeserializeNamed as DeNamed, DeserializePositional as DePos, SerializeNamed as SerNamed,
    SerializePositional as SerPos,
};
use serde::de::value::{MapDeserializer, SeqDeserializer};
use serde_json::{json, Value};

#[derive(DeNamed, DePos, SerNamed, SerPos, PartialEq, Debug)]
struct Empty;

#[test]
fn empty() {
    de_named(json!({}), Empty);
    de_named(json!({"excess": "argument"}), Empty);
    ser_named(Empty, json!({}));
    de_pos(json!([]), Empty);
    de_pos(json!(["excess argument"]), Empty);
    ser_pos(Empty, json!([]));
}

#[derive(DeNamed, DePos, SerNamed, SerPos, PartialEq, Debug)]
#[jsonrpc(deny_unknown_fields)]
struct EmptyDenyUnknown;

#[test]
fn empty_deny_unknown() {
    de_named(json!({}), EmptyDenyUnknown);
    should_panic(|| de_named(json!({"excess": "argument"}), EmptyDenyUnknown));
    ser_named(EmptyDenyUnknown, json!({}));
    de_pos(json!([]), EmptyDenyUnknown);
    should_panic(|| de_pos(json!(["excess argument"]), EmptyDenyUnknown));
    ser_pos(EmptyDenyUnknown, json!([]));
}

#[derive(DeNamed, DePos, SerNamed, SerPos, PartialEq, Debug)]
struct One {
    one: String,
}

#[test]
fn one() {
    fn one(s: &str) -> One {
        One { one: s.into() }
    }

    should_panic(|| de_named(json!({}), one("")));
    should_panic(|| de_pos(json!([]), one("")));
    de_named(json!({"one": "hello"}), one("hello"));
    de_named(json!({"one": "hello", "excess": "argument"}), one("hello"));
    de_named(json!({"excess": "argument", "one": "hello"}), one("hello"));
    ser_named(one("hello"), json!({"one": "hello"}));
    de_pos(json!(["hello"]), one("hello"));
    de_pos(json!(["hello", "excess argument"]), one("hello"));
    ser_pos(one("hello"), json!(["hello"]));
}

#[derive(DeNamed, DePos, SerNamed, SerPos, PartialEq, Debug)]
struct OneRename {
    #[jsonrpc(rename = "ONE")]
    one: String,
}

#[test]
fn one_rename() {
    fn one(s: &str) -> OneRename {
        OneRename { one: s.into() }
    }

    de_named(json!({"ONE": "hello"}), one("hello"));
    de_named(json!({"ONE": "hello", "excess": "argument"}), one("hello"));
    ser_named(one("hello"), json!({"ONE": "hello"}));
    de_pos(json!(["hello"]), one("hello"));
    de_pos(json!(["hello", "excess argument"]), one("hello"));
    ser_pos(one("hello"), json!(["hello"]));
}

#[derive(DeNamed, DePos, SerNamed, SerPos, PartialEq, Debug)]
struct OneDefault {
    #[jsonrpc(default)]
    one: String,
}

#[test]
fn one_default() {
    fn one(s: &str) -> OneDefault {
        OneDefault { one: s.into() }
    }

    de_named(json!({"one": "hello"}), one("hello"));
    de_named(json!({}), one(""));
    de_named(json!({"one": "hello", "excess": "argument"}), one("hello"));
    de_named(json!({"excess": "argument"}), one(""));
    ser_named(one("hello"), json!({"one": "hello"}));
    de_pos(json!(["hello"]), one("hello"));
    de_pos(json!([]), one(""));
}

#[derive(DeNamed, DePos, SerNamed, SerPos, PartialEq, Debug)]
struct OneDefaultRename {
    #[jsonrpc(default, rename = "ONE")]
    one: String,
}

#[test]
fn one_default_rename() {
    fn one(s: &str) -> OneDefaultRename {
        OneDefaultRename { one: s.into() }
    }

    de_named(json!({"ONE": "hello"}), one("hello"));
    de_named(json!({}), one(""));
    de_named(json!({"ONE": "hello", "excess": "argument"}), one("hello"));
    de_named(json!({"excess": "argument"}), one(""));
    ser_named(one("hello"), json!({"ONE": "hello"}));
}

#[derive(DeNamed, DePos, SerNamed, SerPos, PartialEq, Debug)]
#[jsonrpc(deny_unknown_fields)]
struct OneDenyUnknown {
    one: String,
}

#[test]
fn one_deny_unknown() {
    fn one(s: &str) -> OneDenyUnknown {
        OneDenyUnknown { one: s.into() }
    }

    de_named(json!({"one": "hello"}), one("hello"));
    should_panic(|| de_named(json!({"one": "hello", "excess": "argument"}), one("hello")));
    ser_named(one("hello"), json!({"one": "hello"}));
    de_pos(json!(["hello"]), one("hello"));
    should_panic(|| de_pos(json!(["hello", "excess argument"]), one("")));
    ser_pos(one("hello"), json!(["hello"]));
}

#[derive(DeNamed, DePos, SerNamed, SerPos, PartialEq, Debug)]
#[jsonrpc(deny_unknown_fields)]
struct OneDefaultDenyUnknown {
    #[jsonrpc(default)]
    one: String,
}

#[test]
fn one_default_deny_unknown() {
    fn one(s: &str) -> OneDefaultDenyUnknown {
        OneDefaultDenyUnknown { one: s.into() }
    }

    de_named(json!({}), one(""));
    should_panic(|| de_named(json!({"excess": "argument"}), one("")));
    de_pos(json!([]), one(""));
    should_panic(|| de_pos(json!(["hello", "excess argument"]), one("")));
    ser_pos(one("hello"), json!(["hello"]));
}

#[derive(DeNamed, DePos, SerNamed, SerPos, PartialEq, Debug)]
struct Two {
    one: String,
    two: String,
}

#[test]
fn two() {
    fn two(a: &str, b: &str) -> Two {
        Two {
            one: a.into(),
            two: b.into(),
        }
    }

    de_named(
        json!({"one": "hello", "two": "world"}),
        two("hello", "world"),
    );
    ser_named(
        two("hello", "world"),
        json!({"one": "hello", "two": "world"}),
    );
    de_pos(json!(["hello", "world"]), two("hello", "world"));
    ser_pos(two("hello", "world"), json!(["hello", "world"]));
}

fn unwrap_object(value: Value) -> serde_json::Map<String, Value> {
    match value {
        Value::Object(it) => it,
        _ => panic!("expected object, got {value}"),
    }
}
fn unwrap_array(value: Value) -> Vec<Value> {
    match value {
        Value::Array(it) => it,
        _ => panic!("expected array, got {value}"),
    }
}

#[track_caller]
fn de_named<T: for<'de> DeNamed<'de> + PartialEq + Debug>(src: Value, expected: T) {
    let src = unwrap_object(src).into_iter();
    let deserialized = T::de_named(MapDeserializer::<_, serde_json::Error>::new(src))
        .expect("failed to deserialize");
    assert_eq!(deserialized, expected);
}

#[track_caller]
fn ser_named<T: SerNamed + PartialEq + Debug>(src: T, expected: Value) {
    let expected = BTreeMap::from_iter(unwrap_object(expected));
    let serialized = T::ser_named(&src, params::ser::ByName::new()).expect("failed to deserialize");
    assert_eq!(serialized, expected);
}

#[track_caller]
fn de_pos<T: for<'de> DePos<'de> + PartialEq + Debug>(src: Value, expected: T) {
    let src = unwrap_array(src).into_iter();
    let deserialized = T::de_positional(SeqDeserializer::<_, serde_json::Error>::new(src))
        .expect("failed to deserialize");
    assert_eq!(deserialized, expected)
}

#[track_caller]
fn ser_pos<T: SerPos + PartialEq + Debug>(src: T, expected: Value) {
    let expected = unwrap_array(expected);
    let serialized =
        T::ser_positional(&src, params::ser::ByPosition::new()).expect("failed to serialize");
    assert_eq!(serialized, expected);
}

#[track_caller]
fn should_panic(f: impl FnOnce() + UnwindSafe) {
    if panic::catch_unwind(f).is_ok() {
        panic!("expected to panic, but didnt")
    }
}
