use std::fmt::Debug;

use serde::{Deserialize, Serialize};
pub use serde_json::{self, json};

#[macro_export]
macro_rules! assert_roundtrip {
    ($($rust:expr, $json:tt);* $(;)?) => {
        $(
            $crate::round_trip($rust, $crate::serde_json::json!($json));
        )*
    };
}

#[track_caller]
pub fn round_trip<T: Serialize + for<'de> Deserialize<'de> + PartialEq + Debug>(
    rust: T,
    json: serde_json::Value,
) {
    let rust2json = serde_json::to_value(&rust).expect("failed to serialize rust to JSON");
    pretty_assertions::assert_str_eq!(json.to_string(), rust2json.to_string());
    let json2rust = serde_json::from_value::<T>(json).expect("failed to serialize rust from JSON");
    pretty_assertions::assert_eq!(rust, json2rust, "rust vs json2rust")
}
