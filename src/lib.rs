pub mod params;

pub mod types {
    #[doc(inline)]
    pub use ez_jsonrpc_types::*;
}

#[doc(hidden)]
pub mod __private {
    macro_rules! exports {
        ($($path:path as $ident:ident);* $(;)?) => {
            pub mod exports {
                $(pub use $path as $ident;)*
            }
        };
    }

    exports! { // sync with the macros crate
        crate::params::DeserializeNamed as DeserializeNamed;
        crate::params::DeserializePositional as DeserializePositional;
        crate::params::SerializeNamed as SerializeNamed;
        crate::params::SerializePositional as SerializePositional;
        Err as Err_;
        None as None_;
        Ok as Ok_;
        Result as Result;
        serde::de::Error as de_Error;
        serde::de::IgnoredAny as IgnoredAny;
        serde::de::MapAccess as MapAccess;
        serde::de::SeqAccess as SeqAccess;
        serde::de::value::MapAccessDeserializer as MapAccessDeserializer;
        serde::ser::SerializeMap as SerializeMap;
        serde::ser::SerializeSeq as SerializeSeq;
        serde::Deserialize as Deserialize;
    }
}
