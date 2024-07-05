<!-- cargo-rdme start -->

A transcription of types from the [`JSON-RPC 2.0` Specification](https://www.jsonrpc.org/specification).

> When quoted, the specification will appear as blockquoted text, like so.

# Design
- All structs are owned (i.e, there is no borrowing of data from the [`Deserializer`](serde::Deserializer)),
  to facilitate ergonomics.
- Appearances of dynamic JSON [`Value`]s are parameterised out, to allow
  deferred serialization using, i.e [RawValue](https://docs.rs/serde_json/latest/serde_json/value/struct.RawValue.html).

<!-- cargo-rdme end -->
