use std::{
    hash::RandomState,
    io::{self, Write},
    marker::PhantomData,
    sync::Arc,
};

use bytes::{Buf, BufMut, BytesMut};
use ez_jsonrpc::components::client::{Client, Input, MethodCall, Task};
use ez_jsonrpc_types::template;
use futures::{
    future::{self, join, FusedFuture},
    stream::{self, StreamExt as _},
};
use serde::{Deserialize, Serialize};
use tokio_util::codec::{Decoder, Encoder, Framed};
use tower::util::ServiceExt as _;

fn main() -> anyhow::Result<()> {
    tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?
        .block_on(async {
            let (sink, stream) = Framed::new(
                tokio::io::join(tokio::io::stdin(), tokio::io::stdout()),
                Codec(
                    StreamDecoder::<ez_jsonrpc::Response>::new(),
                    LinesEncoder::<ez_jsonrpc::Request>::new(),
                ),
            )
            .split();

            let (sendreq, rxreq) = futures_channel::mpsc::channel(100);
            let _ = join(
                async {
                    let errors = Task::<_, _, _, _, _, _>::new(
                        rxreq,
                        sink,
                        stream.map(|it| match it {
                            Ok(it) => Input::Response(it),
                            Err(e) => {
                                let e = Arc::new(e);
                                Input::ReceiveError(move || Arc::clone(&e))
                            }
                        }),
                        (0..=u64::MAX).cycle().map(|it| {
                            template::Id::<String, serde_json::Number>::Number(it.into())
                        }),
                        RandomState::new(),
                    );
                    let mut errors = as_fused_stream(errors);
                    while let Some(e) = errors.next().await {
                        dbg!(e);
                    }
                },
                async {
                    let client = Client::<_, _, _>::new(sendreq);

                    let req = client
                        .oneshot(MethodCall {
                            method: String::new(),
                            params: None,
                            timeout: future::pending(),
                        })
                        .await;
                    dbg!(req)
                },
            )
            .await;
            Ok(())
        })
}

fn as_stream<S: stream::Stream<Item = T>, T>(s: S) -> S {
    s
}
fn as_sink<S: futures_util::Sink<T, Error = E>, T, E>(s: S) -> S {
    s
}
fn as_fused_stream<S: stream::Stream<Item = T>, T>(s: S) -> S {
    s
}
fn as_fused_future<F: FusedFuture<Output = T>, T>(f: F) -> F {
    f
}

fn assert_stream<S: stream::Stream<Item = T>, T>() {}
fn assert_sink<S: futures_util::Sink<T, Error = E>, T, E>() {}
fn assert_fused_stream<S: stream::FusedStream<Item = T>, T>() {}

pub struct Codec<D, E>(pub D, pub E);
impl<D, E> Decoder for Codec<D, E>
where
    D: Decoder,
{
    type Item = D::Item;

    type Error = D::Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        self.0.decode(src)
    }

    fn decode_eof(&mut self, buf: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        self.0.decode_eof(buf)
    }
}
impl<T, D, E> Encoder<T> for Codec<D, E>
where
    E: Encoder<T>,
{
    type Error = E::Error;

    fn encode(&mut self, item: T, dst: &mut BytesMut) -> Result<(), Self::Error> {
        self.1.encode(item, dst)
    }
}

struct StreamDecoder<T>(PhantomData<fn() -> T>);
impl<T> StreamDecoder<T> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

impl<T> Decoder for StreamDecoder<T>
where
    T: for<'de> Deserialize<'de>,
{
    type Item = T;
    type Error = io::Error;
    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        let mut stream = serde_json::Deserializer::from_slice(src).into_iter::<T>();
        match stream.next() {
            Some(Ok(t)) => {
                src.advance(stream.byte_offset());
                Ok(Some(t))
            }
            Some(Err(e)) => match e.is_eof() {
                true => Ok(None),
                false => Err(e.into()),
            },
            None => Ok(None),
        }
    }
    fn decode_eof(&mut self, buf: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        serde_json::Deserializer::from_slice(buf).end()?;
        Ok(None)
    }
}

struct LinesEncoder<T>(PhantomData<T>);
impl<T> LinesEncoder<T> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

impl<T> Encoder<T> for LinesEncoder<T>
where
    T: Serialize,
{
    type Error = io::Error;

    fn encode(&mut self, item: T, dst: &mut BytesMut) -> Result<(), Self::Error> {
        serde_json::to_writer(dst.writer(), &item)?;
        dst.writer().write_all(b"\n")?;
        Ok(())
    }
}
