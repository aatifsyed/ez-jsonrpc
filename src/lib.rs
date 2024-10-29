pub mod params;

pub mod types {
    #[doc(inline)]
    pub use ez_jsonrpc_types::*;
}

pub use types::{
    Error, Id, Map, MaybeBatchedRequest, MaybeBatchedResponse, Request, RequestParameters,
    Response, Result,
};

pub mod server;
pub mod client {
    use std::{
        collections::HashMap,
        convert::Infallible,
        future::Future,
        hash::{BuildHasher, Hash, RandomState},
        marker::PhantomData,
        num::Wrapping,
        ops::Deref,
        pin::{self, pin, Pin},
        sync::{
            atomic::{AtomicU32, AtomicU64},
            Arc, Mutex,
        },
        task::{ready, Context, Poll},
    };

    use either::Either;
    use ez_jsonrpc_types::Id;
    use futures_channel::oneshot;
    use futures_util::{
        lock::{Mutex as AsyncMutex, OwnedMutexLockFuture},
        stream::Fuse,
    };
    use futures_util::{stream, FutureExt, SinkExt as _, StreamExt, TryStreamExt};
    use futures_util::{Sink, Stream};
    use pin_project_lite::pin_project;
    use serde::{Deserialize, Serialize};
    use serde_json::Value;

    use crate::types::template;

    pin_project! {
    /// Stream adapter which sends each item, one at a time,
    /// from the souce stream into the given sink.
    ///
    /// The source stream is a tuple of `(Item, Sender)`.
    /// If the sink returns an error, the `Sender` for the relevant message is
    /// notified (or if that fails, the error is yielded).
    ///
    /// If the item was successfully flushed into the sink, it is yielded,
    /// along with some extracted metadata.
    #[derive(Debug)]
    pub struct SendEach<StreamT, SinkT, AnyOk, SinkE, ForwardT, ForwardF> {
        // Always [`Some`] if [`SendEachState::FlushSink`].
        outstanding: Option<(ForwardT, oneshot::Sender<Result<AnyOk, SinkE>>)>,
        state: SendEachState,
        forward: ForwardF,
        #[pin] stream: Fuse<StreamT>,
        #[pin] sink: SinkT,
    }}

    #[derive(Debug)]
    enum SendEachState {
        Alive,
        FlushSink,
        StreamDead,
        SinkDead,
        Dead,
    }

    impl<StreamT, SinkT, T, AnyOk, SinkE, ForwardT, ForwardF> Stream
        for SendEach<StreamT, SinkT, AnyOk, SinkE, ForwardT, ForwardF>
    where
        StreamT: Stream<Item = (T, oneshot::Sender<Result<AnyOk, SinkT::Error>>)>,
        SinkT: Sink<T, Error = SinkE>,
        ForwardF: FnMut(&T) -> ForwardT,
    {
        type Item = Result<(ForwardT, oneshot::Sender<Result<AnyOk, SinkT::Error>>), SinkT::Error>;

        fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
            let mut this = self.project();
            loop {
                match this.state {
                    // Note that even if the sink is dead,
                    // we _still_ want to propogate its errors to the sender.
                    SendEachState::Alive | SendEachState::SinkDead => {
                        match (
                            // the ordering is important here -
                            // we only want to pull an item from the stream if
                            // we _know_ the sink is ready.
                            ready!(this.sink.as_mut().poll_ready(cx)),
                            ready!(this.stream.as_mut().poll_next(cx)),
                        ) {
                            (Ok(()), None) => *this.state = SendEachState::StreamDead,
                            (Ok(()), Some((sendme, pageme))) => {
                                let forward = (this.forward)(&sendme);
                                match this.sink.as_mut().start_send(sendme) {
                                    Ok(()) => {
                                        *this.state = SendEachState::FlushSink;
                                        assert!(this.outstanding.is_none());
                                        *this.outstanding = Some((forward, pageme));
                                    }
                                    Err(e) => {
                                        *this.state = SendEachState::SinkDead;
                                        match pageme.send(Err(e)) {
                                            Ok(()) => {}
                                            Err(Err(e)) => return Poll::Ready(Some(Err(e))),
                                            Err(Ok(_)) => unreachable!(),
                                        }
                                    }
                                };
                            }
                            (Err(e), None) => {
                                *this.state = SendEachState::Dead;
                                return Poll::Ready(Some(Err(e)));
                            }
                            (Err(e), Some((_sendme, pageme))) => {
                                *this.state = SendEachState::SinkDead;
                                match pageme.send(Err(e)) {
                                    Ok(()) => {}
                                    Err(Err(e)) => return Poll::Ready(Some(Err(e))),
                                    Err(Ok(_)) => unreachable!(),
                                }
                            }
                        }
                    }
                    // We're flushing a message and have the sender to notify
                    // of errors.
                    SendEachState::FlushSink => match ready!(this.sink.as_mut().poll_flush(cx)) {
                        Ok(()) => {
                            *this.state = SendEachState::Alive;
                            return Poll::Ready(Some(Ok(this.outstanding.take().unwrap())));
                        }
                        Err(e) => {
                            *this.state = SendEachState::SinkDead;
                            let (_forward, pageme) = this.outstanding.take().unwrap();
                            match pageme.send(Err(e)) {
                                Ok(()) => {}
                                Err(Err(e)) => return Poll::Ready(Some(Err(e))),
                                Err(Ok(_)) => unreachable!(),
                            }
                        }
                    },
                    // We've handled all requests - close the sink.
                    SendEachState::StreamDead => {
                        let closed = ready!(this.sink.as_mut().poll_close(cx));
                        // cannot call any sink methods from this point.
                        *this.state = SendEachState::Dead;
                        match closed {
                            Ok(()) => {}
                            Err(e) => return Poll::Ready(Some(Err(e))), // parting error
                        }
                    }
                    SendEachState::Dead => return Poll::Ready(None),
                }
            }
        }
    }

    pub struct Service<SinkT, StreamT> {
        sink: Arc<AsyncMutex<SinkT>>,
        stream: Arc<AsyncMutex<StreamT>>,
        id: AtomicU32,
    }

    impl<MethodT, RequestParametersT, SinkT, StreamT, ValueT, ValueE, StringE, StreamE>
        tower_service::Service<(MethodT, RequestParametersT)> for &Service<SinkT, StreamT>
    where
        SinkT: Sink<template::Request<MethodT, u32, RequestParametersT>>,
        StreamT: Stream<Item = Result<template::Response<ValueT, ValueE, StringE, u32>, StreamE>>,
    {
        type Response = template::Result;
        type Error = Either<SinkT::Error, StreamE>;
        type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>>>>;

        fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
            Poll::Ready(Ok(()))
        }

        fn call(&mut self, req: (MethodT, RequestParametersT)) -> Self::Future {
            todo!()
        }
    }

    pub enum TransportError {}

    pub struct Client {}

    async fn executor<ClientT, SinkT, MethodT, RequestParametersT, IdI, IdT, ResponseT>(
        client: ClientT,
        mut ids: IdI,
        sink: SinkT,
    ) where
        ClientT: Stream<
            Item = (
                MethodT,
                Option<RequestParametersT>,
                Option<oneshot::Sender<Result<ResponseT, SinkT::Error>>>,
            ),
        >,
        SinkT: Sink<template::Request<MethodT, IdT, RequestParametersT>>,
        IdI: Iterator<Item = IdT>,
    {
        let mut client = pin!(client);
        let mut sink = pin!(sink);
        while let Some((method, params, sender)) = client.next().await {
            match sender {
                Some(sender) => match ids.next() {
                    Some(id) => todo!(),
                    None => continue, // we've run out of IDs! skip
                },
                None => {
                    let _ = sink
                        .feed(template::Request {
                            method,
                            params,
                            id: None,
                        })
                        .await;
                }
            }
        }
        todo!()
    }

    pin_project! {
    struct Reactor<StreamT, P> {
        #[pin]
        stream: StreamT,
        map: P,
    }}

    struct Pruner<P> {
        map: P,
    }

    impl<StreamT, P> Reactor<StreamT, P> {
        pub fn new(stream: StreamT, map: P) -> Self {
            Self { stream, map }
        }
    }

    impl<StreamT, P, ValueT, ValueE, StringE, IdT, StreamE> Stream for Reactor<StreamT, P>
    where
        StreamT: Stream<Item = Result<template::Response<ValueT, ValueE, StringE, IdT>, StreamE>>,
        P: Deref<
            Target = Mutex<
                HashMap<IdT, oneshot::Sender<template::Result<ValueT, ValueE, StringE>>>,
            >,
        >,
        IdT: Hash + Eq,
    {
        type Item = ReactorError<StreamE, ValueT, ValueE, StringE, IdT>;

        fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
            let mut this = self.project();
            loop {
                let next = ready!(this.stream.as_mut().poll_next(cx));
                match next {
                    Some(Ok(template::Response { result, id })) => {
                        match this.map.lock().expect("poisoned lock").remove(&id) {
                            Some(sender) => match sender.send(result) {
                                Ok(()) => continue,
                                Err(result) => {
                                    return Poll::Ready(Some(ReactorError::Hangup(
                                        template::Response { result, id },
                                    )))
                                }
                            },
                            None => {
                                return Poll::Ready(Some(ReactorError::NoSuchId(
                                    template::Response { result, id },
                                )))
                            }
                        }
                    }
                    Some(Err(e)) => return Poll::Ready(Some(ReactorError::Incoming(e))),
                    None => return Poll::Ready(None),
                }
            }
        }
    }

    pin_project! {
    /// Stream adapter which assigns IDs to JSON-RPC requests.
    pub struct AssignId<StreamT, IdI, P> {
        #[pin]
        stream: StreamT,
        map: P,
        running_id: IdI,
    }}

    impl<StreamT, IdI, P> AssignId<StreamT, IdI, P> {
        /// `running_id` MUST be an infinite iterator.
        pub fn new(stream: StreamT, running_id: IdI, map: P) -> Self {
            Self {
                stream,
                map,
                running_id,
            }
        }
    }

    impl<StreamT, IdI, P, MethodT, RequestParametersT, IdT, V, S> Stream for AssignId<StreamT, IdI, P>
    where
        StreamT: Stream<Item = (Option<V>, MethodT, Option<RequestParametersT>)>,
        P: Deref<Target = Mutex<HashMap<IdT, V, S>>>,
        IdI: Iterator<Item = IdT>,
        IdT: Clone + Hash + Eq,
        S: BuildHasher,
    {
        type Item = template::Request<MethodT, IdT, RequestParametersT>;

        fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
            let this = self.project();
            let inner = ready!(this.stream.poll_next(cx));
            Poll::Ready(inner.map(|(sender, method, params)| {
                template::Request {
                    method,
                    params,
                    id: match sender {
                        Some(v) => {
                            let id = this.running_id.next().expect("out of IDs");
                            // if there was already an outstanding ID,
                            // there could be miscorrelation
                            this.map
                                .lock()
                                .expect("poisoned lock")
                                .insert(id.clone(), v);
                            Some(id)
                        }
                        None => None,
                    },
                }
            }))
        }
    }

    pub enum ReactorError<IncomingE, ValueT = Value, ValueE = Value, StringE = String, IdT = Id> {
        NoSuchId(template::Response<ValueT, ValueE, StringE, IdT>),
        Hangup(template::Response<ValueT, ValueE, StringE, IdT>),
        Incoming(IncomingE),
    }
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
        serde as serde;
        serde::de::Error as de_Error;
        serde::de::IgnoredAny as IgnoredAny;
        serde::de::MapAccess as MapAccess;
        serde::de::SeqAccess as SeqAccess;
        serde::de::value::MapAccessDeserializer as MapAccessDeserializer;
        serde::Deserialize as Deserialize;
        serde::ser::SerializeMap as SerializeMap;
        serde::ser::SerializeSeq as SerializeSeq;
    }
}
