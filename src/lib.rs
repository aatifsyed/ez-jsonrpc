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
        stream::{Fuse, Map, Select},
    };
    use futures_util::{stream, FutureExt, SinkExt as _, StreamExt, TryStreamExt};
    use futures_util::{Sink, Stream};
    use pin_project_lite::pin_project;
    use serde::{Deserialize, Serialize};
    use serde_json::Value;

    use crate::types::template;

    #[derive(Debug)]
    enum DispatchState {
        Alive,
        FlushSink,
        StreamDead,
        SinkDead,
        Dead,
    }

    pub enum Interest<RespT, SendE, RecvE> {
        Notification(oneshot::Sender<SendE>),
        Dialogue(oneshot::Sender<Result<RespT, Direction<SendE, RecvE>>>),
    }

    enum Outstanding<RespT, SendE, RecvE, IdT> {
        Notification(oneshot::Sender<SendE>),
        Dialogue(IdT, oneshot::Sender<Result<RespT, Direction<SendE, RecvE>>>),
    }

    pub enum Direction<SendE, RecvE> {
        Sending(SendE),
        Receiving(RecvE),
    }

    pin_project! {
    pub struct Dispatcher<StreamT, SinkT, RespT, IdT, SendE, RecvE, IdI> {
        #[pin] stream: StreamT,
        #[pin] sink: SinkT,
        state: DispatchState,
        // Always [`Some`] if [`DispatchState::FlushSink`].
        outstanding: Option<Outstanding<RespT, SendE, RecvE, IdT>>,
        ids: IdI,
    }}

    impl<StreamT, SinkT, RespT, IdT, SendE, RecvE, IdI>
        Dispatcher<StreamT, SinkT, RespT, IdT, SendE, RecvE, IdI>
    {
        pub fn new(stream: StreamT, sink: SinkT, ids: IdI) -> Self {
            Self {
                stream,
                sink,
                state: DispatchState::Alive,
                outstanding: None,
                ids,
            }
        }
    }

    impl<StreamT, SinkT, RespT, IdT, SendE, RecvE, IdI, MethodT, ParamsT> Stream
        for Dispatcher<StreamT, SinkT, RespT, IdT, SendE, RecvE, IdI>
    where
        StreamT: Stream<Item = (MethodT, Option<ParamsT>, Interest<RespT, SendE, RecvE>)>,
        SinkT: Sink<template::Request<MethodT, IdT, ParamsT>, Error = SendE>,
        IdI: Iterator<Item = IdT>,
        IdT: Clone,
    {
        type Item = Result<(IdT, oneshot::Sender<Result<RespT, Direction<SendE, RecvE>>>), SendE>;

        fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
            let mut this = self.project();
            loop {
                match this.state {
                    // Note that even if the sink is dead,
                    // we _still_ want to propogate its errors to the sender.
                    DispatchState::Alive | DispatchState::SinkDead => {
                        match (
                            // the ordering is important here -
                            // we only want to pull an item from the stream if
                            // we _know_ the sink is ready.
                            ready!(this.sink.as_mut().poll_ready(cx)),
                            ready!(this.stream.as_mut().poll_next(cx)),
                        ) {
                            (Ok(()), None) => *this.state = DispatchState::StreamDead,
                            (Ok(()), Some((method, params, interest))) => match interest {
                                Interest::Notification(sender) => {
                                    let request = template::Request {
                                        method,
                                        params,
                                        id: None,
                                    };
                                    match this.sink.as_mut().start_send(request) {
                                        Ok(()) => {
                                            *this.state = DispatchState::FlushSink;
                                            assert!(this.outstanding.is_none());
                                            *this.outstanding =
                                                Some(Outstanding::Notification(sender));
                                        }
                                        Err(e) => {
                                            *this.state = DispatchState::SinkDead;
                                            match sender.send(e) {
                                                Ok(()) => {}
                                                Err(e) => return Poll::Ready(Some(Err(e))),
                                            }
                                        }
                                    }
                                }
                                Interest::Dialogue(sender) => {
                                    let id =
                                        this.ids.next().expect("ID iterator should be infinite");
                                    let request = template::Request {
                                        method,
                                        params,
                                        id: Some(id.clone()),
                                    };
                                    match this.sink.as_mut().start_send(request) {
                                        Ok(()) => {
                                            *this.state = DispatchState::FlushSink;
                                            assert!(this.outstanding.is_none());
                                            *this.outstanding =
                                                Some(Outstanding::Dialogue(id, sender));
                                        }
                                        Err(e) => {
                                            *this.state = DispatchState::SinkDead;
                                            if let Some(e) = send_error(sender, e) {
                                                return Poll::Ready(Some(Err(e)));
                                            }
                                        }
                                    }
                                }
                            },
                            (Err(e), None) => {
                                *this.state = DispatchState::Dead;
                                return Poll::Ready(Some(Err(e)));
                            }
                            (Err(e), Some((_, _, interest))) => {
                                *this.state = DispatchState::SinkDead;
                                match interest {
                                    Interest::Notification(sender) => match sender.send(e) {
                                        Ok(()) => {}
                                        Err(e) => return Poll::Ready(Some(Err(e))),
                                    },
                                    Interest::Dialogue(sender) => {
                                        if let Some(e) = send_error(sender, e) {
                                            return Poll::Ready(Some(Err(e)));
                                        }
                                    }
                                };
                            }
                        }
                    }
                    // We're flushing a message and have the sender to notify
                    // of errors.
                    DispatchState::FlushSink => match ready!(this.sink.as_mut().poll_flush(cx)) {
                        Ok(()) => {
                            *this.state = DispatchState::Alive;
                            match this.outstanding.take().unwrap() {
                                Outstanding::Notification(sender) => drop(sender),
                                Outstanding::Dialogue(id, sender) => {
                                    return Poll::Ready(Some(Ok((id, sender))))
                                }
                            }
                        }
                        Err(e) => {
                            *this.state = DispatchState::SinkDead;
                            match this.outstanding.take().unwrap() {
                                Outstanding::Notification(sender) => match sender.send(e) {
                                    Ok(()) => {}
                                    Err(e) => return Poll::Ready(Some(Err(e))),
                                },
                                Outstanding::Dialogue(_, sender) => {
                                    if let Some(e) = send_error(sender, e) {
                                        return Poll::Ready(Some(Err(e)));
                                    }
                                }
                            }
                        }
                    },
                    // We've handled all requests - close the sink.
                    DispatchState::StreamDead => {
                        let closed = ready!(this.sink.as_mut().poll_close(cx));
                        // cannot call any sink methods from this point.
                        *this.state = DispatchState::Dead;
                        match closed {
                            Ok(()) => {}
                            Err(e) => return Poll::Ready(Some(Err(e))), // parting error
                        }
                    }
                    DispatchState::Dead => return Poll::Ready(None),
                }
            }
        }
    }

    fn send_error<RespT, SendE, RecvE>(
        sender: oneshot::Sender<Result<RespT, Direction<SendE, RecvE>>>,
        e: SendE,
    ) -> Option<SendE> {
        match sender.send(Err(Direction::Sending(e))) {
            Ok(()) => None,
            Err(Err(Direction::Sending(e))) => Some(e),
            Err(_) => unreachable!(),
        }
    }

    pin_project! {
    pub struct Reactor<DispatchS, IncomingS, DItem, IItem, SendE, RecvE, IdT, RespT, BuildHasherT> {
        #[pin] react: Select<
            Map<DispatchS, fn(DItem) -> React<DItem, IItem>>,
            Map<IncomingS, fn(IItem) -> React<DItem, IItem>>,
        >,
        map: HashMap<IdT, oneshot::Sender<Result<RespT, Direction<SendE, RecvE>>>, BuildHasherT>
    }}

    impl<DispatchS, IncomingS, SendE, RecvE, IdT, ValueT, ValueE, StringE, BuildHasherT> Stream
        for Reactor<
            DispatchS,
            IncomingS,
            (
                IdT,
                oneshot::Sender<
                    Result<template::Result<ValueT, ValueE, StringE>, Direction<SendE, RecvE>>,
                >,
            ),
            template::Response<ValueT, ValueE, StringE, IdT>,
            SendE,
            RecvE,
            IdT,
            template::Result<ValueT, ValueE, StringE>,
            BuildHasherT,
        >
    where
        DispatchS: Stream<
            Item = (
                IdT,
                oneshot::Sender<
                    Result<template::Result<ValueT, ValueE, StringE>, Direction<SendE, RecvE>>,
                >,
            ),
        >,
        IncomingS: Stream<Item = template::Response<ValueT, ValueE, StringE, IdT>>,
        BuildHasherT: BuildHasher,
        IdT: Hash + Eq,
    {
        type Item = ();

        fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
            let mut this = self.project();
            loop {
                match ready!(this.react.as_mut().poll_next(cx)) {
                    Some(React::Dispatched((id, sender))) => {
                        this.map.insert(id, sender);
                    }
                    Some(React::Incoming(template::Response { result, id })) => {
                        match this.map.remove(&id) {
                            Some(sender) => match sender.send(Ok(result)) {
                                Ok(()) => todo!(),
                                Err(result) => todo!(),
                            },
                            None => todo!(),
                        }
                    }
                    None => return Poll::Ready(None),
                }
            }
        }
    }

    enum React<D, I> {
        Dispatched(D),
        Incoming(I),
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
    struct _Reactor<StreamT, P> {
        #[pin]
        stream: StreamT,
        map: P,
    }}

    struct Pruner<P> {
        map: P,
    }

    impl<StreamT, P> _Reactor<StreamT, P> {
        pub fn new(stream: StreamT, map: P) -> Self {
            Self { stream, map }
        }
    }

    impl<StreamT, P, ValueT, ValueE, StringE, IdT, StreamE> Stream for _Reactor<StreamT, P>
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
