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
        fmt,
        hash::{BuildHasher, Hash},
        pin::Pin,
        task::{ready, Context, Poll},
    };

    use futures_channel::oneshot;
    use futures_util::{
        stream::FusedStream,
        stream::{Map, Select},
    };
    use futures_util::{Sink, Stream};
    use pin_project_lite::pin_project;

    use crate::types::template;

    #[derive(Clone, Default)]
    pub struct Ids(u64);

    impl Ids {
        pub fn new() -> Self {
            Self::default()
        }
    }
    impl Iterator for Ids {
        type Item = template::Id;
        fn next(&mut self) -> Option<Self::Item> {
            let id = template::Id::from(self.0);
            self.0 = self.0.wrapping_add(1);
            Some(id)
        }
    }

    /// Item to be send to a [`Dispatcher`].
    #[derive(Debug)]
    pub struct DispatchRequest<
        SendE = Infallible,
        RecvE = SendE,
        MethodT = String,
        ParamsT = template::RequestParameters,
        RespT = template::Result,
        MetaT = (),
    > {
        pub method: MethodT,
        pub params: Option<ParamsT>,
        pub interest: Interest<RespT, SendE, RecvE, MetaT>,
    }

    /// How a [`Dispatcher`] should handle a message.
    #[derive(Debug)]
    pub enum Interest<RespT = template::Result, SendE = Infallible, RecvE = SendE, MetaT = ()> {
        /// Flush it to the [`Sink`], notifying the sender on failure (dropping it on success)
        Notification(oneshot::Sender<SendE>),
        /// As above, but forward the [`id`](template::Request::id) to the [`Reactor`],
        /// along with the given metadata.
        Dialogue {
            sender: oneshot::Sender<Result<RespT, Direction<SendE, RecvE>>>,
            meta: MetaT,
        },
    }

    /// Which half of message handling an error pertains to.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Direction<SendE = Infallible, RecvE = SendE> {
        Sending(SendE),
        Receiving(RecvE),
    }

    impl<SendE, RecvE> fmt::Display for Direction<SendE, RecvE> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Direction::Sending(_) => f.write_str("error sending message"),
                Direction::Receiving(_) => f.write_str("error receiving response"),
            }
        }
    }

    impl<SendE, RecvE> std::error::Error for Direction<SendE, RecvE>
    where
        SendE: std::error::Error + 'static,
        RecvE: std::error::Error + 'static,
    {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            Some(match self {
                Direction::Sending(it) => it,
                Direction::Receiving(it) => it,
            })
        }
    }

    #[derive(Debug)]
    enum DispatchState {
        Alive,
        FlushSink,
        StreamDead,
        SinkDead,
        Dead,
    }

    /// The request that the [`Dispatcher`] is currently flushing into its [`Sink`].
    enum Outstanding<
        RespT = template::Result,
        SendE = Infallible,
        RecvE = SendE,
        IdT = template::Id,
        MetaT = (),
    > {
        Notification(oneshot::Sender<SendE>),
        Dialogue {
            id: IdT,
            sender: oneshot::Sender<Result<RespT, Direction<SendE, RecvE>>>,
            meta: MetaT,
        },
    }

    pin_project! {
    /// Receives [`DispatchRequest`]s, and flush each into its sink, one at a time.
    ///
    /// [sender](oneshot::Sender)s are notified of sink failures,
    /// and [`Interest::Dialogue`]s are yielded as [`ReactorRequest`]s.
    pub struct Dispatcher<StreamT, SinkT, RespT = template::Result, IdT = template::Id, SendE = Infallible, RecvE = SendE, MetaT = (), IdI = Ids> {
        #[pin] stream: StreamT,
        #[pin] sink: SinkT,
        state: DispatchState,
        // Always [`Some`] if [`DispatchState::FlushSink`].
        outstanding: Option<Outstanding<RespT, SendE, RecvE, IdT, MetaT>>,
        ids: IdI,
    }}

    impl<StreamT, SinkT, RespT, IdT, SendE, RecvE, MetaT, IdI>
        Dispatcher<StreamT, SinkT, RespT, IdT, SendE, RecvE, MetaT, IdI>
    {
        /// `ids` MUST NOT be exhausted during the lifetime of the dispatcher.
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

    pub struct ReactorRequest<
        IdT = template::Id,
        RespT = template::Result,
        SendE = Infallible,
        RecvE = SendE,
        MetaT = (),
    > {
        pub id: IdT,
        pub sender: oneshot::Sender<Result<RespT, Direction<SendE, RecvE>>>,
        pub meta: MetaT,
    }

    impl<StreamT, SinkT, RespT, IdT, SendE, RecvE, MetaT, IdI, MethodT, ParamsT> Stream
        for Dispatcher<StreamT, SinkT, RespT, IdT, SendE, RecvE, MetaT, IdI>
    where
        StreamT: Stream<Item = DispatchRequest<SendE, RecvE, MethodT, ParamsT, RespT, MetaT>>,
        SinkT: Sink<template::Request<MethodT, IdT, ParamsT>, Error = SendE>,
        IdI: Iterator<Item = IdT>,
        IdT: Clone,
    {
        type Item = Result<ReactorRequest<IdT, RespT, SendE, RecvE, MetaT>, SendE>;

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
                            (
                                Ok(()),
                                Some(DispatchRequest {
                                    method,
                                    params,
                                    interest,
                                }),
                            ) => match interest {
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
                                Interest::Dialogue { sender, meta } => {
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
                                                Some(Outstanding::Dialogue { id, sender, meta });
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
                            (
                                Err(e),
                                Some(DispatchRequest {
                                    method: _,
                                    params: _,
                                    interest,
                                }),
                            ) => {
                                *this.state = DispatchState::SinkDead;
                                match interest {
                                    Interest::Notification(sender) => match sender.send(e) {
                                        Ok(()) => {}
                                        Err(e) => return Poll::Ready(Some(Err(e))),
                                    },
                                    Interest::Dialogue { sender, meta: _ } => {
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
                                Outstanding::Dialogue { id, sender, meta } => {
                                    return Poll::Ready(Some(Ok(ReactorRequest {
                                        id,
                                        sender,
                                        meta,
                                    })))
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
                                Outstanding::Dialogue {
                                    id: _,
                                    sender,
                                    meta: _,
                                } => {
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

    impl<StreamT, SinkT, RespT, IdT, SendE, RecvE, MetaT, IdI, MethodT, ParamsT> FusedStream
        for Dispatcher<StreamT, SinkT, RespT, IdT, SendE, RecvE, MetaT, IdI>
    where
        StreamT: Stream<Item = DispatchRequest<SendE, RecvE, MethodT, ParamsT, RespT, MetaT>>,
        SinkT: Sink<template::Request<MethodT, IdT, ParamsT>, Error = SendE>,
        IdI: Iterator<Item = IdT>,
        IdT: Clone,
    {
        fn is_terminated(&self) -> bool {
            matches!(self.state, DispatchState::Dead)
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
