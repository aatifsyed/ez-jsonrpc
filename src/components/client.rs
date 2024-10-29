use std::{
    collections::HashMap,
    convert::Infallible,
    fmt,
    future::Future,
    hash::{BuildHasher, Hash, RandomState},
    pin::Pin,
    task::{ready, Context, Poll},
};

use futures_channel::oneshot;
use futures_util::{
    future::Pending,
    stream::{FusedStream, FuturesUnordered},
    Sink, Stream,
};
use pin_project_lite::pin_project;
use serde_json::Value;

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

pub struct ReactorRegistration<
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
    type Item = Result<ReactorRegistration<IdT, RespT, SendE, RecvE, MetaT>, SendE>;

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
                                        *this.outstanding = Some(Outstanding::Notification(sender));
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
                                let id = this.ids.next().expect("ID iterator should be infinite");
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
                                return Poll::Ready(Some(Ok(ReactorRegistration {
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
pub struct Reactor<RegS, RespS, TimeoutFut = Pending<()>, IdT = template::Id, RespT = template::Result, SendE = Infallible, RecvE = SendE, BuildHasherT = RandomState> {
    #[pin] registrations: RegS,
    #[pin] responses: RespS,
    #[pin] timeouts: FuturesUnordered<Timeout<TimeoutFut, IdT>>,
    map: HashMap<IdT, oneshot::Sender<Result<RespT, Direction<SendE, RecvE>>>, BuildHasherT>,
}}

impl<RegS, RespS, TimeoutFut, IdT, RespT, SendE, RecvE, BuildHasherT>
    Reactor<RegS, RespS, TimeoutFut, IdT, RespT, SendE, RecvE, BuildHasherT>
{
    pub fn new(registrations: RegS, responses: RespS, build_hasher: BuildHasherT) -> Self {
        Self {
            registrations,
            responses,
            timeouts: FuturesUnordered::new(),
            map: HashMap::with_hasher(build_hasher),
        }
    }
}

impl<RegS, RespS, TimeoutFut, IdT, ValueT, ValueE, StringE, SendE, RecvE, BuildHasherT> Stream
    for Reactor<
        RegS,
        RespS,
        TimeoutFut,
        IdT,
        template::Result<ValueT, ValueE, StringE>,
        SendE,
        RecvE,
        BuildHasherT,
    >
where
    RegS: FusedStream<
        Item = ReactorRegistration<
            IdT,
            template::Result<ValueT, ValueE, StringE>,
            SendE,
            RecvE,
            TimeoutFut,
        >,
    >,
    RespS: FusedStream<Item = template::Response<ValueT, ValueE, StringE, IdT>>,
    TimeoutFut: Future<Output = ()>,
    IdT: Hash + Eq + Clone,
    BuildHasherT: BuildHasher,
{
    type Item = ReactorError<ValueT, ValueE, StringE, IdT>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let mut this = self.project();

        // handle registrations first, they're likely to be in-process,
        // and we don't want to risk a response arriving before the registration.
        while let Poll::Ready(Some(ReactorRegistration { id, sender, meta })) =
            this.registrations.as_mut().poll_next(cx)
        {
            this.map.insert(id.clone(), sender);
            this.timeouts.push(Timeout::new(meta, id));
        }
        while let Poll::Ready(Some(template::Response { result, id })) =
            this.responses.as_mut().poll_next(cx)
        {
            match this.map.remove(&id) {
                Some(sender) => match sender.send(Ok(result)) {
                    Ok(()) => {}
                    Err(Ok(result)) => {
                        return Poll::Ready(Some(ReactorError {
                            kind: ReactorErrorKind::Hangup,
                            response: template::Response { result, id },
                        }))
                    }
                    Err(Err(_)) => unreachable!(),
                },
                None => {
                    return Poll::Ready(Some(ReactorError {
                        kind: ReactorErrorKind::NoSuchId,
                        response: template::Response { result, id },
                    }))
                }
            }
        }
        // finally, handle timeouts
        while let Poll::Ready(Some(id)) = this.timeouts.as_mut().poll_next(cx) {
            this.map.remove(&id);
        }
        match this.registrations.is_terminated() && this.responses.is_terminated() {
            true => Poll::Ready(None),
            false => Poll::Pending,
        }
    }
}

impl<RegS, RespS, TimeoutFut, IdT, ValueT, ValueE, StringE, SendE, RecvE, BuildHasherT> FusedStream
    for Reactor<
        RegS,
        RespS,
        TimeoutFut,
        IdT,
        template::Result<ValueT, ValueE, StringE>,
        SendE,
        RecvE,
        BuildHasherT,
    >
where
    RegS: FusedStream<
        Item = ReactorRegistration<
            IdT,
            template::Result<ValueT, ValueE, StringE>,
            SendE,
            RecvE,
            TimeoutFut,
        >,
    >,
    RespS: FusedStream<Item = template::Response<ValueT, ValueE, StringE, IdT>>,
    TimeoutFut: Future<Output = ()>,
    IdT: Hash + Eq + Clone,
    BuildHasherT: BuildHasher,
{
    fn is_terminated(&self) -> bool {
        self.registrations.is_terminated() && self.responses.is_terminated()
    }
}

impl<RegS, RespS, TimeoutFut, IdT, RespT, SendE, RecvE, BuildHasherT>
    Reactor<RegS, RespS, TimeoutFut, IdT, RespT, SendE, RecvE, BuildHasherT>
{
    pub fn error_with(
        &mut self,
        id: &IdT,
        mut e: impl FnMut() -> RecvE,
    ) -> Result<(), ReactorErrorKind>
    where
        IdT: Hash + Eq,
        BuildHasherT: BuildHasher,
    {
        match self.map.remove(id) {
            Some(sender) => match sender.send(Err(Direction::Receiving(e()))) {
                Ok(()) => Ok(()),
                Err(_) => Err(ReactorErrorKind::Hangup),
            },
            None => Err(ReactorErrorKind::NoSuchId),
        }
    }
    pub fn error_pin_with(
        self: Pin<&mut Self>,
        id: &IdT,
        mut e: impl FnMut() -> RecvE,
    ) -> Result<(), ReactorErrorKind>
    where
        IdT: Hash + Eq,
        BuildHasherT: BuildHasher,
    {
        match self.project().map.remove(id) {
            Some(sender) => match sender.send(Err(Direction::Receiving(e()))) {
                Ok(()) => Ok(()),
                Err(_) => Err(ReactorErrorKind::Hangup),
            },
            None => Err(ReactorErrorKind::NoSuchId),
        }
    }
    pub fn broadcast_with(&mut self, mut e: impl FnMut() -> RecvE) {
        for (_, sender) in self.map.drain() {
            let _ignore_hup = sender.send(Err(Direction::Receiving(e())));
        }
    }
    pub fn broadcast_pin_with(self: Pin<&mut Self>, mut e: impl FnMut() -> RecvE) {
        for (_, sender) in self.project().map.drain() {
            let _ignore_hup = sender.send(Err(Direction::Receiving(e())));
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReactorErrorKind {
    NoSuchId,
    Hangup,
}
impl fmt::Display for ReactorErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReactorErrorKind::NoSuchId => f.write_str("no listener for the given ID"),
            ReactorErrorKind::Hangup => f.write_str("listener for the given ID hung up"),
        }
    }
}

impl std::error::Error for ReactorErrorKind {}

#[derive(Debug, Clone, PartialEq)]
pub struct ReactorError<ValueT = Value, ValueE = ValueT, StringE = String, IdT = template::Id> {
    pub kind: ReactorErrorKind,
    pub response: template::Response<ValueT, ValueE, StringE, IdT>,
}

impl<ValueT, ValueE, StringE, IdT> fmt::Display for ReactorError<ValueT, ValueE, StringE, IdT> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}
impl<ValueT: fmt::Debug, ValueE: fmt::Debug, StringE: fmt::Debug, IdT: fmt::Debug> std::error::Error
    for ReactorError<ValueT, ValueE, StringE, IdT>
{
}

pin_project! {
#[derive(Debug, Clone)]
struct Timeout<Fut, IdT = template::Id> {
    #[pin] fut: Fut,
    id: Option<IdT>,
}}

impl<Fut, IdT> Timeout<Fut, IdT> {
    pub fn new(fut: Fut, id: IdT) -> Self {
        Self { fut, id: Some(id) }
    }
}

impl<Fut, IdT> Future for Timeout<Fut, IdT>
where
    Fut: Future<Output = ()>,
{
    type Output = IdT;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();
        ready!(this.fut.poll(cx));
        Poll::Ready(this.id.take().expect("future polled after completion"))
    }
}
