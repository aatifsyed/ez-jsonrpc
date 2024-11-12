use core::fmt;
use std::{
    collections::HashMap,
    convert::Infallible,
    future::{self, Future},
    hash::{BuildHasher, Hash, RandomState},
    marker::PhantomData,
    mem,
    pin::{pin, Pin},
    task::{ready, Context, Poll},
};

use either::Either;
use ez_jsonrpc_types::template;
use futures_channel::oneshot::{self, Canceled};
use futures_util::{
    future::Pending, stream::FusedStream, stream::FuturesUnordered, FutureExt as _, Sink,
    SinkExt as _, Stream, TryFutureExt as _,
};
use pin_project_lite::pin_project;

#[derive(Clone, Copy)]
pub struct Client<
    SinkT,
    SendE = Infallible,
    RecvE = SendE,
    ValueT = serde_json::Value,
    ValueE = serde_json::Value,
    StringE = String,
    TimeoutFut = Pending<()>,
> {
    sink: SinkT,
    #[allow(clippy::type_complexity)]
    _phantom: PhantomDisown<(SendE, RecvE, ValueT, ValueE, StringE, TimeoutFut)>,
}

impl<SinkT, SendE, RecvE, ValueT, ValueE, StringE, TimeoutFut>
    Client<SinkT, SendE, RecvE, ValueT, ValueE, StringE, TimeoutFut>
{
    pub fn new(sink: SinkT) -> Self {
        Self {
            sink,
            _phantom: PhantomDisown::new(),
        }
    }
}

impl<SinkT, MethodT, ParamsT, SendE, RecvE, ValueT, ValueE, StringE, TimeoutFut>
    tower_service::Service<MethodCall<MethodT, ParamsT, TimeoutFut>>
    for Client<SinkT, SendE, RecvE, ValueT, ValueE, StringE, TimeoutFut>
where
    SinkT: Sink<DispatchRequest<MethodT, ParamsT, SendE, RecvE, ValueT, ValueE, StringE, TimeoutFut>>
        + Unpin
        + Clone,
{
    type Response = template::Result<ValueT, ValueE, StringE>;
    type Error = MethodCallError<SinkT::Error, SendE, RecvE>;
    type Future = MethodCallFuture<
        ValueT,
        ValueE,
        StringE,
        SinkT::Error,
        SendE,
        RecvE,
        MethodT,
        ParamsT,
        TimeoutFut,
        SinkT,
    >;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        pin!(&mut self.sink)
            .poll_ready(cx)
            .map_err(MethodCallError::Task)
    }

    fn call(&mut self, req: MethodCall<MethodT, ParamsT, TimeoutFut>) -> Self::Future {
        let sink = self.sink.clone();
        let mut sink = mem::replace(&mut self.sink, sink);
        let MethodCall {
            method,
            params,
            timeout,
        } = req;
        let (sent, done_send) = oneshot::channel();
        let (received, done_receive) = oneshot::channel();
        let inner = match sink.start_send_unpin(DispatchRequest {
            method,
            params,
            interest: Interest::Dialogue {
                sent,
                received,
                meta: timeout,
            },
        }) {
            Ok(()) => Either::Right(MethodCallFutInner::new(sink, done_send, done_receive)),
            Err(e) => Either::Left(future::ready(Err(MethodCallError::Task(e)))),
        }
        .fuse();
        MethodCallFuture { inner }
    }
}

fsmentry::dsl! {
    MethodCallFutInnerState {
        Flush -> DoneSend -> DoneReceive -> Done;
        Flush -> Done;
        DoneSend -> Done;
    }
}

pin_project! {
pub struct MethodCallFuture<
    ValueT,
    ValueE,
    StringE,
    SinkE,
    SendE,
    RecvE,
    MethodT,
    ParamsT,
    TimeoutFut,
    SinkT,
> {
    #[pin] inner: futures_util::future::Fuse<
        Either<
            future::Ready<
                Result<template::Result<ValueT, ValueE, StringE>, MethodCallError<SinkE, SendE, RecvE>>,
            >,
            MethodCallFutInner<
                DispatchRequest<
                    MethodT,
                    ParamsT,
                    SendE,
                    RecvE,
                    ValueT,
                    ValueE,
                    StringE,
                    TimeoutFut,
                >,
                SinkT,
                SendE,
                ValueT,
                ValueE,
                StringE,
                RecvE,
            >,
        >,
    >,
}}

impl<ValueT, ValueE, StringE, SinkE, SendE, RecvE, MethodT, ParamsT, TimeoutFut, SinkT> Future
    for MethodCallFuture<
        ValueT,
        ValueE,
        StringE,
        SinkE,
        SendE,
        RecvE,
        MethodT,
        ParamsT,
        TimeoutFut,
        SinkT,
    >
where
    SinkT: Sink<
        DispatchRequest<MethodT, ParamsT, SendE, RecvE, ValueT, ValueE, StringE, TimeoutFut>,
        Error = SinkE,
    >,
{
    type Output = Result<
        template::Result<ValueT, ValueE, StringE>,
        MethodCallError<SinkT::Error, SendE, RecvE>,
    >;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.project().inner.poll(cx)
    }
}

pin_project! {
struct MethodCallFutInner<SinkItem, SinkT, SendE, ValueT, ValueE, StringE, RecvE> {
    state: method_call_fut_inner_state::MethodCallFutInnerState,
    #[pin] sink: SinkT,
    #[pin] done_send: oneshot::Receiver<SendE>,
    #[pin] done_receive: oneshot::Receiver<Result<template::Result<ValueT, ValueE, StringE>, RecvE>>,
    phantom: PhantomDisown<SinkItem>,
}}

impl<SinkItem, SinkT, SendE, ValueT, ValueE, StringE, RecvE>
    MethodCallFutInner<SinkItem, SinkT, SendE, ValueT, ValueE, StringE, RecvE>
{
    fn new(
        sink: SinkT,
        done_send: oneshot::Receiver<SendE>,
        done_receive: oneshot::Receiver<Result<template::Result<ValueT, ValueE, StringE>, RecvE>>,
    ) -> Self {
        Self {
            state: method_call_fut_inner_state::MethodCallFutInnerState::new(
                method_call_fut_inner_state::State::Flush,
            ),
            sink,
            done_send,
            done_receive,
            phantom: PhantomDisown::new(),
        }
    }
}

#[derive(Debug)]
struct PhantomDisown<T>(PhantomData<fn() -> T>);

impl<T> Clone for PhantomDisown<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for PhantomDisown<T> {}
impl<T> PhantomDisown<T> {
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<SinkItem, SinkT, SendE, ValueT, ValueE, StringE, RecvE> Future
    for MethodCallFutInner<SinkItem, SinkT, SendE, ValueT, ValueE, StringE, RecvE>
where
    SinkT: Sink<SinkItem>,
{
    type Output = Result<
        template::Result<ValueT, ValueE, StringE>,
        MethodCallError<SinkT::Error, SendE, RecvE>,
    >;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut this = self.project();
        use method_call_fut_inner_state::Entry;
        loop {
            match this.state.entry() {
                Entry::Flush(st) => match ready!(this.sink.as_mut().poll_flush(cx)) {
                    Ok(()) => st.done_send(),
                    Err(e) => {
                        st.done();
                        return Poll::Ready(Err(MethodCallError::Task(e)));
                    }
                },
                Entry::DoneSend(st) => match ready!(this.done_send.as_mut().poll(cx)) {
                    Ok(e) => {
                        st.done();
                        return Poll::Ready(Err(MethodCallError::Send(e)));
                    }
                    Err(Canceled) => {
                        st.done_receive();
                    }
                },
                Entry::DoneReceive(st) => {
                    let res = ready!(this.done_receive.as_mut().poll(cx));
                    st.done();
                    let res = match res {
                        Ok(Ok(res)) => Ok(res),
                        Ok(Err(e)) => Err(MethodCallError::Recv(e)),
                        Err(Canceled) => Err(MethodCallError::Timeout),
                    };
                    return Poll::Ready(res);
                }
                Entry::Done => panic!("future polled after completion"),
            }
        }
    }
}

#[derive(Debug)]
pub struct MethodCall<MethodT, ParamsT, TimeoutFut> {
    pub method: MethodT,
    pub params: Option<ParamsT>,
    pub timeout: TimeoutFut,
}

#[derive(Debug)]
pub enum MethodCallError<SinkE, SendE, RecvE> {
    Task(SinkE),
    Send(SendE),
    Recv(RecvE),
    Timeout,
}

impl<
        MethodT,
        ParamsT,
        //
        SinkT,
        SendE,
        RecvE,
        ValueT,
        ValueE,
        StringE,
        TimeoutFut,
    > tower_service::Service<Notification<MethodT, ParamsT>>
    for Client<SinkT, SendE, RecvE, ValueT, ValueE, StringE, TimeoutFut>
where
    SinkT: Sink<DispatchRequest<MethodT, ParamsT, SendE, RecvE, ValueT, ValueE, StringE, TimeoutFut>>
        + Unpin
        + Clone,
{
    type Response = ();
    type Error = NotificationError<SinkT::Error, SendE>;
    type Future = NotificationFuture<
        SinkT,
        SinkT::Error,
        SendE,
        DispatchRequest<MethodT, ParamsT, SendE, RecvE, ValueT, ValueE, StringE, TimeoutFut>,
    >;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        pin!(&mut self.sink)
            .poll_ready(cx)
            .map_err(NotificationError::Task)
    }

    fn call(&mut self, req: Notification<MethodT, ParamsT>) -> Self::Future {
        let sink = self.sink.clone();
        let mut sink = mem::replace(&mut self.sink, sink);
        let Notification { method, params } = req;
        let (sender, done_send) = oneshot::channel();
        let inner = {
            match sink.start_send_unpin(DispatchRequest {
                method,
                params,
                interest: Interest::Notification(sender),
            }) {
                Ok(()) => Either::Right(FollowedBy::new(
                    Close::new(sink).map_err(NotificationError::Task as _),
                    done_send.map(
                        (|it| match it {
                            Ok(e) => Err(NotificationError::Send(e)),
                            Err(Canceled) => Ok(()),
                        }) as _,
                    ),
                )),
                Err(e) => Either::Left(future::ready(Err::<(), _>(NotificationError::Task(e)))),
            }
        }
        .fuse();
        NotificationFuture { inner }
    }
}

pin_project! {
pub struct NotificationFuture<SinkT, SinkE, SendE, SinkItem> {
    #[pin] inner: futures_util::future::Fuse<
        Either<
            future::Ready<Result<(), NotificationError<SinkE, SendE>>>,
            FollowedBy<
                futures_util::future::MapErr<
                    Close<
                        SinkT,
                        SinkItem
                    >,
                    fn(SinkE) -> NotificationError<SinkE, SendE>,
                >,
                futures_util::future::Map<
                    oneshot::Receiver<SendE>,
                    fn(
                        Result<SendE, Canceled>,
                    )
                        -> Result<(), NotificationError<SinkE, SendE>>,
                >,
            >,
        >
    >
}}

impl<
        SinkT,
        SinkE,
        SendE,
        //
        MethodT,
        ParamsT,
        RecvE,
        ValueT,
        ValueE,
        StringE,
        MetaT,
    > Future
    for NotificationFuture<
        SinkT,
        SinkE,
        SendE,
        DispatchRequest<MethodT, ParamsT, SendE, RecvE, ValueT, ValueE, StringE, MetaT>,
    >
where
    SinkT: Sink<
        DispatchRequest<MethodT, ParamsT, SendE, RecvE, ValueT, ValueE, StringE, MetaT>,
        Error = SinkE,
    >,
{
    type Output = Result<(), NotificationError<SinkE, SendE>>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.project().inner.poll(cx)
    }
}

pin_project! {
struct FollowedBy<F1, F2> {
    #[pin] f1: Option<F1>,
    #[pin] f2: Option<F2>,
}}

impl<F1, F2> FollowedBy<F1, F2> {
    fn new(f1: F1, f2: F2) -> Self {
        Self {
            f1: Some(f1),
            f2: Some(f2),
        }
    }
}

impl<F1, F2, T1, T2, U> Future for FollowedBy<F1, F2>
where
    F1: Future<Output = Result<T1, U>>,
    F2: Future<Output = Result<T2, U>>,
{
    type Output = Result<T2, U>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut this = self.project();
        loop {
            match (this.f1.as_mut().as_pin_mut(), this.f2.as_mut().as_pin_mut()) {
                (Some(f1), Some(_)) => {
                    let res = ready!(f1.poll(cx));
                    this.f1.set(None);
                    match res {
                        Ok(_t1) => {}
                        Err(e) => {
                            this.f2.set(None);
                            return Poll::Ready(Err(e));
                        }
                    }
                }
                (None, Some(f2)) => {
                    let res = ready!(f2.poll(cx));
                    this.f2.set(None);
                    return Poll::Ready(res);
                }
                (None, None) => panic!("future polled after completion"),
                (Some(_), None) => unreachable!(),
            }
        }
    }
}

pin_project! {
struct Close<SinkT, SinkItem> {
    #[pin] sink: Option<SinkT>,
    phantom: PhantomDisown<SinkItem>,
}}

impl<SinkT, SinkItem> Close<SinkT, SinkItem> {
    fn new(sink: SinkT) -> Self {
        Self {
            sink: Some(sink),
            phantom: PhantomDisown::new(),
        }
    }
}

impl<SinkT, SinkItem> Future for Close<SinkT, SinkItem>
where
    SinkT: Sink<SinkItem>,
{
    type Output = Result<(), SinkT::Error>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match self.as_mut().project().sink.as_pin_mut() {
            Some(sink) => {
                let res = ready!(sink.poll_close(cx));
                self.project().sink.set(None);
                Poll::Ready(res)
            }
            None => panic!("future polled after completion"),
        }
    }
}

#[derive(Debug)]
pub struct Notification<MethodT, ParamsT> {
    pub method: MethodT,
    pub params: Option<ParamsT>,
}

#[derive(Debug)]
pub enum NotificationError<SinkE, SendE> {
    Dropped,
    Task(SinkE),
    Send(SendE),
}

#[derive(Debug)]
pub struct DispatchRequest<MethodT, ParamsT, SendE, RecvE, ValueT, ValueE, StringE, MetaT> {
    pub method: MethodT,
    pub params: Option<ParamsT>,
    pub interest: Interest<SendE, RecvE, ValueT, ValueE, StringE, MetaT>,
}

#[derive(Debug)]
pub enum Interest<SendE, RecvE, ValueT, ValueE, StringE, MetaT> {
    Notification(oneshot::Sender<SendE>),
    Dialogue {
        sent: oneshot::Sender<SendE>,
        received: oneshot::Sender<Result<template::Result<ValueT, ValueE, StringE>, RecvE>>,
        meta: MetaT,
    },
}

fsmentry::dsl! {
    SinkState {
        /// `ReqSt` is exhausted.
        Closing;
        /// Sink::close has been called
        Closed;
        NotReady -> Ready -> Flushing -> NotReady;
        Ready -> Closing;
        NotReady -> Closing;
        Closing -> Closed;
    }
}

fsmentry::dsl! {
    StreamState {
        Running -> Done;
    }
}

impl<
        ReqSt,
        SinkT,
        StreamT,
        IdsT,
        SendE,
        RecvE,
        ValueT,
        ValueE,
        StringE,
        MetaT,
        IdT,
        BuildHasherT,
    >
    Task<
        ReqSt,
        SinkT,
        StreamT,
        IdsT,
        SendE,
        RecvE,
        ValueT,
        ValueE,
        StringE,
        MetaT,
        IdT,
        BuildHasherT,
    >
{
    pub fn new(
        reqs: ReqSt,
        sink: SinkT,
        stream: StreamT,
        ids: IdsT,
        build_hasher: BuildHasherT,
    ) -> Self {
        Self {
            reqs,
            sink,
            stream,
            timeouts: FuturesUnordered::new(),
            ids,
            sink_state: sink_state::SinkState::new(sink_state::State::NotReady),
            stream_state: stream_state::StreamState::new(stream_state::State::Running),
            flush: None,
            map: HashMap::with_hasher(build_hasher),
        }
    }
}

pin_project! {
pub struct Task<ReqSt, SinkT, StreamT, IdsT, SendE = Infallible, RecvE = SendE, ValueT = serde_json::Value, ValueE = serde_json::Value, StringE = String, MetaT = Pending<()>, IdT = template::Id, BuildHasherT = RandomState> {
    #[pin] reqs: ReqSt,
    #[pin] sink: SinkT,
    #[pin] stream: StreamT,
    #[pin] timeouts: FuturesUnordered<Timeout<MetaT, IdT>>,
    ids: IdsT,
    sink_state: sink_state::SinkState,
    stream_state: stream_state::StreamState,
    flush: Option<(oneshot::Sender<SendE>, Option<(IdT, oneshot::Sender<Result<template::Result<ValueT, ValueE, StringE>, RecvE>>, MetaT)>)>,
    map: HashMap<IdT, oneshot::Sender<Result<template::Result<ValueT, ValueE, StringE>, RecvE>>, BuildHasherT>,
}}

impl<
        ReqSt,
        SinkT,
        StreamT,
        IdsT,
        SendE,
        RecvE,
        ValueT,
        ValueE,
        StringE,
        MetaT,
        IdT,
        BuildHasherT,
        //
        MethodT,
        ParamsT,
        //
        MakeErrorF,
    > Stream
    for Task<
        ReqSt,
        SinkT,
        StreamT,
        IdsT,
        SendE,
        RecvE,
        ValueT,
        ValueE,
        StringE,
        MetaT,
        IdT,
        BuildHasherT,
    >
where
    ReqSt: Stream<
        Item = DispatchRequest<MethodT, ParamsT, SendE, RecvE, ValueT, ValueE, StringE, MetaT>,
    >,
    SinkT: Sink<template::Request<MethodT, IdT, ParamsT>, Error = SendE>,
    StreamT: Stream<Item = Input<ValueT, ValueE, StringE, IdT, MakeErrorF>>,
    IdT: Hash + Eq + Clone,
    BuildHasherT: BuildHasher,
    MetaT: Future<Output = ()>,
    IdsT: Iterator<Item = IdT>,
    MakeErrorF: FnMut() -> RecvE,
{
    type Item = TaskError<ValueT, ValueE, StringE, IdT, SendE>;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let mut this = self.as_mut().project();
        loop {
            match this.sink_state.entry() {
                sink_state::Entry::Flushing(st) => match this.sink.as_mut().poll_flush(cx) {
                    Poll::Ready(res) => {
                        let (sender, extra) = this.flush.take().unwrap();
                        match res {
                            Ok(()) => {
                                drop(sender);
                                if let Some((id, received, timeout)) = extra {
                                    this.map.insert(id.clone(), received);
                                    this.timeouts.push(Timeout::new(timeout, id))
                                }
                                st.not_ready();
                            }
                            Err(e) => {
                                let _ = sender.send(e);
                            }
                        }
                    }
                    Poll::Pending => break,
                },
                sink_state::Entry::NotReady(st) => match this.sink.as_mut().poll_ready(cx) {
                    Poll::Ready(Ok(())) => st.ready(),
                    Poll::Ready(Err(e)) => match this.reqs.as_mut().poll_next(cx) {
                        Poll::Ready(None) => st.closing(),
                        Poll::Ready(Some(DispatchRequest {
                            method: _,
                            params: _,
                            interest:
                                Interest::Notification(sent)
                                | Interest::Dialogue {
                                    sent,
                                    received: _,
                                    meta: _,
                                },
                        })) => {
                            let _ = sent.send(e);
                        }
                        Poll::Pending => break,
                    },
                    Poll::Pending => break,
                },
                sink_state::Entry::Ready(st) => {
                    match this.reqs.as_mut().poll_next(cx) {
                        Poll::Ready(None) => st.closing(),
                        Poll::Ready(Some(DispatchRequest {
                            method,
                            params,
                            interest,
                        })) => {
                            match interest {
                                Interest::Notification(sender) => {
                                    match this.sink.as_mut().start_send(template::Request {
                                        method,
                                        params,
                                        id: None,
                                    }) {
                                        Ok(()) => {
                                            *this.flush = Some((sender, None));
                                            st.flushing()
                                        }
                                        Err(e) => {
                                            let _ = sender.send(e);
                                        }
                                    };
                                }
                                Interest::Dialogue {
                                    sent,
                                    received,
                                    meta,
                                } => {
                                    let id = this.ids.next().expect("ran out of IDs");
                                    match this.sink.as_mut().start_send(template::Request {
                                        method,
                                        params,
                                        id: Some(id.clone()),
                                    }) {
                                        Ok(()) => {
                                            *this.flush = Some((sent, Some((id, received, meta))));
                                            st.flushing()
                                        }
                                        Err(e) => {
                                            let _ = sent.send(e);
                                        }
                                    }
                                }
                            };
                        }
                        Poll::Pending => break,
                    };
                }
                sink_state::Entry::Closing(st) => match this.sink.as_mut().poll_close(cx) {
                    Poll::Ready(res) => {
                        st.closed();
                        match res {
                            Ok(()) => {}
                            Err(e) => return Poll::Ready(Some(TaskError::SinkClose(e))),
                        }
                    }
                    Poll::Pending => break,
                },
                sink_state::Entry::Closed => break,
            }
        }

        loop {
            match this.stream_state.entry() {
                stream_state::Entry::Done => break,
                stream_state::Entry::Running(st) => match this.stream.as_mut().poll_next(cx) {
                    Poll::Ready(None) => {
                        this.map.clear();
                        st.done()
                    }
                    Poll::Ready(Some(Input::ReceiveError(mut f))) => {
                        for (_, sender) in this.map.drain() {
                            let _ = sender.send(Err(f()));
                        }
                        st.done()
                    }
                    Poll::Ready(Some(Input::Response(template::Response { result, id }))) => {
                        match this.map.remove(&id) {
                            Some(it) => {
                                let _ = it.send(Ok(result));
                            }
                            None => {
                                return Poll::Ready(Some(TaskError::NoSuchId(template::Response {
                                    result,
                                    id,
                                })))
                            }
                        }
                    }
                    Poll::Pending => break,
                },
            }
        }
        while let Poll::Ready(Some(id)) = this.timeouts.as_mut().poll_next(cx) {
            this.map.remove(&id);
        }
        match self.is_terminated() {
            true => {
                self.project().timeouts.clear();
                Poll::Ready(None)
            }
            false => Poll::Pending,
        }
    }
}

impl<
        ReqSt,
        SinkT,
        StreamT,
        IdsT,
        SendE,
        RecvE,
        ValueT,
        ValueE,
        StringE,
        MetaT,
        IdT,
        BuildHasherT,
        //
        MethodT,
        ParamsT,
        //
        MakeErrorF,
    > FusedStream
    for Task<
        ReqSt,
        SinkT,
        StreamT,
        IdsT,
        SendE,
        RecvE,
        ValueT,
        ValueE,
        StringE,
        MetaT,
        IdT,
        BuildHasherT,
    >
where
    ReqSt: Stream<
        Item = DispatchRequest<MethodT, ParamsT, SendE, RecvE, ValueT, ValueE, StringE, MetaT>,
    >,
    SinkT: Sink<template::Request<MethodT, IdT, ParamsT>, Error = SendE>,
    StreamT: Stream<Item = Input<ValueT, ValueE, StringE, IdT, MakeErrorF>>,
    IdT: Hash + Eq + Clone,
    BuildHasherT: BuildHasher,
    MetaT: Future<Output = ()>,
    IdsT: Iterator<Item = IdT>,
    MakeErrorF: FnMut() -> RecvE,
{
    fn is_terminated(&self) -> bool {
        matches!(
            (self.stream_state.state(), self.sink_state.state()),
            (stream_state::State::Done, sink_state::State::Closed)
        )
    }
}

#[derive(Debug)]
pub enum TaskError<ValueT, ValueE, StringE, IdT, SendE> {
    NoSuchId(template::Response<ValueT, ValueE, StringE, IdT>),
    SinkClose(SendE),
}
#[derive(Debug)]
pub enum Input<ValueT, ValueE, StringE, IdT, MakeErrorF> {
    Response(template::Response<ValueT, ValueE, StringE, IdT>),
    ReceiveError(MakeErrorF),
}

pin_project! {
#[derive(Debug, Clone)]
struct Timeout<Fut, IdT> {
    #[pin] fut: Fut,
    id: Option<IdT>,
}}

impl<Fut, IdT> Timeout<Fut, IdT> {
    fn new(fut: Fut, id: IdT) -> Self {
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
