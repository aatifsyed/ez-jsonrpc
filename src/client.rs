use core::{
    convert::Infallible,
    future::{Future, Pending},
    hash::{BuildHasher, Hash},
    num::Wrapping,
    pin::Pin,
    task::{ready, Context, Poll},
};

use std::hash::RandomState;

use ez_client::{Dialogue, ErrorFactory, IdFactory, Notification};
use ez_jsonrpc_types::template;
use futures_channel::mpsc;
use futures_util::{stream::FusedStream, Sink, Stream};
use pin_project::pin_project;
use tower_util::ServiceExt as _;

#[expect(clippy::type_complexity)]
pub fn new<
    TransportT,
    TimeoutFut,
    TimeoutE,
    MethodT,
    RequestParametersT,
    ValueT,
    ValueE,
    StringE,
    IdFact,
    ErrFact,
    BuildHasherT,
>(
    buffer: usize,
    transport: TransportT,
    id_factory: IdFact,
    hasher: BuildHasherT,
) -> (
    Client<
        TransportT::Error,
        TimeoutFut,
        TimeoutFut::Output,
        MethodT,
        RequestParametersT,
        ValueT,
        ValueE,
        StringE,
    >,
    Task<
        TransportT,
        TransportT::Error,
        TimeoutFut,
        TimeoutFut::Output,
        MethodT,
        RequestParametersT,
        ValueT,
        ValueE,
        StringE,
        IdFact,
        IdFact::Id,
        BuildHasherT,
    >,
)
where
    IdFact: IdFactory,
    IdFact::Id: Hash + Eq + Clone,
    BuildHasherT: BuildHasher,
    TimeoutFut: Future,
    TransportT: FusedStream<Item = Result<template::Response<ValueT, ValueE, StringE, IdFact::Id>, ErrFact>>
        + Sink<template::Request<MethodT, IdFact::Id, RequestParametersT>>,
    ErrFact: ErrorFactory<Error = TransportT::Error>,
{
    let (from_client, to_task) = mpsc::channel(buffer);
    let client = Client(ez_client::Service::new(from_client));
    let task = Task(ez_client::Task::new(
        to_task,
        Adapt(transport),
        id_factory,
        hasher,
    ));
    (client, task)
}

#[derive(Debug)]
pub struct Client<
    TransportE = Infallible,
    TimeoutFut = Pending<Infallible>,
    TimeoutE = Infallible,
    MethodT = String,
    RequestParametersT = template::RequestParameters,
    ValueT = serde_json::Value,
    ValueE = ValueT,
    StringE = String,
>(
    #[expect(clippy::type_complexity)]
    ez_client::Service<
        mpsc::Sender<
            ez_client::Ask<
                (MethodT, Option<RequestParametersT>),
                template::Result<ValueT, ValueE, StringE>,
                TransportE,
                TimeoutFut,
                TimeoutE,
            >,
        >,
        template::Result<ValueT, ValueE, StringE>,
        TransportE,
        TimeoutFut,
        TimeoutE,
    >,
);

impl<TransportE, TimeoutFut, TimeoutE, MethodT, RequestParametersT, ValueT, ValueE, StringE> Clone
    for Client<
        TransportE,
        TimeoutFut,
        TimeoutE,
        MethodT,
        RequestParametersT,
        ValueT,
        ValueE,
        StringE,
    >
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<TransportE, TimeoutFut, TimeoutE, MethodT, RequestParametersT, ValueT, ValueE, StringE>
    Client<TransportE, TimeoutFut, TimeoutE, MethodT, RequestParametersT, ValueT, ValueE, StringE>
{
    #[expect(clippy::type_complexity)]
    pub fn notify(
        &self,
        method: MethodT,
        params: Option<RequestParametersT>,
    ) -> tower_util::Oneshot<
        ez_client::Service<
            mpsc::Sender<
                ez_client::Ask<
                    (MethodT, Option<RequestParametersT>),
                    Result<ValueT, template::Error<ValueE, StringE>>,
                    TransportE,
                    TimeoutFut,
                    TimeoutE,
                >,
            >,
            Result<ValueT, template::Error<ValueE, StringE>>,
            TransportE,
            TimeoutFut,
            TimeoutE,
        >,
        Notification<(MethodT, Option<RequestParametersT>)>,
    > {
        self.0.clone().oneshot(Notification((method, params)))
    }
    #[expect(clippy::type_complexity)]
    pub fn request(
        &self,
        method: MethodT,
        params: Option<RequestParametersT>,
        timeout: TimeoutFut,
    ) -> tower_util::Oneshot<
        ez_client::Service<
            mpsc::Sender<
                ez_client::Ask<
                    (MethodT, Option<RequestParametersT>),
                    Result<ValueT, template::Error<ValueE, StringE>>,
                    TransportE,
                    TimeoutFut,
                    TimeoutE,
                >,
            >,
            Result<ValueT, template::Error<ValueE, StringE>>,
            TransportE,
            TimeoutFut,
            TimeoutE,
        >,
        Dialogue<
            (MethodT, Option<RequestParametersT>),
            Result<ValueT, template::Error<ValueE, StringE>>,
            TimeoutFut,
        >,
    > {
        self.0
            .clone()
            .oneshot(Dialogue::new((method, params), timeout))
    }
}

#[derive(Debug)]
pub struct Task<
    TransportT,
    TransportE = Infallible,
    TimeoutFut = Pending<Infallible>,
    TimeoutE = Infallible,
    MethodT = String,
    RequestParametersT = template::RequestParameters,
    ValueT = serde_json::Value,
    ValueE = ValueT,
    StringE = String,
    IdFact = DefaultIdFactory,
    IdT = template::Id,
    BuildHasherT = RandomState,
>(
    #[expect(clippy::type_complexity)]
    ez_client::Task<
        IdT,
        template::Result<ValueT, ValueE, StringE>,
        mpsc::Receiver<
            ez_client::Ask<
                (MethodT, Option<RequestParametersT>),
                template::Result<ValueT, ValueE, StringE>,
                TransportE,
                TimeoutFut,
                TimeoutE,
            >,
        >,
        Adapt<TransportT>,
        TransportE,
        TimeoutFut,
        TimeoutE,
        IdFact,
        BuildHasherT,
    >,
);

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefaultIdFactory(Wrapping<u64>);

impl DefaultIdFactory {
    pub fn new() -> Self {
        Self(Wrapping(0))
    }
}

impl IdFactory for DefaultIdFactory {
    type Id = template::Id;
    fn id(&mut self) -> Self::Id {
        self.0 += 1;
        template::Id::Number(self.0 .0.into())
    }
    fn finish(&mut self, id: Self::Id) {
        let _ = id;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[pin_project]
struct Adapt<T>(#[pin] pub T);

impl<T, ValueT, ValueE, StringE, IdT, ErrFact> Stream for Adapt<T>
where
    T: Stream<Item = Result<template::Response<ValueT, ValueE, StringE, IdT>, ErrFact>>,
{
    type Item = Result<(template::Result<ValueT, ValueE, StringE>, IdT), ErrFact>;
    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        Poll::Ready(match ready!(self.project().0.poll_next(cx)) {
            Some(Err(e)) => Some(Err(e)),
            Some(Ok(template::Response { result, id })) => Some(Ok((result, id))),
            None => None,
        })
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}
impl<T> FusedStream for Adapt<T>
where
    Self: Stream,
    T: FusedStream,
{
    fn is_terminated(&self) -> bool {
        self.0.is_terminated()
    }
}

impl<T, MethodIdT, IdT, RequestParametersT>
    Sink<((MethodIdT, Option<RequestParametersT>), Option<IdT>)> for Adapt<T>
where
    T: Sink<template::Request<MethodIdT, IdT, RequestParametersT>>,
{
    type Error = T::Error;
    fn poll_ready(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.project().0.poll_ready(cx)
    }
    fn start_send(
        self: Pin<&mut Self>,
        ((method, params), id): ((MethodIdT, Option<RequestParametersT>), Option<IdT>),
    ) -> Result<(), Self::Error> {
        self.project()
            .0
            .start_send(template::Request { method, params, id })
    }
    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.project().0.poll_flush(cx)
    }
    fn poll_close(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.project().0.poll_close(cx)
    }
}
