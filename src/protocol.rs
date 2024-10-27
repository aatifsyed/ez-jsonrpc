use std::{
    convert::Infallible,
    fmt,
    future::{ready, Future, Ready},
    pin::Pin,
    task::{ready, Context, Poll},
};

use futures_util::future::Either;
use pin_project_lite::pin_project;

use crate::types::template;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Layer<HandleErrF, GoneF> {
    handle_err: HandleErrF,
    gone: GoneF,
}

impl<S, HandleErrF, GoneF> tower_layer::Layer<S> for Layer<HandleErrF, GoneF>
where
    HandleErrF: Clone,
    GoneF: Clone,
{
    type Service = Service<S, HandleErrF, GoneF>;
    fn layer(&self, inner: S) -> Self::Service {
        let Self { handle_err, gone } = self;
        Service::new(inner, handle_err.clone(), gone.clone())
    }
}

impl<HandleErrF, GoneF> Layer<HandleErrF, GoneF> {
    pub fn new(handle_err: HandleErrF, gone: GoneF) -> Self {
        Self { handle_err, gone }
    }
}

/// Server middleware that implements the JSON-RPC 2.0 protocol.
///
/// Wrappers an inner service of generic `(method, params)` pairs to generic
/// JSON-RPC [`Result`](crate::types::template::Result)s.
///
/// Handles the following:
/// - Silencing responses to notifications.
/// - Converting inner service errors to JSON-RPC errors.
/// - Fusing to an error if the inner service fails
///   (i.e returns an error from [`Service::poll_ready`](tower_service::Service::poll_ready)).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Service<SvcT, HandleErrF, GoneF> {
    inner: Option<SvcT>,
    handle_err: HandleErrF,
    gone: GoneF,
}

impl<SvcT, HandleErrF, GoneF> Service<SvcT, HandleErrF, GoneF> {
    /// See [`Service`].
    pub fn new(inner: SvcT, handle_err: HandleErrF, gone: GoneF) -> Self {
        Self {
            inner: Some(inner),
            handle_err,
            gone,
        }
    }
}

impl<MethodT, IdT, RequestParametersT, SvcT, ValueT, ValueE, StringT, HandleErrF, GoneF>
    tower_service::Service<template::Request<MethodT, IdT, RequestParametersT>>
    for Service<SvcT, HandleErrF, GoneF>
where
    SvcT: tower_service::Service<
        (MethodT, Option<RequestParametersT>),
        Response = Result<ValueT, template::Error<ValueE, StringT>>,
    >,
    HandleErrF: FnOnce(SvcT::Error) -> template::Error<ValueE, StringT> + Clone,
    GoneF: FnMut() -> template::Error<ValueE, StringT>,
{
    type Response = Option<template::Response<ValueT, ValueE, StringT, IdT>>;
    type Error = Infallible;
    type Future = ServiceFuture<SvcT::Future, IdT, HandleErrF, Self::Response>;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        match self.inner.as_mut() {
            Some(it) => match it.poll_ready(cx) {
                Poll::Ready(Ok(())) => Poll::Ready(Ok(())),
                Poll::Ready(Err(_)) => {
                    self.inner.take(); // return server errors from now on
                    Poll::Ready(Ok(()))
                }
                Poll::Pending => Poll::Pending,
            },
            None => Poll::Ready(Ok(())),
        }
    }

    fn call(&mut self, req: template::Request<MethodT, IdT, RequestParametersT>) -> Self::Future {
        let template::Request { method, params, id } = req;
        ServiceFuture {
            inner: match self.inner.as_mut() {
                Some(inner) => Either::Left(_ServiceFuture {
                    inner: inner.call((method, params)),
                    id,
                    handle_err: Some(self.handle_err.clone()),
                }),
                None => match id {
                    Some(id) => Either::Right(ready(Ok(Some(template::Response {
                        result: Err((self.gone)()),
                        id,
                    })))),
                    None => Either::Right(ready(Ok(None))),
                },
            },
        }
    }
}

pin_project! {
#[derive(Clone)]
pub struct ServiceFuture<Fut, IdT, HandleErrF, Resp>{
    #[pin]
    inner: Either<_ServiceFuture<Fut, IdT, HandleErrF>, Ready<Result<Resp, Infallible>>>,
}}

impl<Fut, IdT, HandleErrF, Resp> Future for ServiceFuture<Fut, IdT, HandleErrF, Resp>
where
    _ServiceFuture<Fut, IdT, HandleErrF>: Future<Output = Result<Resp, Infallible>>,
{
    type Output = Result<Resp, Infallible>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.project().inner.poll(cx)
    }
}

pin_project! {
#[derive(Clone)]
struct _ServiceFuture<Fut, IdT, HandleErrF> {
    #[pin]
    inner: Fut,
    id: Option<IdT>,
    handle_err: Option<HandleErrF>
}}

impl<F, IdT, HandleErrF> fmt::Debug for _ServiceFuture<F, IdT, HandleErrF> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ProtocolServiceFuture")
            .finish_non_exhaustive()
    }
}

impl<F, IdT, ValueT, ValueE, StringT, E, HandleErrF> Future for _ServiceFuture<F, IdT, HandleErrF>
where
    F: Future<Output = Result<Result<ValueT, template::Error<ValueE, StringT>>, E>>,
    HandleErrF: FnOnce(E) -> template::Error<ValueE, StringT>,
{
    type Output = Result<Option<template::Response<ValueT, ValueE, StringT, IdT>>, Infallible>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();
        let res = ready!(this.inner.poll(cx));
        Poll::Ready(match this.id.take() {
            Some(id) => match res {
                Ok(Ok(ok)) => Ok(Some(template::Response { result: Ok(ok), id })),
                Ok(Err(e)) => Ok(Some(template::Response { result: Err(e), id })),
                Err(e) => Ok(Some(template::Response {
                    result: {
                        let handle_err = this
                            .handle_err
                            .take()
                            .expect("future polled after completion");
                        Err(handle_err(e))
                    },
                    id,
                })),
            },
            None => Ok(None),
        })
    }
}
