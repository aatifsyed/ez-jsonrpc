//! Completely generic request/response client built on [`tower::Service`](tower_service::Service),
//! [`futures::Stream`](futures_core::Stream) and [`futures::Sink`](futures_sink::Sink).
//!
//! <pre class="mermaid">
//! sequenceDiagram
//!     participant app as Your Application
//!     participant svc as ez_client::Service
//!     participant task as ez_client::Task
//!     participant transport as TransportT
//!
//!     autonumber
//!
//!     app->>svc: RequestT
//!     svc->>task: ez_client::Ask
//!     note over svc,task: message over a Sink/Stream
//!     task->>transport: (RequestT, IdT)
//!     note over task,transport: message over a Sink/Stream
//!     note over task: wait for response...
//!     transport->>task: (ResponseT, IdT)
//!     task->>app: ResponseT
//!
//! </pre>
//!
//! <script type="module">
//!   import mermaid from "https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs";
//!   var doc_theme = localStorage.getItem("rustdoc-theme");
//!   if (doc_theme === "dark" || doc_theme === "ayu") mermaid.initialize({theme: "dark"});
//! </script>
//!
//! 0. Create a [`Service`] and [`Task`].
//!    - Connect them to each other over a [`Stream`]/[`Sink`],
//!      allowing you to configure your own e.g queue depth, MPSC etc.
//!    - Give the [`Task`] a [`Stream`]/[`Sink`] transport to own.
//!      This is typically a wrapper over e.g a websocket connection.
//! 1. You [`call`](tower_service::Service::call) the [`Service`] with a [`Dialogue`].
//! 2. The [`Service`] formats this into an [`Ask`] for the [`Task`].
//! 3. The [`Task`] assigns the request an identifier for correlation using an
//!    [`IdFactory`], and sends the pair to the transport.
//! 4. A response eventually arrives on the transport.
//! 5. The [`Task`] reacts to the response,
//!    resolving the [`Future`](tower_service::Service::Future) returned in `(1)`.
//!
//! [`Task`]s also handle timeouts, and propogating errors from the transport.

#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use core::{
    cmp, fmt,
    future::Future,
    hash::{BuildHasher, Hash, Hasher},
    mem,
    pin::{pin, Pin},
    task::{Context, Poll},
};

use self::{future::*, util::*};
use futures_channel::oneshot;
use futures_core::{FusedStream, Stream};
use futures_sink::Sink;
use futures_util::{future::Either, stream::FuturesUnordered};
use hashbrown::HashMap;
use pin_project::pin_project;

/// Implementor of [`Service`](tower_service::Service) which dispatches to a [`Task`].
pub struct Service<SinkT, ResponseT, TransportE, TimeoutFut, TimeoutE> {
    /// The sink over which to communicate with the [`Task`].
    pub sink: SinkT,
    pub phantom: PhantomDisown<(ResponseT, TransportE, TimeoutFut, TimeoutE)>,
}

impl<SinkT: fmt::Debug, ResponseT, TransportE, TimeoutFut, TimeoutE> fmt::Debug
    for Service<SinkT, ResponseT, TransportE, TimeoutFut, TimeoutE>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Service")
            .field("sink", &self.sink)
            .field("phantom", &self.phantom)
            .finish()
    }
}

impl<SinkT: Clone, ResponseT, TransportE, TimeoutFut, TimeoutE> Clone
    for Service<SinkT, ResponseT, TransportE, TimeoutFut, TimeoutE>
{
    fn clone(&self) -> Self {
        Self {
            sink: self.sink.clone(),
            phantom: PhantomDisown::new(),
        }
    }
}

impl<SinkT, ResponseT, TransportE, TimeoutFut, TimeoutE>
    Service<SinkT, ResponseT, TransportE, TimeoutFut, TimeoutE>
{
    pub const fn new<RequestT>(sink: SinkT) -> Self
    where
        Self: tower_service::Service<Notification<RequestT>>,
    {
        Self {
            sink,
            phantom: PhantomDisown::new(),
        }
    }
}

/// Indicate to a [`Service`] that the [`Task`] should NOT assign
/// an ID, or wait for a response.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Notification<RequestT>(pub RequestT);

impl<SinkT, RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>
    tower_service::Service<Notification<RequestT>>
    for Service<SinkT, ResponseT, TransportE, TimeoutFut, TimeoutE>
where
    SinkT: Sink<Ask<RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>> + Unpin + Clone,
{
    type Response = ();
    type Error = notification::Error<SinkT::Error, TransportE>;
    type Future = notification::Future<
        SinkT,
        SinkT::Error,
        RequestT,
        ResponseT,
        TransportE,
        TimeoutFut,
        TimeoutE,
    >;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        pin!(&mut self.sink)
            .poll_ready(cx)
            .map_err(notification::Error::Task)
    }

    fn call(&mut self, Notification(request): Notification<RequestT>) -> Self::Future {
        let sink = self.sink.clone();
        let sink = mem::replace(&mut self.sink, sink);
        notification::future(request, sink)
    }
}

impl<'a, SinkT, SinkE, RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>
    tower_service::Service<Notification<RequestT>>
    for &'a Service<SinkT, ResponseT, TransportE, TimeoutFut, TimeoutE>
where
    &'a SinkT: Sink<Ask<RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>, Error = SinkE>,
{
    type Response = ();
    type Error = notification::Error<SinkE, TransportE>;
    type Future = notification::Future<
        &'a SinkT,
        SinkE,
        RequestT,
        ResponseT,
        TransportE,
        TimeoutFut,
        TimeoutE,
    >;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        pin!(&mut &self.sink)
            .poll_ready(cx)
            .map_err(notification::Error::Task)
    }

    fn call(&mut self, Notification(request): Notification<RequestT>) -> Self::Future {
        notification::future(request, &self.sink)
    }
}

pub mod notification {
    //! Types when a [`Service`] handles a [`Notification`].
    //!
    //! Qualify these types in your code like [`enum@notification::Error`].

    use futures_channel::oneshot;
    use futures_sink::Sink;
    use futures_util::{
        future::{Map, MapErr},
        FutureExt as _, TryFutureExt as _,
    };
    use thiserror::Error;

    use crate::*;

    /// Error when a [`Service`] handles a [`Notification`].
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
    pub enum Error<TaskE, TransportE> {
        /// Failure to communicate with the application [`Task`].
        #[error("Failure communicating with application task")]
        Task(#[source] TaskE),
        /// Failure in the transport owned by the [`Task`].
        #[error("Failure sending request along transport")]
        Transport(#[source] TransportE),
    }

    pub type Future<SinkT, TaskE, RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE> = OneTwo<
        MapErr<
            SendAndClose<Ask<RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>, SinkT>,
            fn(TaskE) -> Error<TaskE, TransportE>,
        >,
        Map<
            oneshot::Receiver<TransportE>,
            fn(Result<TransportE, oneshot::Canceled>) -> Result<(), Error<TaskE, TransportE>>,
        >,
    >;

    pub(crate) fn future<SinkT, RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>(
        request: RequestT,
        sink: SinkT,
    ) -> Future<SinkT, SinkT::Error, RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>
    where
        SinkT: Sink<Ask<RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>>,
    {
        let (sent, done_send) = oneshot::channel();
        OneTwo::new(
            SendAndClose::new(
                sink,
                Ask {
                    request,
                    sent,
                    kind: ask::Kind::Notification,
                },
            )
            .map_err(Error::Task),
            done_send.map(|res| match res {
                Ok(e) => Err(Error::Transport(e)),
                Err(oneshot::Canceled) => Ok(()),
            }),
        )
    }
}

/// Indicate to a [`Service`] that the [`Task`] should assign
/// an ID, and wait for a response until the [timeout](Self::timeout).
pub struct Dialogue<RequestT, ResponseT, TimeoutFut> {
    /// The request to send to the underlying transport.
    pub request: RequestT,
    /// Stop listening for a response and return this as an error.
    pub timeout: TimeoutFut,
    pub response: PhantomDisown<ResponseT>,
}

impl<RequestT: PartialOrd, ResponseT, TimeoutFut: PartialOrd> PartialOrd
    for Dialogue<RequestT, ResponseT, TimeoutFut>
{
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        let Self {
            request,
            timeout,
            response,
        } = self;
        match request.partial_cmp(&other.request) {
            Some(cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match timeout.partial_cmp(&other.timeout) {
            Some(cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        response.partial_cmp(&other.response)
    }
}

impl<RequestT: Ord, ResponseT, TimeoutFut: Ord> Ord for Dialogue<RequestT, ResponseT, TimeoutFut> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        let Self {
            request,
            timeout,
            response,
        } = self;
        request
            .cmp(&other.request)
            .then_with(|| timeout.cmp(&other.timeout))
            .then_with(|| response.cmp(&other.response))
    }
}

impl<RequestT: Hash, ResponseT, TimeoutFut: Hash> Hash
    for Dialogue<RequestT, ResponseT, TimeoutFut>
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.request.hash(state);
        self.timeout.hash(state);
        self.response.hash(state);
    }
}

impl<RequestT: PartialEq, ResponseT, TimeoutFut: PartialEq> PartialEq
    for Dialogue<RequestT, ResponseT, TimeoutFut>
{
    fn eq(&self, other: &Self) -> bool {
        self.request == other.request
            && self.timeout == other.timeout
            && self.response == other.response
    }
}

impl<RequestT: Eq, ResponseT, TimeoutFut: Eq> Eq for Dialogue<RequestT, ResponseT, TimeoutFut> {}

impl<RequestT: Copy, ResponseT, TimeoutFut: Copy> Copy
    for Dialogue<RequestT, ResponseT, TimeoutFut>
{
}

impl<RequestT: Clone, ResponseT, TimeoutFut: Clone> Clone
    for Dialogue<RequestT, ResponseT, TimeoutFut>
{
    fn clone(&self) -> Self {
        Self {
            request: self.request.clone(),
            timeout: self.timeout.clone(),
            response: self.response,
        }
    }
}

impl<RequestT: fmt::Debug, ResponseT, TimeoutFut: fmt::Debug> fmt::Debug
    for Dialogue<RequestT, ResponseT, TimeoutFut>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Dialogue")
            .field("request", &self.request)
            .field("timeout", &self.timeout)
            .field("response", &self.response)
            .finish()
    }
}

impl<RequestT, ResponseT, TimeoutFut> Dialogue<RequestT, ResponseT, TimeoutFut> {
    pub const fn new(request: RequestT, timeout: TimeoutFut) -> Self {
        Self {
            request,
            timeout,
            response: PhantomDisown::new(),
        }
    }
}

impl<SinkT, RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>
    tower_service::Service<Dialogue<RequestT, ResponseT, TimeoutFut>>
    for Service<SinkT, ResponseT, TransportE, TimeoutFut, TimeoutE>
where
    SinkT: Sink<Ask<RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>> + Unpin + Clone,
{
    type Response = ResponseT;
    type Error = dialogue::Error<SinkT::Error, TransportE, TimeoutE>;
    type Future = dialogue::Future<
        SinkT,
        SinkT::Error,
        RequestT,
        ResponseT,
        TransportE,
        TimeoutFut,
        TimeoutE,
    >;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        pin!(&mut self.sink)
            .poll_ready(cx)
            .map_err(dialogue::Error::Task)
    }

    fn call(
        &mut self,
        Dialogue {
            request, timeout, ..
        }: Dialogue<RequestT, ResponseT, TimeoutFut>,
    ) -> Self::Future {
        let sink = self.sink.clone();
        let sink = mem::replace(&mut self.sink, sink);
        dialogue::future(sink, request, timeout)
    }
}

impl<'a, SinkT, SinkE, RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>
    tower_service::Service<Dialogue<RequestT, ResponseT, TimeoutFut>>
    for &'a Service<SinkT, ResponseT, TransportE, TimeoutFut, TimeoutE>
where
    &'a SinkT: Sink<Ask<RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>, Error = SinkE>,
{
    type Response = ResponseT;
    type Error = dialogue::Error<SinkE, TransportE, TimeoutE>;
    type Future =
        dialogue::Future<&'a SinkT, SinkE, RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        pin!(&mut &self.sink)
            .poll_ready(cx)
            .map_err(dialogue::Error::Task)
    }

    fn call(
        &mut self,
        Dialogue {
            request, timeout, ..
        }: Dialogue<RequestT, ResponseT, TimeoutFut>,
    ) -> Self::Future {
        dialogue::future(&self.sink, request, timeout)
    }
}

pub mod dialogue {
    //! Types when a [`Service`] handles a [`Dialogue`].
    //!
    //! Qualify these types in your code like [`enum@dialogue::Error`].
    use futures_channel::oneshot;
    use futures_sink::Sink;
    use futures_util::{
        future::{Either, Map, MapErr},
        FutureExt as _, TryFutureExt as _,
    };
    use thiserror::Error;

    use crate::*;

    /// Error when a [`Service`] handles a [`Dialogue`].
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
    pub enum Error<TaskE, TransportE, TimeoutE> {
        /// Failure to communicate with the application [`Task`].
        #[error("Failure communicating with application task")]
        Task(#[source] TaskE),

        /// Application [`Task`] was dropped.
        #[error("Application task dropped while waiting for response")]
        TaskDropped,

        /// Failure in the transport owned by the [`Task`].
        #[error("Failure sending request along transport")]
        Send(#[source] TransportE),

        /// Failure in the transport owned by the [`Task`].
        #[error("Failure receiving response from transport")]
        Recv(#[source] TransportE),

        /// The [`timeout`](Dialogue::timeout) expired.
        #[error("Timed out waiting for response")]
        Timeout(#[source] TimeoutE),
    }

    pub type Future<SinkT, SinkE, RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE> = OneTwo<
        MapErr<
            SendAndClose<Ask<RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>, SinkT>,
            fn(SinkE) -> Error<SinkE, TransportE, TimeoutE>,
        >,
        OneTwo<
            Map<
                oneshot::Receiver<TransportE>,
                fn(
                    Result<TransportE, oneshot::Canceled>,
                ) -> Result<(), Error<SinkE, TransportE, TimeoutE>>,
            >,
            Map<
                oneshot::Receiver<Result<ResponseT, Either<TransportE, TimeoutE>>>,
                fn(
                    Result<Result<ResponseT, Either<TransportE, TimeoutE>>, oneshot::Canceled>,
                ) -> Result<ResponseT, Error<SinkE, TransportE, TimeoutE>>,
            >,
        >,
    >;

    pub(crate) fn future<SinkT, RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>(
        sink: SinkT,
        request: RequestT,
        timeout: TimeoutFut,
    ) -> Future<SinkT, SinkT::Error, RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>
    where
        SinkT: Sink<Ask<RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>>,
    {
        let (sent, done_send) = oneshot::channel();
        let (response, done_response) = oneshot::channel();
        OneTwo::new(
            SendAndClose::new(
                sink,
                Ask {
                    request,
                    sent,
                    kind: ask::Kind::Dialogue { response, timeout },
                },
            )
            .map_err(Error::Task),
            OneTwo::new(
                done_send.map(|res| match res {
                    Ok(e) => Err(Error::Send(e)),
                    Err(oneshot::Canceled) => Ok(()),
                }),
                done_response.map(|res| match res {
                    Ok(Ok(resp)) => Ok(resp),
                    Ok(Err(eith)) => Err(match eith {
                        Either::Left(trans) => Error::Recv(trans),
                        Either::Right(time) => Error::Timeout(time),
                    }),
                    Err(oneshot::Canceled) => Err(Error::TaskDropped),
                }),
            ),
        )
    }
}

/// Application-level input to a [`Task`].
pub struct Ask<RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE> {
    /// The outgoing request.
    pub request: RequestT,
    /// [`Task`] will [`flush`](Sink::poll_flush) to its inner transport per-message.
    ///
    /// This receives either the error from that flush, or is dropped once
    /// the message is successfully flushed.
    pub sent: oneshot::Sender<TransportE>,
    /// Whether this is a [`Notification`](ask::Kind::Notification) or a [`Dialogue`](ask::Kind::Dialogue).
    pub kind: ask::Kind<ResponseT, TransportE, TimeoutFut, TimeoutE>,
}

impl<RequestT: fmt::Debug, ResponseT, TransportE, TimeoutFut: fmt::Debug, TimeoutE> fmt::Debug
    for Ask<RequestT, ResponseT, TransportE, TimeoutFut, TimeoutE>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ask")
            .field("request", &self.request)
            .field("sent", &self.sent)
            .field("kind", &self.kind)
            .finish()
    }
}

/// Contains [`ask::Kind`].
pub mod ask {
    use super::*;

    /// The representation of a [`Notification`] or [`Dialogue`].
    ///
    /// See [`Ask::kind`].
    pub enum Kind<ResponseT, TransportE, TimeoutFut, TimeoutE> {
        /// The caller doesn't listen for a response, and an ID is not assigned.
        Notification,
        /// The caller listens for a response or transport error until [`timeout`](Kind::Dialogue::timeout) resolves.
        Dialogue {
            response: oneshot::Sender<Result<ResponseT, Either<TransportE, TimeoutE>>>,
            timeout: TimeoutFut,
        },
    }

    impl<ResponseT, TransportE, TimeoutFut: fmt::Debug, TimeoutE> fmt::Debug
        for Kind<ResponseT, TransportE, TimeoutFut, TimeoutE>
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Notification => write!(f, "Notification"),
                Self::Dialogue { response, timeout } => f
                    .debug_struct("Dialogue")
                    .field("response", response)
                    .field("timeout", timeout)
                    .finish(),
            }
        }
    }
}

/// A transport error for [`Task] which can be broadcast to all clients.
pub trait ErrorFactory {
    type Error;
    fn error(&mut self) -> Self::Error;
    fn map<F, E>(self, f: F) -> error_factory::Map<Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Error) -> E,
    {
        error_factory::Map(self, f)
    }
}

/// Issuer of identifiers for requests in [`Task`].
pub trait IdFactory {
    type Id;
    /// Issue a new unique identifier.
    fn id(&mut self) -> Self::Id;
    /// Return an identifier for possible future reuse.
    fn finish(&mut self, id: Self::Id);
}

/// Generic dispatcher for notifications and request/response pairs.
///
/// Receives a [`Stream`] of [`Ask`]s, which are typically created by [`Service`]s.
/// [`Task`] flushes messages one-by-one to the underlying [`transport`](Task::transport),
/// and errors sending a single message are propogated to the [`call`](tower_service::Service::call)er.
///
/// If the transport fails to be [`ready`](Sink::poll_ready),
/// or an [`ErrorFactory`] is [sent](Stream::poll_next),
/// all incoming and pending calls will receive the error.
/// This can be used to signal an error in the transport.
///
/// [`Task`]s are driven as a [`Stream`] of [`task::Error`]s.
#[pin_project]
#[derive(Debug)]
pub struct Task<
    IdT,
    ResponseT,
    AskSt,
    TransportT,
    TransportE,
    TimeoutFut,
    TimeoutE,
    IdFact,
    BuildHasherT,
> {
    #[pin]
    pub asks: AskSt,
    #[pin]
    pub transport: TransportT,
    #[pin]
    pub timeouts: FuturesUnordered<With<TimeoutFut, IdT>>,
    pub id_factory: IdFact,
    #[expect(clippy::type_complexity)]
    pub map: HashMap<
        IdT,
        oneshot::Sender<Result<ResponseT, Either<TransportE, TimeoutE>>>,
        BuildHasherT,
    >,
    sink_state: sink_state::State<IdT, ResponseT, TransportE, TimeoutFut, TimeoutE>,
}

macro_rules! project_ref {
    ($(
        $(#[$meta:meta])*
        $ident:ident for $field:ident: $ty:ty
    );* $(;)?) => {
        $(
            $(#[$meta])*
            pub fn $ident(self: Pin<&Self>) -> $ty {
                self.project_ref().$field
            }
        )*
    };
}
macro_rules! project_mut {
    ($(
        $(#[$meta:meta])*
        $ident:ident for $field:ident: $ty:ty
    );* $(;)?) => {
        $(
            $(#[$meta])*
            pub fn $ident(self: Pin<&mut Self>) -> $ty {
                self.project().$field
            }
        )*
    };
}

impl<IdT, ResponseT, AskSt, TransportT, TransportE, TimeoutFut, TimeoutE, IdFact, BuildHasherT>
    Task<IdT, ResponseT, AskSt, TransportT, TransportE, TimeoutFut, TimeoutE, IdFact, BuildHasherT>
{
    pub fn new(
        posts: AskSt,
        transport: TransportT,
        id_factory: IdFact,
        hasher: BuildHasherT,
    ) -> Self
    where
        Self: Stream,
    {
        Self {
            asks: posts,
            transport,
            timeouts: FuturesUnordered::new(),
            id_factory,
            sink_state: sink_state::State::NotReady,
            map: HashMap::with_hasher(hasher),
        }
    }
    project_ref! {
        pin_asks_ref for asks: Pin<&AskSt>;
        pin_transport_ref for transport: Pin<&TransportT>;
        pin_timeouts_ref for timeouts: Pin<&FuturesUnordered<With<TimeoutFut, IdT>>>;

        pin_id_factory_ref for id_factory: &IdFact;
        #[expect(clippy::type_complexity)]
        pin_map_ref for map: &HashMap<IdT, oneshot::Sender<Result<ResponseT, Either<TransportE, TimeoutE>>>, BuildHasherT>;
    }
    project_mut! {
        pin_asks_mut for asks: Pin<&mut AskSt>;
        pin_transport_mut for transport: Pin<&mut TransportT>;
        pin_timeouts_mut for timeouts: Pin<&mut FuturesUnordered<With<TimeoutFut, IdT>>>;

        pin_id_factory_mut for id_factory: &mut IdFact;
        #[expect(clippy::type_complexity)]
        pin_map_mut for map: &mut HashMap<IdT, oneshot::Sender<Result<ResponseT, Either<TransportE, TimeoutE>>>, BuildHasherT>;

    }
}

/// Contains [`task::Error`], emitted by a [`Task`].
pub mod task {
    use super::*;
    use thiserror::Error;

    /// An error emmitted by a [`Task`].
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
    pub enum Error<IdT, ResponseT, SendE, TimeoutE> {
        /// The sink was not [ready](Sink::poll_ready), or failed to [close](Sink::poll_close).
        #[error("Sink failed")]
        Sink(#[source] SendE),

        /// Unrecognised ID on a response from the transport.
        ///
        /// This may originate from a timed out message.
        #[error("Unknown ID")]
        NoSuchId(IdT, ResponseT),

        /// A timed out message.
        #[error("Timed out waiting for a response")]
        Timeout(IdT, #[source] TimeoutE),
    }
}

#[expect(clippy::type_complexity)]
mod sink_state {
    use super::*;

    fsmentry::fsmentry! {
    #[derive(Debug)]
    #[fsmentry(entry = pub(crate) Entry)]
    pub(crate) enum State<IdT, ResponseT, TransportE, TimeoutFut, TimeoutE> {
        Flushing((
            oneshot::Sender<TransportE>,
            Option<(IdT, TimeoutFut, oneshot::Sender<Result<ResponseT, Either<TransportE, TimeoutE>>>)>,
        )),
        // live loop
        NotReady -> Ready -> Flushing -> NotReady,
        // dying loop
        NotReady -> Closed,
        Ready -> Closing -> Closed,
    }}
}

impl<IdT, RequestT, ResponseT, AskSt, TransportT, TimeoutFut, IdFact, ErrorFact, BuildHasherT>
    Stream
    for Task<
        IdT,
        ResponseT,
        AskSt,
        TransportT,
        TransportT::Error,
        TimeoutFut,
        TimeoutFut::Output,
        IdFact,
        BuildHasherT,
    >
where
    AskSt:
        Stream<Item = Ask<RequestT, ResponseT, TransportT::Error, TimeoutFut, TimeoutFut::Output>>,
    TransportT: FusedStream<Item = Result<(ResponseT, IdT), ErrorFact>>,
    TransportT: Sink<(RequestT, Option<IdT>)>,
    ErrorFact: ErrorFactory<Error = TransportT::Error>,
    IdFact: IdFactory<Id = IdT>,
    IdT: Hash + Eq + Clone,
    TimeoutFut: Future,
    BuildHasherT: BuildHasher,
{
    type Item = task::Error<IdT, ResponseT, TransportT::Error, TimeoutFut::Output>;
    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let mut this = self.as_mut().project();
        use sink_state::Entry;
        loop {
            match this.sink_state.entry() {
                Entry::NotReady(to) => match this.transport.as_mut().poll_ready(cx) {
                    Poll::Ready(Ok(())) => to.ready(),
                    Poll::Ready(Err(e)) if !this.map.is_empty() => {
                        let Some((_, resp)) = this.map.extract_if(|_, _| true).next() else {
                            unreachable!("already checked map nonempty")
                        };
                        let _ = resp.send(Err(Either::Left(e)));
                    }
                    Poll::Ready(Err(e)) => match this.asks.as_mut().poll_next(cx) {
                        Poll::Ready(None) => {
                            to.closed();
                            return Poll::Ready(Some(task::Error::Sink(e)));
                        }
                        Poll::Ready(Some(Ask { sent, .. })) => {
                            let _ = sent.send(e);
                        }
                        Poll::Pending => break,
                    },
                    Poll::Pending => break,
                },
                Entry::Ready(to) => match this.asks.as_mut().poll_next(cx) {
                    Poll::Ready(None) => to.closing(),
                    Poll::Ready(Some(Ask {
                        request,
                        sent,
                        kind,
                    })) => match kind {
                        ask::Kind::Dialogue { response, timeout } => {
                            let id = this.id_factory.id();
                            match this
                                .transport
                                .as_mut()
                                .start_send((request, Some(id.clone())))
                            {
                                Ok(()) => to.flushing((sent, Some((id, timeout, response)))),
                                Err(e) => {
                                    this.id_factory.finish(id);
                                    let _ = sent.send(e);
                                }
                            }
                        }
                        ask::Kind::Notification => {
                            match this.transport.as_mut().start_send((request, None)) {
                                Ok(()) => to.flushing((sent, None)),
                                Err(e) => {
                                    let _ = sent.send(e);
                                }
                            }
                        }
                    },
                    Poll::Pending => break,
                },
                Entry::Flushing(to) => match this.transport.as_mut().poll_flush(cx) {
                    Poll::Ready(res) => {
                        let (sent, extra) = to.not_ready();
                        match res {
                            Ok(()) => {
                                if let Some((id, timeout, received)) = extra {
                                    this.map.insert(id.clone(), received);
                                    this.timeouts.push(With::new(timeout, id))
                                }
                            }
                            Err(e) => {
                                let _ = sent.send(e);
                            }
                        }
                    }
                    Poll::Pending => break,
                },
                Entry::Closing(to) => match this.transport.as_mut().poll_close(cx) {
                    Poll::Ready(res) => {
                        to.closed();
                        match res {
                            Ok(()) => break,
                            Err(e) => return Poll::Ready(Some(task::Error::Sink(e))),
                        }
                    }
                    Poll::Pending => break,
                },
                Entry::Closed => break,
            }
        }
        while !this.transport.is_terminated() {
            match this.transport.as_mut().poll_next(cx) {
                Poll::Ready(None) => this.map.clear(),
                Poll::Ready(Some(Err(mut errfact))) => {
                    this.map.drain().for_each(|(_, received)| {
                        let _ = received.send(Err(Either::Left(errfact.error())));
                    })
                }
                Poll::Ready(Some(Ok((resp, id)))) => match this.map.remove(&id) {
                    Some(received) => {
                        let _ = received.send(Ok(resp));
                    }
                    None => return Poll::Ready(Some(task::Error::NoSuchId(id, resp))),
                },
                Poll::Pending => break,
            }
        }
        while let Poll::Ready(Some((timeout, id))) = this.timeouts.as_mut().poll_next(cx) {
            if let Some(received) = this.map.remove(&id) {
                match received.send(Err(Either::Right(timeout))) {
                    Ok(()) => {}
                    Err(Err(Either::Right(timeout))) => {
                        return Poll::Ready(Some(task::Error::Timeout(id, timeout)))
                    }
                    _ => unreachable!("we submitted an Err(Right(_)), so expect one back"),
                }
            }
        }
        match self.is_terminated() {
            true => Poll::Ready(None),
            false => Poll::Pending,
        }
    }
}

impl<IdT, ResponseT, AskSt, TransportT, TransportE, TimeoutFut, TimeoutE, IdFact, BuildHasherT>
    FusedStream
    for Task<
        IdT,
        ResponseT,
        AskSt,
        TransportT,
        TransportE,
        TimeoutFut,
        TimeoutE,
        IdFact,
        BuildHasherT,
    >
where
    Self: Stream,
    TransportT: FusedStream,
{
    fn is_terminated(&self) -> bool {
        matches!(self.sink_state, sink_state::State::Closed) && self.transport.is_terminated()
    }
}

/// Utility implementations of [`IdFactory`].
pub mod id_factory {
    use crate::*;
    use alloc::boxed;

    impl<T: IdFactory> IdFactory for &mut T {
        type Id = T::Id;
        fn id(&mut self) -> Self::Id {
            T::id(self)
        }
        fn finish(&mut self, id: Self::Id) {
            T::finish(self, id);
        }
    }
    impl<T: IdFactory> IdFactory for boxed::Box<T> {
        type Id = T::Id;
        fn id(&mut self) -> Self::Id {
            T::id(self)
        }
        fn finish(&mut self, id: Self::Id) {
            T::finish(self, id);
        }
    }

    /// Issue identifiers by repeatedly calling the given closure.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
    pub struct Caller<F>(pub F);
    impl<T, F: FnMut() -> T> IdFactory for Caller<F> {
        type Id = T;
        fn id(&mut self) -> Self::Id {
            (self.0)()
        }
        fn finish(&mut self, _: Self::Id) {}
    }

    /// Issue identifiers from an [Iterator](core::iter::Iterator).
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
    pub struct Iterator<I>(pub I);
    impl<I> Iterator<I> {
        /// The iterator MUST NOT be exhausted in the lifetime of the factory,
        /// else issuing a new identifier will panic.
        pub fn new<II: IntoIterator<IntoIter = I>>(ii: II) -> Self {
            Self(ii.into_iter())
        }
    }
    impl<I: core::iter::Iterator> IdFactory for Iterator<I> {
        type Id = I::Item;
        /// # Panics
        /// - If the iterator is exhausted
        fn id(&mut self) -> Self::Id {
            self.0.next().expect("Ran out of IDs.")
        }
        fn finish(&mut self, _: Self::Id) {}
    }
    impl<L: IdFactory, R: IdFactory<Id = L::Id>> IdFactory for Either<L, R> {
        type Id = L::Id;
        fn id(&mut self) -> Self::Id {
            match self {
                Either::Left(it) => it.id(),
                Either::Right(it) => it.id(),
            }
        }
        fn finish(&mut self, id: Self::Id) {
            match self {
                Either::Left(it) => it.finish(id),
                Either::Right(it) => it.finish(id),
            }
        }
    }
}

/// Utility implementors of [`ErrorFactory`].
pub mod error_factory {
    use core::{convert::Infallible, marker::PhantomData};

    use crate::*;
    use alloc::boxed;

    impl<T: ErrorFactory> ErrorFactory for &mut T {
        type Error = T::Error;
        fn error(&mut self) -> Self::Error {
            T::error(self)
        }
    }
    impl<T: ErrorFactory> ErrorFactory for boxed::Box<T> {
        type Error = T::Error;
        fn error(&mut self) -> Self::Error {
            T::error(self)
        }
    }

    impl ErrorFactory for Infallible {
        type Error = Infallible;
        fn error(&mut self) -> Self::Error {
            match *self {}
        }
    }

    /// [`ErrorFactory`] for the [`map`](ErrorFactory::map) combinator.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
    pub struct Map<T, F>(pub(crate) T, pub(crate) F);

    impl<T: ErrorFactory, F: FnMut(T::Error) -> E, E> ErrorFactory for Map<T, F> {
        type Error = E;
        fn error(&mut self) -> Self::Error {
            let Self(t, f) = self;
            f(t.error())
        }
    }

    /// Repeatedly call the given closure to produce errors.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
    pub struct Caller<F>(pub F);
    impl<E, F: FnMut() -> E> ErrorFactory for Caller<F> {
        type Error = E;
        fn error(&mut self) -> Self::Error {
            (self.0)()
        }
    }

    /// Repeatedly clone out the given error.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
    pub struct Cloner<T>(pub T);
    impl<T: Clone> ErrorFactory for Cloner<T> {
        type Error = T;
        fn error(&mut self) -> Self::Error {
            self.0.clone()
        }
    }

    impl<L: ErrorFactory, R: ErrorFactory<Error = L::Error>> ErrorFactory for Either<L, R> {
        type Error = L::Error;
        fn error(&mut self) -> Self::Error {
            match self {
                Either::Left(it) => it.error(),
                Either::Right(it) => it.error(),
            }
        }
    }

    /// An uninhabitable type to yield arbitrary errors.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Conjure<T> {
        never: Infallible,
        conjure: PhantomData<T>,
    }
    impl<T> ErrorFactory for Conjure<T> {
        type Error = T;
        fn error(&mut self) -> Self::Error {
            match self.never {}
        }
    }
}

/// [`Future`]s used by this crate.
pub mod future {
    use core::{
        future::Future,
        pin::Pin,
        task::{ready, Context, Poll},
    };

    use fsmentry::fsmentry;
    use futures_core::FusedFuture;
    use futures_sink::Sink;

    use pin_project::pin_project;

    /// A [`Future`] which [sends](Sink::start_send) the given item into the [sink](Sink), before [closing](Sink::poll_close) it.
    #[pin_project]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct SendAndClose<T, SinkT> {
        #[pin]
        sink: Option<SinkT>,
        item: Option<T>,
    }

    impl<T, SinkT> SendAndClose<T, SinkT> {
        /// `sink` MUST be [`ready`](Sink::poll_ready).
        pub fn new(sink: SinkT, item: T) -> Self
        where
            Self: Future,
        {
            Self {
                sink: Some(sink),
                item: Some(item),
            }
        }
    }

    impl<SinkT: Sink<T>, T> Future for SendAndClose<T, SinkT> {
        type Output = Result<(), SinkT::Error>;
        fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
            let mut this = self.project();
            loop {
                match (this.item.take(), this.sink.as_mut().as_pin_mut()) {
                    (Some(item), Some(sink)) => match sink.start_send(item) {
                        Ok(()) => {}
                        Err(e) => {
                            this.sink.set(None);
                            break Poll::Ready(Err(e));
                        }
                    },
                    (None, Some(sink)) => {
                        let closed = ready!(sink.poll_close(cx));
                        this.sink.set(None);
                        break Poll::Ready(closed);
                    }
                    (None, None) => panic!("future polled after completion"),
                    (Some(_), None) => unreachable!(),
                }
            }
        }
    }

    impl<SinkT: Sink<T>, T> FusedFuture for SendAndClose<T, SinkT> {
        fn is_terminated(&self) -> bool {
            self.item.is_none() && self.sink.is_none()
        }
    }
    /// Execute two futures, one after the other, short-circuiting on error.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[pin_project]
    pub struct OneTwo<F1, F2> {
        #[pin]
        f1: F1,
        #[pin]
        f2: F2,
        state: OneTwoState,
    }

    impl<F1, F2> OneTwo<F1, F2> {
        pub const fn new(f1: F1, f2: F2) -> Self
        where
            Self: Future,
        {
            Self {
                f1,
                f2,
                state: OneTwoState::F1,
            }
        }
    }

    fsmentry! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        enum OneTwoState {
            F1 -> F2 -> Done,
            F1 -> Done,
        }
    }

    impl<T1, E1, F1, T2, E2, F2> Future for OneTwo<F1, F2>
    where
        F1: Future<Output = Result<T1, E1>>,
        F2: Future<Output = Result<T2, E2>>,
        E2: From<E1>,
    {
        type Output = Result<T2, E2>;
        fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
            let mut this = self.project();
            loop {
                match this.state.entry() {
                    OneTwoStateEntry::Done => panic!("future polled after completion"),
                    OneTwoStateEntry::F1(to) => match ready!(this.f1.as_mut().poll(cx)) {
                        Ok(_) => to.f2(),
                        Err(e) => {
                            to.done();
                            break Poll::Ready(Err(e.into()));
                        }
                    },
                    OneTwoStateEntry::F2(to) => {
                        let res = ready!(this.f2.as_mut().poll(cx));
                        to.done();
                        break Poll::Ready(res);
                    }
                }
            }
        }
    }
    impl<T1, E1, F1, T2, E2, F2> FusedFuture for OneTwo<F1, F2>
    where
        F1: Future<Output = Result<T1, E1>>,
        F2: Future<Output = Result<T2, E2>>,
        E2: From<E1>,
    {
        fn is_terminated(&self) -> bool {
            matches!(self.state, OneTwoState::Done)
        }
    }

    /// A [`Future`] that attaches the given `T` on completion.
    #[pin_project]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct With<Fut, T> {
        #[pin]
        fut: Fut,
        with: Option<T>,
    }

    impl<Fut, T> With<Fut, T> {
        pub fn new(fut: Fut, with: T) -> Self
        where
            Self: Future,
        {
            Self {
                fut,
                with: Some(with),
            }
        }
    }

    impl<Fut, IdT> Future for With<Fut, IdT>
    where
        Fut: Future,
    {
        type Output = (Fut::Output, IdT);

        fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
            let this = self.project();
            let out = ready!(this.fut.poll(cx));
            Poll::Ready((
                out,
                this.with.take().expect("future polled after completion"),
            ))
        }
    }
    impl<Fut, IdT> FusedFuture for With<Fut, IdT>
    where
        Self: Future,
    {
        fn is_terminated(&self) -> bool {
            self.with.is_none()
        }
    }
}

pub mod util {
    //! General utilities.

    use core::{
        any, cmp,
        convert::Infallible,
        fmt,
        hash::{Hash, Hasher},
        marker::PhantomData,
        pin::Pin,
        task::{Context, Poll},
    };

    use futures_core::{FusedStream, Stream};
    use futures_sink::Sink;

    use pin_project::pin_project;

    /// Marker for not owning a `T`.
    pub struct PhantomDisown<T>(PhantomData<fn() -> T>);

    impl<T> PhantomDisown<T> {
        pub const fn new() -> Self {
            Self(PhantomData)
        }
    }

    impl<T> Clone for PhantomDisown<T> {
        fn clone(&self) -> Self {
            *self
        }
    }
    impl<T> Copy for PhantomDisown<T> {}
    impl<T> cmp::Ord for PhantomDisown<T> {
        fn cmp(&self, _: &Self) -> cmp::Ordering {
            cmp::Ordering::Equal
        }
    }
    impl<T> cmp::PartialOrd for PhantomDisown<T> {
        fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
            Some(self.cmp(other))
        }
    }
    impl<T> cmp::Eq for PhantomDisown<T> {}
    impl<T> cmp::PartialEq for PhantomDisown<T> {
        fn eq(&self, _: &Self) -> bool {
            true
        }
    }
    impl<T> fmt::Debug for PhantomDisown<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_fmt(format_args!("PhantomDisown<{}>", any::type_name::<T>()))
        }
    }
    impl<T> Default for PhantomDisown<T> {
        fn default() -> Self {
            Self::new()
        }
    }
    impl<T> Hash for PhantomDisown<T> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.0.hash(state);
        }
    }

    /// Utilities around the [`Extend`] trait.
    pub mod extend {
        use super::*;

        /// A [`Sink`](futures_sink::Sink) which [`Extend`]s an inner collection.
        #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[pin_project]
        pub struct Sink<C>(pub C);

        impl<C> Sink<C> {
            pub const fn new<T>(c: C) -> Self
            where
                Self: futures_sink::Sink<T>,
            {
                Self(c)
            }
        }

        impl<T, C: Extend<T>> futures_sink::Sink<T> for Sink<C> {
            type Error = Infallible;
            fn poll_ready(
                self: Pin<&mut Self>,
                _: &mut Context<'_>,
            ) -> Poll<Result<(), Self::Error>> {
                Poll::Ready(Ok(()))
            }
            fn start_send(mut self: Pin<&mut Self>, item: T) -> Result<(), Self::Error> {
                self.0.extend([item]);
                Ok(())
            }
            fn poll_flush(
                self: Pin<&mut Self>,
                _: &mut Context<'_>,
            ) -> Poll<Result<(), Self::Error>> {
                Poll::Ready(Ok(()))
            }
            fn poll_close(
                self: Pin<&mut Self>,
                _: &mut Context<'_>,
            ) -> Poll<Result<(), Self::Error>> {
                Poll::Ready(Ok(()))
            }
        }

        /// The missing blanket impl for [`Extend`].
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct ByRef<'a, C>(pub &'a mut C);

        impl<'a, C> ByRef<'a, C> {
            pub fn new<T>(c: &'a mut C) -> Self
            where
                Self: Extend<T>,
            {
                Self(c)
            }
        }
        impl<T, C> Extend<T> for ByRef<'_, C>
        where
            C: Extend<T>,
        {
            fn extend<II: IntoIterator<Item = T>>(&mut self, iter: II) {
                self.0.extend(iter);
            }
        }
    }

    /// Combine a [`Sink`] and a [`Stream`] into a single transport.
    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[pin_project]
    pub struct Transport<SinkT, StreamT> {
        #[pin]
        pub sink: SinkT,
        #[pin]
        pub stream: StreamT,
    }
    impl<SinkT, StreamT> Transport<SinkT, StreamT> {
        pub fn new(sink: SinkT, stream: StreamT) -> Self {
            Self { sink, stream }
        }
        pub fn pin_sink_mut(self: Pin<&mut Self>) -> Pin<&mut SinkT> {
            self.project().sink
        }
        pub fn pin_stream_mut(self: Pin<&mut Self>) -> Pin<&mut StreamT> {
            self.project().stream
        }
    }
    impl<T, SinkT: Sink<T>, StreamT> Sink<T> for Transport<SinkT, StreamT> {
        type Error = SinkT::Error;
        fn poll_ready(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
            self.pin_sink_mut().poll_ready(cx)
        }
        fn start_send(self: Pin<&mut Self>, item: T) -> Result<(), Self::Error> {
            self.pin_sink_mut().start_send(item)
        }
        fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
            self.pin_sink_mut().poll_flush(cx)
        }
        fn poll_close(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
            self.pin_sink_mut().poll_close(cx)
        }
    }
    impl<SinkT, StreamT: Stream> Stream for Transport<SinkT, StreamT> {
        type Item = StreamT::Item;
        fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
            self.pin_stream_mut().poll_next(cx)
        }
        fn size_hint(&self) -> (usize, Option<usize>) {
            self.stream.size_hint()
        }
    }
    impl<SinkT, StreamT: FusedStream> FusedStream for Transport<SinkT, StreamT> {
        fn is_terminated(&self) -> bool {
            self.stream.is_terminated()
        }
    }

    /// Wrap a given future so that it resolves to an implementor of [`Error`](core::error::Error).
    pub mod timeout {
        use core::task::ready;
        use futures_core::FusedFuture;
        use thiserror::Error;

        use super::*;

        /// Wraps the given future so that it resolves to a [`timeout::Error`](struct@Error).
        pub fn after<T>(fut: T) -> Future<T> {
            Future(fut)
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[pin_project]
        pub struct Future<T>(#[pin] T);
        impl<T: core::future::Future> core::future::Future for Future<T> {
            type Output = Error;
            fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                ready!(self.project().0.poll(cx));
                Poll::Ready(Error)
            }
        }
        impl<T: FusedFuture> FusedFuture for Future<T> {
            fn is_terminated(&self) -> bool {
                self.0.is_terminated()
            }
        }

        /// Slim implementor of [`Error`](core::error::Error).
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
        #[error("Timed out")]
        pub struct Error;
    }
}

#[cfg(all(feature = "std", test))]
mod tests {
    use core::{convert::Infallible, future::pending};
    use std::hash::RandomState;

    use alloc::vec;
    use futures::{future::join, stream, StreamExt};
    use futures_channel::mpsc;
    use tower::ServiceExt as _;

    use crate::{
        id_factory,
        util::{extend, Transport},
        Dialogue, Service,
    };

    use super::*;

    #[test]
    fn test() {
        futures::executor::block_on(async {
            let (svc2task_svc, svc2task_task) = mpsc::unbounded();
            let mut transport_sink = vec![];
            let transport_stream = vec![Ok::<((), i32), Infallible>(((), 0))];
            let (response, task_errors) = join(
                Service::new(svc2task_svc)
                    .oneshot(Dialogue::<_, _, _>::new((), pending::<Infallible>())),
                Task::new(
                    svc2task_task,
                    Transport::new(
                        extend::Sink(extend::ByRef(&mut transport_sink)),
                        stream::iter(transport_stream).fuse(),
                    ),
                    id_factory::Iterator::new(0..),
                    RandomState::new(),
                )
                .collect::<Vec<_>>(),
            )
            .await;
            assert_eq!(transport_sink, [((), Some(0))]);
            assert_eq!(task_errors, []);
            assert!(matches!(response, Ok(())));
        });
    }
}
