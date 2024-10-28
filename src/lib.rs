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
        sync::{Arc, Mutex},
        task::{ready, Context, Poll},
    };

    use either::Either;
    use ez_jsonrpc_types::Id;
    use futures_channel::oneshot;
    use futures_util::Stream;
    use futures_util::{FutureExt, SinkExt as _, StreamExt, TryStreamExt};
    use pin_project_lite::pin_project;
    use serde::{Deserialize, Serialize};
    use serde_json::Value;

    use crate::types::template;

    pub struct Client {}

    async fn executor<
        ClientT,
        SinkT,
        StreamT,
        MethodT,
        RequestParametersT,
        ValueT,
        ValueE,
        StringT,
        StreamE,
    >(
        client: ClientT,
        sink: SinkT,
        stream: StreamT,
    ) where
        ClientT: futures_util::Stream<
            Item = (
                Option<oneshot::Sender<template::Result<ValueT, ValueE, StringT>>>,
                MethodT,
                Option<RequestParametersT>,
            ),
        >,
        StreamT: futures_util::Stream<
            Item = Result<template::Response<ValueT, ValueE, StringT, u64>, StreamE>,
        >,
        SinkT: futures_util::Sink<template::Request<MethodT, u64, RequestParametersT>>,
    {
        let map = Mutex::new(HashMap::new());
        let send_requests = AssignId::new(client, (0..=u64::MAX).cycle(), &map)
            .map(Ok)
            .forward(sink);

        Reactor::new(stream, &map);
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

    impl<StreamT, P, ValueT, ValueE, StringE, IdT, TransportE> Stream for Reactor<StreamT, P>
    where
        StreamT:
            Stream<Item = Result<template::Response<ValueT, ValueE, StringE, IdT>, TransportE>>,
        P: Deref<
            Target = Mutex<
                HashMap<IdT, oneshot::Sender<template::Result<ValueT, ValueE, StringE>>>,
            >,
        >,
        IdT: Hash + Eq,
    {
        type Item = ReactorError<TransportE, ValueT, ValueE, StringE, IdT>;

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
