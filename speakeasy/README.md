<!-- cargo-rdme start -->

Completely generic request/response client built on [`tower::Service`](tower_service::Service),
[`futures::Stream`](futures_core::Stream) and [`futures::Sink`](futures_sink::Sink).

<pre class="mermaid">
sequenceDiagram
    participant app as Your Application
    participant svc as ez_client::Service
    participant task as ez_client::Task
    participant transport as TransportT

    autonumber

    app->>svc: RequestT
    svc->>task: ez_client::Ask
    note over svc,task: message over a Sink/Stream
    task->>transport: (RequestT, IdT)
    note over task,transport: message over a Sink/Stream
    note over task: wait for response...
    transport->>task: (ResponseT, IdT)
    task->>app: ResponseT

</pre>

<script type="module">
  import mermaid from "https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs";
  var doc_theme = localStorage.getItem("rustdoc-theme");
  if (doc_theme === "dark" || doc_theme === "ayu") mermaid.initialize({theme: "dark"});
</script>

0. Create a [`Service`] and [`Task`].
   - Connect them to each other over a [`Stream`]/[`Sink`],
     allowing you to configure your own e.g queue depth, MPSC etc.
   - Give the [`Task`] a [`Stream`]/[`Sink`] transport to own.
     This is typically a wrapper over e.g a websocket connection.
1. You [`call`](tower_service::Service::call) the [`Service`] with a [`Dialogue`].
2. The [`Service`] formats this into an [`Ask`] for the [`Task`].
3. The [`Task`] assigns the request an identifier for correlation using an
   [`IdFactory`], and sends the pair to the transport.
4. A response eventually arrives on the transport.
5. The [`Task`] reacts to the response,
   resolving the [`Future`](tower_service::Service::Future) returned in `(1)`.

[`Task`]s also handle timeouts, and propogating errors from the transport.

<!-- cargo-rdme end -->
