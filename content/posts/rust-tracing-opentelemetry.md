---
title: 'Flexible Tracing with Rust and OpenTelemetry OTLP'
author: Luke
date: 2023-04-06
description: Exporting traces to Jaeger, Honeycomb and other backends using Rust tracing and OpenTelemetry.
thumb: /posts/rust-tracing-opentelemetry/rust-otel.webp
thumb_alt: The OpenTelemetry telescope logo and Ferris, the Rust mascot, superimposed on a Jaeger telemetry trace view
tags: [Rust,Tracing,Telemetry,Actix]
lastmod: 2024-01-12
---

If you have been using log files all your life, the telemetry ecosytem can be a bit daunting for a newcomer. And if you've just discovered Rust's [tracing](https://tokio.rs/#tk-lib-tracing) framework, it's not immediately clear where to look beyond the basic examples. There are a lot of crates and APIs to get your head round and a lot of new terminology. This article isn't intended to be a complete tutorial but will explain how to set up a configuration which works with multiple systems and hopefully provide some insight into how things fit together. For an overview of telemetry, the [OpenTelemetry Observability Primer](https://opentelemetry.io/docs/concepts/observability-primer) is a quick introduction. Note that tracing can theoretically take place across multiple processes, but we'll only be dealing with exporting data from a single web application here. Source code for a working application can be found [on github](https://github.com/tekul/rust-tracing-otlp).

## The Rust tracing and telemetry ecosystem

Rust has its own [tracing framework](https://crates.io/crates/tracing). Getting started with some logging to `stdout` is easy. You can just add:

```rust
tracing_subscriber::fmt().init();
```

to your `main` function. If you are running a web application you can use integrations such as those for [actix](https://docs.rs/tracing-actix-web/latest/tracing_actix_web/) or [axum](https://docs.rs/tower-http/latest/tower_http/trace/index.html), add some [tracing events](https://docs.rs/tracing/0.1.37/tracing/index.html#events-1) to your code and you will see output in the console [^sample-app-middleware].

[^sample-app-middleware]: The example app code is a web application using actix, so the root spans we export are created by the `tracing-actix-web` middleware.

But you want more than console output, right? Tracing links the various "spans" and log events from a single HTTP request together and we want to see them collected in a dashboard so we can track what happened during each request, query the data and so on. At this point you might read about the [`tracing-opentelemetry`](https://crates.io/crates/tracing-opentelemetry) and [`opentelemetry`](https://crates.io/crates/opentelemetry) crates. The `opentelemetry` crate lists yet more crates which you can use to export traces to various backends. One of these is [Jaeger](https://www.jaegertracing.io/) which is a popular open source tool, with a nice UI. You can easily run it locally using Docker, it's often used in blog articles and many of the code samples on `crates.io` use it.

This is roughly where I was about a year ago. I was working on a system which I'd integrated with Rust `tracing` and copied one of those samples to export to Jaeger using the [`opentelemetry-jaeger`](https://crates.io/crates/opentelemetry-jaeger) crate. But ideally we wanted to provide a binary to users and allow them to choose their own telemetry backend. OpenTelemetry didn't seem to be providing a lot of value if we had to compile against a different integration crate each time 🤔.

On revisiting this issue and doing a bit more research, I realised that OpenTelemetry defines [its own protocol](https://opentelemetry.io/docs/reference/specification/protocol/), OTLP (hence the "Open" 🙄), and that it is *directly supported* by Jaeger as well as by many [commercial vendors](https://opentelemetry.io/ecosystem/vendors/). In fact, Jaeger has now deprecated its own clients in favour of OTLP, so the `opentelemetry-jaeger` crate is really redundant.

This makes things much simpler. Hiding in that big list of OpenTelemetry crates is [`opentelemetry-otlp`](https://crates.io/crates/opentelemetry-otlp). If we use that then hopefully our system will "just work" with these compatible backends.

Let's see how far we can get.

## Required crates

For a setup combining Rust tracing and OpenTelemetry/OTLP we need the following crates:

* `tracing` -- to instrument our Rust code.
* `tracing-subscriber` -- allows us to listen for tracing events and define how they are filtered and exported.
* `opentelemetry` -- OpenTelemetry's API-level view of tracing, spans, etc.
* `opentelemetry_sdk` -- Implements the OpenTelemetry APIs [^otel-sdk].
* `tracing-opentelemetry` -- provides a compatibility layer between the two.
* `opentelemetry-otlp` -- the protocol implementation to export data to Jaeger or some other backend.

[^otel-sdk]: The SDK crate was not required in earlier versions of this article, since the functionality was directly available via the `opentelemetry` crate itself.

The `tracing-subscriber` crate also helpfully processes messages generated by code using the `log` crate [^tracing-log], converting them to tracing events. This is useful if you're using a library such as `tokio-postgres` and want to see the interactions with the database during a request.

[^tracing-log]: It defines the `tracing-log` crate as an [optional dependency](https://doc.rust-lang.org/cargo/reference/features.html#optional-dependencies) but it is enabled by default. This confused me a bit to start with because I found references to this feature flag and didn't know that optional dependencies implicitly define features for a crate in Rust.

Some of these crates are maintained by the tokio tracing team and some are part of the OpenTelemetry organization. It's important that the versions are compatible or you can get errors as everything is still quite unstable. Some of the code also requires specific crate features to be enabled. The `Cargo.toml` file from the [example code](https://github.com/tekul/rust-tracing-otlp) can be used as a starting point.

## Basic Tracing Setup

Rust tracing allows us to instrument our code to generate spans and events. We then need to define at least one [`Subscriber`](https://docs.rs/tracing-core/0.1.30/tracing_core/subscriber/trait.Subscriber.html) to capture and log these events somewhere. The code `tracing_subscriber::fmt().init();` creates a simple subscriber which, by default, writes timestamped output to the console similar to a traditional log.
This is roughly equivalent to [^prelude]:

[^prelude]: The `tracing_subscriber::prelude::*` import adds in extension traits which provide the `with` and `init` `Subscriber` methods used here. We'll assume this is used in further code snippets.

```rust
use tracing_subscriber::prelude::*;

let fmt_layer = tracing_subscriber::fmt::layer();
tracing_subscriber::registry().with(fmt_layer).init();
```

[Layers](https://docs.rs/tracing-subscriber/0.3.16/tracing_subscriber/layer/index.html) allow us to use multiple subscribers if we want, and also to add filtering. An easy way to add filtering is to add an [`EnvFilter`](https://docs.rs/tracing-subscriber/0.3.16/tracing_subscriber/struct.EnvFilter.html) as an extra layer:

```rust
tracing_subscriber::registry()
    .with(fmt_layer)
    .with(tracing_subscriber::EnvFilter::from_default_env())
    .init();
```
This will filter the output based on the value of the environment variable `RUST_LOG` [^rust_log].

Now try running the sample application:
```shell
$ RUST_LOG="info" cargo run
```
We see some log messages as the server starts up.
```
2023-04-03T12:17:04.513692Z  INFO rust_tracing_otlp: Starting server
2023-04-03T12:17:04.515377Z  INFO actix_server::builder: starting 2 workers
```

Now if we request the `/rand` endpoint which generates and returns a random number:

```shell
$ curl localhost:8080/rand
Hello. Your random number is 7789005614985398892.
```

But we don't see anything in the server log 🤔. Why doesn't our request span appear? This is part of the way the `fmt` subscriber works. It logs the events in our code (where we have `debug`, `trace` etc), but doesn't log spans by default [^with_span_events]. If we enable debug level logging then we will see output during our request, since we have debug statements in the request handler:

```shell
$ RUST_LOG="debug,h2=warn" cargo run
...
2023-04-03T12:50:27.485688Z  INFO rust_tracing_otlp: Starting server
2023-04-03T12:50:27.487449Z  INFO actix_server::builder: starting 2 workers
2023-04-03T12:50:27.487473Z  INFO actix_server::server: Tokio runtime found; starting in existing Tokio runtime
2023-04-03T12:50:30.626347Z DEBUG HTTP request{http.method=GET http.route=/rand http.flavor=1.1 http.scheme=http http.host=localhost:8080 http.client_ip=127.0.0.1 http.user_agent=curl/8.0.1 http.target=/rand otel.name=HTTP GET /rand otel.kind="server" request_id=a862d880-dd54-4a3f-a175-101b1e00b5f9}:get_random: rust_tracing_otlp: Generating random number
2023-04-03T12:50:30.626389Z DEBUG HTTP request{http.method=GET http.route=/rand http.flavor=1.1 http.scheme=http http.host=localhost:8080 http.client_ip=127.0.0.1 http.user_agent=curl/8.0.1 http.target=/rand otel.name=HTTP GET /rand otel.kind="server" request_id=a862d880-dd54-4a3f-a175-101b1e00b5f9}:get_random: rust_tracing_otlp: Value is 5478471227533226790
```

The additional attributes added by `tracing-actix-web` are also visible with each log message. This is still unstructured logging though. Events are logged individually and events from different requests may be interleaved.

[^rust_log]: For example, you might set `RUST_LOG='debug,h2=warn` which will output debug level events but suppress anything below warn level from the `h2` crate which generates a lot of low-level messages which might drown out those from your application.
[^with_span_events]: You can change this behaviour by calling the [with_span_events](https://docs.rs/tracing-subscriber/latest/tracing_subscriber/fmt/struct.Layer.html#method.with_span_events) method when creating the subscriber.

## Adding OpenTelemetry to the mix

As a first step towards using OpenTelemetry, we can swap the `fmt` layer for an `opentelemetry` `stdout` "tracer" (update: this now also requires the `opentelemetry_stdout` crate). We then use `tracing_opentelemetry` to convert it to a layer compatible with our previous code:

```rust
use opentelemetry_sdk::trace::TracerProvider;
use opentelemetry::trace::TracerProvider as _;

let provider = TracerProvider::builder()
    .with_simple_exporter(opentelemetry_stdout::SpanExporter::default())
    .build();
let tracer = provider.tracer("randy");

let telemetry_layer  = tracing_opentelemetry::layer().with_tracer(tracer);

tracing_subscriber::registry()
    .with(tracing_subscriber::EnvFilter::from_default_env())
    //.with(fmt_layer)
    .with(telemetry_layer)
    .init();
```

This outputs OpenTelemetry data structures to the console, but the output is very dense and `fmt` is definitely preferable if you want human readable console logging.

## Exporting to Jaeger with OTLP

To use OTLP, we just replace the `stdout` tracer with one from `opentelemetry-otlp`:

```rust
...
let tracer = opentelemetry_otlp::new_pipeline()
    .tracing()
    .with_exporter(opentelemetry_otlp::new_exporter().tonic())
    .install_batch(opentelemetry_sdk::runtime::Tokio)
    .expect("Couldn't create OTLP tracer");

let telemetry_layer = tracing_opentelemetry::layer().with_tracer(tracer);

tracing_subscriber::registry()
    .with(tracing_subscriber::EnvFilter::from_default_env())
    .with(fmt_layer)
    .with(telemetry_layer)
    .init();
```

We've added `fmt_layer` back in here, so that we get console logging to compare with what we see in Jaeger. We're also using a batch exporter which exports data periodically and is recommended for performance. Apart from that everything is the same.

Now pull the docker image for Jaeger:

```
$ docker pull jaegertracing/all-in-one:latest
```

And run the Jaeger server:

```
$ docker run -d --name jaeger -e COLLECTOR_OTLP_ENABLED=true -p 16686:16686 -p 4317:4317 -p 4318:4318 jaegertracing/all-in-one:latest
```

You can then view the UI by browsing to `http://localhost:16686`. Ports 4317 and 4318 are used for OTLP over gRPC and HTTP respectively. The `tonic` exporter we've configured means we are using gRPC here.

We can use the environment variable `OTEL_SERVICE_NAME` to avoid hard-coding a name for our service:

```shell
OTEL_SERVICE_NAME=randy RUST_LOG="debug,h2=warn" cargo run
```

You will be able to find your requests in the UI. Note that unlike the `fmt` subscriber, our spans will appear as Jaeger traces even if we don't have debug logging enabled. Spans and events which occur during a request are shown as children of their parent span. For example, the request handler function `get_random` has the excellent [`#[instrument]`](https://docs.rs/tracing/0.1.37/tracing/attr.instrument.html) macro applied, which does most of the work for us and causes it to appear as a child span.

![/rand request trace in Jaeger](jaeger-ui-rand.webp)

There will probably be some other traces for `GET default` requests. This may be confusing, since we don't see anything about these in our console logging but they appear in the OpenTelemetry output. These are our browser's requests for `favicon.ico` which are returning a 404.

## Exporting to an online provider

In the previous example, the exporter uses a default endpoint which happens to be where our Jaeger server is listening (`http://127.0.0.1:4317`). The OpenTelemetry SDK in theory allows us to override the endpoint and other settings by using [predefined environment variables](https://opentelemetry.io/docs/concepts/sdk-configuration/otlp-exporter-configuration/).

Can we use the same code and configure it for any provider by just setting these variables?

### Honeycomb

I signed up for a vendor with a free (non-expiring) plan, Honeycomb. [Their documentation](https://docs.honeycomb.io/getting-data-in/opentelemetry-overview/#using-the-honeycomb-opentelemetry-endpoint) recommends the same OpenTelemetry crates we are already using and says we should set the variables:

```
OTEL_EXPORTER_OTLP_ENDPOINT=api.honeycomb.io:443
OTEL_EXPORTER_OTLP_HEADERS="x-honeycomb-team=your-api-key"
```

This endpoint will cause an "invalid URI" error unless we prefix it with "https://" so that's the first thing we need to change. Also, the variable `OTEL_EXPORTER_OTLP_HEADERS` isn't recognised by Rust opentelemetry, so we have to parse that ourselves for now [^extra-env-vars]. We also need to tell tonic that we want to use TLS [^tonic-tls]. The code for the exporter would look something like this (note that `parse_metadata_from_env` doesn't actually exist in the code):

[^extra-env-vars]: A fix for this [has been merged](https://github.com/open-telemetry/opentelemetry-rust/pull/1377) and is listed in the changelog for `opentelemetry-otlp` version `0.14.0`. However this seems to be a mistake as the change is still not in the tonic exporter in this version.
[^tonic-tls]: The `tls` and `tls-roots` features need to be enabled on the tonic crate in order to create a connection and validate the server certificate. There are also corresponding features on `opentelemetry-otlp` which have the same effect.

```rust
let exporter = opentelemetry_otlp::new_exporter()
    .tonic()
    .with_metadata(parse_metadata_from_env())
    .with_tls_config(Default::default());
```

For maximum flexibility we might also want to be able to use the `http/protobuf` protocol rather than being restricted to gRPC. So the final version of the example code reads the value of the [`OTEL_EXPORTER_OTLP_PROTOCOL`](https://opentelemetry.io/docs/reference/specification/protocol/exporter/#specify-protocol) environment variable to decide what to use (it defaults to "grpc" if it's not set).
It also checks `OTEL_EXPORTER_OTLP_ENDPOINT` to see whether it starts with `https`, before deciding whether to enable TLS for the grpc/tonic exporter. That way we can still use it locally with Jaeger, for example [^http-exporter].

[^http-exporter]: Note that you might run into [this issue](https://github.com/open-telemetry/opentelemetry-rust/issues/997) when using the HTTP exporter. In the meantime you have to append `/v1/traces` to the URL for both Jaeger and Honeycomb.

If we run the app with:

```
OTEL_EXPORTER_OTLP_ENDPOINT=https://api.honeycomb.io:443 OTEL_EXPORTER_OTLP_HEADERS="x-honeycomb-team=your-api-key" OTEL_EXPORTER_OTLP_PROTOCOL=grpc OTEL_SERVICE_NAME=randy RUST_LOG="debug,h2=warn" cargo run
```

we can finally see our request traces in Honeycomb's dashboard and view individual spans:

![/rand request trace in Honeycomb](honeycomb-rand.webp)

It works!

### Aspecto

Just to check, I signed up for another free provider, Aspecto, and changed the endpoint and headers:

```
OTEL_EXPORTER_OTLP_ENDPOINT=https://otelcol.aspecto.io:4317 OTEL_EXPORTER_OTLP_HEADERS="Authorization=your-api-key" OTEL_SERVICE_NAME=randy RUST_LOG="debug,h2=warn" cargo run
```

This worked immediately! [^lie]:

[^lie]: This is a lie, I used the wrong port number to start with 🙂.

![/rand request trace in Aspecto](aspecto-rand.webp)

## Conclusion

There's a lot more to tracing and telemetry than what we've covered here, but we've successfully created a setup that should work with different OpenTelemetry providers, without the need to recompile our app. We can easily configure it using standard environment variables (which will hopefully be supported directly by the opentelemetry libraries some day). This gives us local event logging along with exporting of tracing spans to an OpenTelemetry traces endpoint.

One issue I had along the way is that it's not obvious what's going on between the opentelemetry client and the endpoint (who traces the tracers 🙂?). When using `http/proto` I only worked out that I was missing the `/v1/traces` part of the endpoint URI when I swapped the default `reqwest` client with one which dumped the request and response to the console. Then I realised the endpoint was returing a `404` which was being ignored. I haven't worked out if this is possible with gRPC and the `tonic` version, since I didn't have any similar issues (Update: the issue with HTTP errors being silently ignored should now be fixed [^http-fix]).

[^http-fix]: The `0.19` release of `opentelemetry-otlp` now [reports HTTP errors](https://github.com/open-telemetry/opentelemetry-rust/pull/945).

Overall though I'm very pleased with the results and the ecosystem is a pleasure to use, thanks to the many people who have worked hard on the various projects involved to get things to where they are today. Instrumenting your code with Rust tracing is a breeze (particularly using the excellent `instrument` macro) and exporting to OpenTelemetry seems to be the way to go if you want to export your data to the cloud. Both the providers used above are easy to sign up for with no commitment, and their free plans should be adequate for a small app.
