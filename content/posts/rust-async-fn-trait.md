---
title: Trying out Rust's Async Functions in Traits
author: Luke
date: 2023-08-31
tags: [rust, async]
thumb: /images/rust-logo-280x160.webp
thumb_alt: The Rust 'R in a sprocket' logo
description: Exploring the latest Rust nightly features for using async functions in traits
---

Async functions in traits have been available in Rust nightly releases for some time now behind the feature gate `async_fn_in_trait`. The current status is summarized in the [Inside Rust blog](https://blog.rust-lang.org/inside-rust/2023/05/03/stabilizing-async-fn-in-trait.html). Many of the issues are also explained in more detail by Niko Matsakis in his [Baby Steps blog](https://smallcultfollowing.com/babysteps/).

I decided to try the feature out with some existing code which was using the [`async-trait`](https://crates.io/crates/async-trait) library (which provides a workaround for stable Rust).

Working code for the example in this article can be found [on Github](https://github.com/tekul/rust-async-fn-trait).

## Existing code using `async-trait` and actix

The code I have is quite complicated but the idea can be reduced to a `Database` trait which is used by an actix web handler. There's also an axum version of the application but we'll get to that later. A toy example which models this could be written as follows:

```rust
use async_trait::async_trait;

pub struct Data {
    id: String,
}

#[async_trait]
pub trait Database {
    async fn load_data(&self, id: &str) -> Data;
}

struct SillyDatabase {};

#[async_trait]
impl Database for SillyDatabase {
    async fn load_data(&self, &str id) -> Data {
        Data { id: id.to_string() }
    }
}

```

The actix application has a function to setup the routes which is generic in the `Database` type, allowing it to be run with different backend implementations:

```rust
use actix_web::{
    web::{self, ServiceConfig},
    App, HttpRequest, HttpResponse, HttpServer,
};

pub fn mk_app<B>(backend: B) -> impl FnOnce(&mut ServiceConfig)
where
    B: Database + 'static,
{
    move |app| {
        app.app_data(backend).service(web::resource("/data").to(get_data::<B>));
    }
}

async fn get_data<B>(req: HttpRequest) -> HttpResponse
where
    B: Database + 'static
{
    let backend = req
        .app_data::<B>()
        .expect("app_data should include Database");
    let Data { id } = backend.load_data("some_id").await;
    HttpResponse::Ok().body(format!("Loaded data, with id {id}"))
}

#[tokio::main]
async fn main() -> io::Result<()> {
    let database = SillyDatabase {};
    let server = HttpServer::new(move || {
        App::new().configure(mk_app(database.clone()))
    })
    .bind("127.0.0.1:8088")
    .unwrap();
    server.run().await
}

```

Running the app with `cargo run` and then sending a GET request to the URL works as expected:

```shell
$ curl localhost:8088/data
Loaded data, with id 'some_id'
```

### Without `async-trait`

Ideally we can just switch to nightly rust, enable the feature, remove the `async_trait` macros and it will just work:

```rust
#![feature(async_fn_in_trait)]

trait Database {
    async fn load_data(&self, &str id) -> Data;
}

impl Database for SillyDatabase {
   // Same as before...
}
```

And indeed it does! We can just run the app as before and we get the same result. Well that was easy. Looks like we can just go ahead and forget about the `async-trait` crate already?

## Adding an axum version

Not so fast. Let's try doing the same thing with axum. The equivalent code for the server is

```rust
use std::net::SocketAddr;

use async_trait::async_trait;
use axum::{extract::State, response::IntoResponse, routing::get, Router};

pub fn mk_app<B>(backend: B) -> Router
where
    B: Clone + Send + Sync + Database + 'static,
{
    Router::new()
        .route("/data", get(get_data::<B>))
        .with_state(backend)
}

async fn get_data<B>(State(backend): State<B>) -> impl IntoResponse
where
    B: Database,
{
    let Data { id } = backend.load_data("some_id").await;
    format!("Loaded data, with id {id}")
}

#[tokio::main]
async fn main() {
    let backend = SillyDatabase {};
    let addr = SocketAddr::from(([0, 0, 0, 0, 0, 0, 0, 0], 8088));
    axum_server::bind(addr)
        .serve(mk_app(backend).into_make_service())
        .await
        .expect("server error");
}

```

This also works with the `async-trait` version of our `Database` trait. But what happens if we switch to using `async_fn_in_trait` again:

```
error[E0277]: the trait bound `fn(State<B>) -> impl Future<Output = impl IntoResponse> {get_data::<B>}: Handler<_, _, _>` is not satisfied
   --> src/bin/axum.rs:28:29
    |
28  |         .route("/data", get(get_data::<B>))
    |                         --- ^^^^^^^^^^^^^ the trait `Handler<_, _, _>` is not implemented for fn item `fn(State<B>) -> impl Future<Output = impl IntoResponse> {get_data::<B>}`
    |                         |
    |                         required by a bound introduced by this call
    |
    = note: Consider using `#[axum::debug_handler]` to improve the error message
    = help: the following other types implement trait `Handler<T, S, B>`:
              <Layered<L, H, T, S, B, B2> as Handler<T, S, B2>>
              <MethodRouter<S, B> as Handler<(), S, B>>
note: required by a bound in `axum::routing::get`
```
Oops, it doesn't work! This opaque error message is common with axum and means that our function needs to implement `Handler` but doesn't. Axum defines Handler implementations for lots of things and we don't usually have to worry about it, but for some reason our function no longer satisfies the requirements even though it did before and we haven't changed the function directly. If we try to follow the advice to use the `axum::debug_handler` macro we will get an additional error message:

```
error: `#[axum_macros::debug_handler]` doesn't support generic functions
  --> src/bin/axum.rs:33:18
   |
33 | async fn get_data<B>(State(backend): State<B>) -> impl IntoResponse
   |                  ^^^
```

So that is no help either. If we remove the generic handler and just use `SillyDatabase` directly, the problem goes away. But that's not what we want. The real code is written to be generic because the router it creates is part of a library and using a trait means users can configure it with whatever backend they want. So why isn't it working any more?

## The `Send` bound problem

Fortunately I'd been reading the blogs I mentioned at the start of this post and the related issues in github and there's a lot of discussion about how to make the futures returned by the `async` functions in traits implement `Send` [^matsakis-01-02-23]. This is a common requirement when using Tokio's multi-threaded runtime which can move tasks about between threads, and this is why it is also a requirement for Axum. In fact, if we look at the section called [Debugging handler type errors](https://docs.rs/axum/0.6.20/axum/handler/index.html#debugging-handler-type-errors) the last point is that a handler function must:

> Return a future that is Send. The most common way to accidentally make a future !Send is to hold a !Send type across an await.

When we used `async-trait`, it automatically adds the `Send` bound to the function in both the trait and the implementation by default. So the `Database` trait's `load_data` function is guaranteed to return a future that is `Send`. We can check this by changing the `async_trait macro` usage to `#[async_trait(?Send)]` which no longer adds the `Send` bound. This gives us the same `Handler` error that we see above.

When we switch to using `async_fn_in_trait` it is no longer the default to assume that the returned future must be `Send`. In our code the `get_data` handler is awaiting a future returned by the `load_data` method and thus we get the error since the future is not guaranteed to be `Send`.

[^matsakis-01-02-23]: A good example is the article [Async trait send bounds, part 1](https://smallcultfollowing.com/babysteps/blog/2023/02/01/async-trait-send-bounds-part-1-intro/) by Niko Matsakis.

### Using the `return_type_notation` feature

So is there a way for our `get_data` handler to say that it only supports `Database` implementations which return a `Send` future? It turns out there is if we use the bleeding edge `return_type_notation` feature [^matsakis-13-02-23]. If we add the feature then change our `mk_app` function to use it:

```rust
#![feature(async_fn_in_trait, return_type_notation)]
...

pub fn mk_app<B>(backend: B) -> Router
where
    B: Clone + Send + Sync + Database<load_data(): Send> + 'static,
{
    ...
}
```

this should fix the problem. Unfortunately we then get another error (ignoring a warning about using incomplete features):

```
error[E0277]: `impl Future<Output = Data>` cannot be sent between threads safely
  --> src/bin/axum.rs:46:23
   |
46 |         .serve(mk_app(backend).into_make_service())
   |                ------ ^^^^^^^ `impl Future<Output = Data>` cannot be sent between threads safely
   |                |
   |                required by a bound introduced by this call
```

This is still complaining that our future is `!Send` even though the compiler knows that `backend` is a `SillyDatabase` at this point, which should be fine. Fortunately someone else had already run into the same problem [^rust-114142]. If we use "turbofish" syntax to explicitly tell `mk_app` what type we're using:

```rust
    axum_server::bind(addr)
        .serve(mk_app::<SillyDatabase>(backend).into_make_service())
        .await
        .expect("server error");

```

Then our code finally compiles.

[^rust-114142]: Fortunate for me at any rate. This [Github issue](https://github.com/rust-lang/rust/issues/114142) describes the same problem and someone helpfully provided a workaround.

[^matsakis-13-02-23]: This is also covered in [Return type notation (send bounds, part 2)](https://smallcultfollowing.com/babysteps/blog/2023/02/13/return-type-notation-send-bounds-part-2/). Note that using it currently breaks things like Rust `tracing` macros.

### What about Actix?

Actix works without any issues though which is a bit confusing since it uses the same `#[tokio::main]` macro. In the past Actix had its own runtime implementation, which used multiple single-threaded Tokio runtimes and did not need futures to be `Send`. Under the hood, they have retained that approach in their current architecture, so this explains why we don't have the kind of problems we get with Axum, which uses Tokio in a more standard way [^actix-4-release].

[^actix-4-release]: See the discussion in this [actix web 4 release](https://www.reddit.com/r/rust/comments/t1bim5/announcing_actix_web_v40/) announcement, for example (or read the code 🙂).

## Conclusion

What at first seems to be a simple enough idea, can turn out to be, well, not so simple. This has been obvious to the people implementing async Rust for a while but it's less obvious if you're just writing code like me.

So far, I haven't had a use case where I don't want to require the `Send` bound at the trait level, but may want to require it for a specific implementation (which is what the `runtime_type_notation` achieves). It makes sense that there might be cases where this is desirable in a general purpose library.

Hopefully in a future Rust version it will be just as easy to turn the `Send` bound on or off for our trait methods as it is with `async-trait`. For now though, sticking with the `async-trait` macros is still the simplest option.
