---
title: OAuth2 and OpenID Connect in Haskell
author: Luke
date: 2015-03-10
tags: haskell,identity,oauth2,openid-connect
---

I've been working on an implementation of the OpenID Connect specification. Since it was something I already knew quite a bit about from my previous job, it seemed like a good idea for a "real-world" Haskell project. The result is a project called "Broch" [^broch-origin]. Features which are implemented to various degrees include

* OAuth2 flows
  * Authorization endpoint
  * Token endpoint
* OpenID Connect hybrid flows
* OpenID Connect Discovery
* Support for signed and encrypted JWTs [^jose-jwt]
* [Client Registration][client-reg]
* Client authentication
  * Basic authentication with client secret
  * [JWT Bearer authentication][jwt-bearer]
* ID Tokens (signed and/or encrypted)


[client-reg]: http://openid.net/specs/openid-connect-registration-1_0.html
[jwt-bearer]: https://tools.ietf.org/html/draft-ietf-oauth-jwt-bearer-12

It's still far from being a production-ready OpenID Connect Provider, but you can easily get a prototype server up and running and the plan is that everything should be easily configurable. This article is more about the Haskell implementation than the specification, so doesn't explain OAuth2 and OpenID Connect concepts in any detail.

# A Minimal Server

To run a server, you need to create a configuration instance (`Broch.Server.Config`) and pass it to the `brochServer` function. The only thing you *have to* supply is an "issuer" for the OpenID Provider. This is the external URL which your server will be visible under, for example "https://myopenidserver.com". Everything else in a simple test server can use default settings and in-memory storage.

``` haskell

TODO

```

If you want to use a database, there's a `Persist` backend provided out of the box

``` haskell

TODO

```



# Configuration

One of the things I struggled with initally was the best approach for building a configurable server application. It's easy enough to come up with intelligent defaults for an OpenID Provider, but pretty much all the functionality needs to be pluggable [^haskell-di]. The `Broch.Server.Config` module contains data structures for settings which are used to initialize a server, and also functions which define pluggable behaviour. For example

``` haskell

TODO: Insert function types

```

# Request Processing

The majority of the functionality for processing authorization and token requests is decoupled from the HTTP interface (WAI). The web handlers extract the request data, bundle the parameters up in a map, then delegate the work to other functions.

## Authorization Endpoint

The result of an authorization request can be one of

TODO

## Token Endpoint


## Dynamic Registration


## Discovery


## UserInfo



# Other Topics

## UI

Blaze

## Client Authentication

## Key Rotation

## Customization

* Enabling/Disabling features
* Different ID Token contents
* Adding a standard OAuth2 protected endpoint


[^haskell-di]: Functional programmers enjoy making fun of other languages and their inadequacies in this department. Just mention "dependency injection" and watch the reaction, but concrete examples of how to build a Haskell server with a pluggable configuration are rather thin on the ground, so for a beginner it's not clear where to start. If you're using a framework like Yesod, then it uses typeclasses to implement different functionality you might want in your application, and you can override specific functions if you wish. However, I'd already decided I didn't want to tie the project to any specific framework. The configuration approach I finally settled on was inspired by this [Stack Overflow answer](http://stackoverflow.com/a/14329487/241990).

[^broch-origin]: This follows from the contrived acronym "Basic Realization of OpenID Connect in Haskell", but I chose the name first and the acronym later. If someone can think of a better one, please let me know. Brochs are tall, round iron age buildings and I enjoyed playing in the ruins of some of them when I was young. They are of simple design, solidly engineered and secure. All good goals for an identity management system to aspire to, even an implementation of OAuth2/OpenID Connect.

[^jose-jwt]: JWTs are implemented in a separate project [`jose-jwt`][http://hackage.haskell.org/package/jose-jwt].
