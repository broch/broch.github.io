---
title: OAuth2 and OpenID Connect in Haskell
author: Luke
date: 2015-03-10
tags: haskell,identity,oauth2,openid-connect
---

I've been working for a whil on an implementation of the OpenID Connect specification. Since it was something I already knew quite a bit about from my previous job, it seemed like a good idea for a "real-world" Haskell project. The result is a project called "Broch" [^broch-origin]. Features which are implemented to various degrees include

[^broch-origin]: This follows from the contrived acronym "Basic Realization of OpenID Connect in Haskell", but I chose the name first and the acronym later. If someone can think of a better one, please let me know. Brochs are tall, round iron age buildings and I enjoyed playing in the ruins of some of them when I was young. They are of simple design, solidly engineered and secure. All good goals for an identity management system to aspire to, even an implementation of OAuth2/OpenID Connect.

* OAuth2 flows
  * Authorization endpoint
  * Token endpoint
* OpenID Connect basic flows
* OpenID Connect hybrid flows
* OpenID Connect Discovery
* Support for signed and encrypted JWTs [^jose-jwt]
* [Client Registration][client-reg]
* Client authentication
  * Basic authentication with client secret
  * [JWT Bearer authentication][jwt-bearer]
* ID Tokens (signed and/or encrypted)
* [Pairwise subject identifiers][sub-id-types]
* Server key management and rotation [^key-rotation]
* PostgreSQL 9.5 back end

[^jose-jwt]: JWTs are implemented in a separate project [`jose-jwt`](http://hackage.haskell.org/package/jose-jwt).
[^key-rotation]: Key rotation is described in the [`openid-connect-core`](http://openid.net/specs/openid-connect-core-1_0.html#RotateSigKeys) spec.

[client-reg]: http://openid.net/specs/openid-connect-registration-1_0.html
[jwt-bearer]: https://tools.ietf.org/html/draft-ietf-oauth-jwt-bearer-12
[sub-id-types]: http://openid.net/specs/openid-connect-core-1_0.html#SubjectIDTypes

You can easily get a prototype server up and running with default settings and it's intended that the important features should be easily configurable. This article is more about the Haskell implementation than the specification, so doesn't explain OAuth2 and OpenID Connect concepts in any detail.

The Command Line Executable
===========================

The project build creates an executable which can be used to get up and running quickly. Instructions for building and running against a PostgreSQL database can be found in the project [Readme file][broch-readme].

[broch-readme]: https://github.com/tekul/broch/blob/master/README.md

Coding a Minimal Server
=======================

In many cases, you will want to write a customized server of your own. To do this, you create a configuration instance (`Broch.Server.Config`) and pass it to the `brochServer` function. The only things you *have* to supply are

* An "issuer" for the OpenID Provider. This is the external URL used to access your server, for example `https://myopenidserver.com`.
* A `Broch.Server.Config.KeyRing` instance to provide the signing and encryption keys for the server. The `defaultKeyRing` function can be used for this.
* A function to authenticate (or reauthenticate) a user.
* A function to provide the identity of the currently authenticated user.
* A means of supplying user information for OpenID authentication requests.

Some standard options for authentication and user management are provided -- you just need to select them in your configuration. Everything else in a simple test server can use default settings, in-memory storage and provided login handlers.

The configured server is built on WAI and can be run using the warp web server:

> import Data.Default.Generics (def)
> import qualified Data.Text as T
> import Network.Wai.Middleware.RequestLogger (logStdoutDev)
> import Network.Wai.Handler.Warp
>
> import Broch.Model (Client(..), GrantType(..), Scope(..), UserInfo(..))
> import Broch.Server.Config
> import Broch.Server (brochServer, authenticatedSubject, authenticateSubject, defaultLoginPage, defaultApprovalPage, passwordLoginHandler)
> import Broch.Server.Internal (routerToApp, text, invalidateSession, complete)
> import Broch.Server.Session (defaultKey, defaultLoadSession)
> import Web.Routing.TextRouting
>
> main :: IO ()
> main = do
>     -- The session key
>     csKey  <- defaultKey
>     opKeys <- defaultKeyRing
>     inMemory <- inMemoryConfig "http://localhost:3000" opKeys Nothing
>     let config = inMemory { authenticateResourceOwner = authenticate, getUserInfo = loadUserInfo }
>     createClient config testClient
>     let extraRoutes =
>             [ ("/home",   text "Hello, I'm the home page")
>             , ("/login",  passwordLoginHandler defaultLoginPage authenticate)
>             , ("/logout", invalidateSession >> complete)
>             ]
>         routingTable = foldl (\tree (r, h) -> addToRoutingTree r h tree) (brochServer config defaultApprovalPage authenticatedSubject authenticateSubject) extraRoutes
>         waiApp = routerToApp (defaultLoadSession 3600 csKey) (issuerUrl config) routingTable
>     run 3000 $ logStdoutDev waiApp
>   where
>     testClient = def
>         { clientId = "123"
>         , clientSecret = Just "abc123"
>         , authorizedGrantTypes = [AuthorizationCode]
>         , redirectURIs = ["http://c123.client"]
>         , allowedScope = [OpenID]
>         }
>     authenticate username password
>         | username == password = return (Just username)
>         | otherwise            = return Nothing
>
>     loadUserInfo uid _ = return . Just $def
>         { sub = uid
>         , email = Just (T.concat [uid, "@someplace.com"])
>         }

The web code is similar to that in [an earlier article on WAI](http://broch.io/posts/build-your-own-wai-framework/), but includes the concept of a session, since users have to be able to authenticate to the authorization server [^default-session]. The `brochServer` function converts the configuration into a routing table of handler functions, and we add some extra handlers for authentication and a very basic home page. The routing table is then converted into a WAI `Application`.

We've added a single client which is allowed to use the authorization code flow and will use basic authentication (the default) at the token endpoint.

Neither OAuth2 nor OpenID Connect define how authentication of the end user should take place at the authorization server, so user account data and authentication are decoupled from the core OpenID/OAuth2 functionality. Here we have used an authentication function which merely compares the username and password for equality, so there aren't actually any user accounts -- you can authenticate with any name. For "user info" requests, we've just added a function `loadUserInfo` to make up the data. In a real implementation, you would have a specific user data type and would write functions to manage user accounts and convert the data to the claims returned for a user info request [^user-claims]. A side project is an implementation based on the SCIM specification [^scim].

[^default-session]: Here we are using the default session implementation which is based on the [`clientsession`](http://hackage.haskell.org/package/clientsession) package.
[^scim]: The aim is to build a full implementation of [the SCIM 2 spec](http://www.simplecloud.info/), but this is a work in progress, and SCIM may be overkill for many use cases.
[^user-claims]: OpenID Connect defines a specific set of [claims](http://openid.net/specs/openid-connect-core-1_0.html#Claims), which unfortunately aren't directly compatible with SCIM.

If you want to use a database, there's a [`Persistent`](http://www.yesodweb.com/book/persistent) backend provided out of the box, which uses the Scim module. We could swap from in-memory to Persistent storage just by using

``` haskell
persistConfig <- persistBackend pool <$> inMemoryConfig issuer
```
where `pool` is a Persistent `ConnectionPool` instance.

Authorization Code Flow Walk-Through
====================================

Using the server above, we can go through a typical flow to which the client application would use to authenticate a user. We'll use `curl` to take the place of the client. All URLs would use HTTPS in a production system.

The first step is a redirection from the client to the authorization server, which creates the [following request](http://localhost:3000/oauth/authorize?client_id=123&state=982147&response_type=code&redirect_uri=http%3A%2F%2Fc123.client) [^auth-code-request]:

[^auth-code-request]: See the [spec](http://openid.net/specs/openid-connect-core-1_0.html#CodeFlowSteps) for a more in-depth description. The only thing that will usually vary in this request is the `state` parameter which is generated by the client.

```
http://localhost:3000/oauth/authorize?client_id=123&state=982147&response_type=code&scope=openid&redirect_uri=http%3A%2F%2Fc123.client
```

The user will be asked to log in (if they aren't already authenticated to the authorization server), and then to approve the request for `openid` scope. Once this is granted, the authorization server redirects back to the client application, with an authorization code:

```
http://c123.client/?state=982147&code=14581d956c81535c&scope=openid
```

The client then exchanges the code at the token endpoint for an `access_token` and an `id_token`, which are returned in a JSON response:

```
$ curl -u 123:abc123 -H "Accept: application/json" -X POST -d code=14581d956c81535c -d client_id=123 -d redirect_uri=http://c123.client -d grant_type=authorization_code http://localhost:3000/oauth/token
{"expires_in":3600,"access_token":"eyJhbGciOiJSU0EtT0FFUCIsImtpZCI6IjIwMTUtMDUtMTlUMjA6MjI6MjUuMTc2MjY1MDAwMDAwWiIsImVuYyI6IkExMjhHQ00ifQ.bESkEA-0vGBhnftPuRLYcxvZuD6xbdTrp4h34zBxsn0AhNgXxAOsMsvC-14YijuMBAU4SxkMsBoxL4P4vEWODGrVwK8xb0_OogyxsrCSRYiYwYopU3xli9k3Dw_LpP0vFC60r1oGGsGexeKsAYy9BwL5kGeTNt9GtnjI2Q-WnrA.oZvgWxUtv4-RNddd.xcaT8kydCGN4Oe_JH5QvFTxsE9YJMJ976b1PEAkHvjHj2xcEM1pE_3MCsEGOV7tSho6omNCJFZC_AiKfP2s4QBLvXxG9kMON7OIIjrx4FKDuTAoZgtl-4aiQ_mt-ppt2lVf0pr03cYTvoBzJK85ofMnNeLsnrjA3oGB-xGxXSG5ZKkyutNo.X4ncv5rOTTBOE6hdclpWYg","token_type":"bearer","id_token":"eyJhbGciOiJSUzI1NiIsImtpZCI6IjIwMTUtMDUtMTlUMjA6MjI6MjQuMTc2MjY1MDAwMDAwWiJ9.eyJzdWIiOiJjYXQiLCJleHAiOjE0MzIxMzkxMDgsImlzcyI6Imh0dHA6Ly9sb2NhbGhvc3Q6MzAwMCIsImlhdCI6MTQzMjEzODEwOCwiYXV0aF90aW1lIjoxNDMyMTM3MTY5LCJhdWQiOlsiMTIzIl19.f_EJI-wiDT1oa0Cta12yco73BurkYTCR-yrxl3k5zsYO7wNrHc9y2QE-ahmkdsiHdlzCZ4roF7_fVXRMHL2JNsC3S6oyeWfO6E-8sjsTFBRvkDSOCbYwm7HnYW-VWZ1e2M8g_RgZb4SVzW4OK55QntRvlwW6Aj6Tu_AN6Dg7Ua4"}
```

The `id_token` is defined by the spec to be a JWT. In this implementation, access tokens are also JWTs by default.

The client can then use the access token to request more information about the user:

```
$ TOKEN=eyJhbGciOiJSU0EtT0FFUCIsImtpZCI6IjIwMTUtMDUtMTlUMjA6MjI6MjUuMTc2MjY1MDAwMDAwWiIsImVuYyI6IkExMjhHQ00ifQ.bESkEA-0vGBhnftPuRLYcxvZuD6xbdTrp4h34zBxsn0AhNgXxAOsMsvC-14YijuMBAU4SxkMsBoxL4P4vEWODGrVwK8xb0_OogyxsrCSRYiYwYopU3xli9k3Dw_LpP0vFC60r1oGGsGexeKsAYy9BwL5kGeTNt9GtnjI2Q-WnrA.oZvgWxUtv4-RNddd.xcaT8kydCGN4Oe_JH5QvFTxsE9YJMJ976b1PEAkHvjHj2xcEM1pE_3MCsEGOV7tSho6omNCJFZC_AiKfP2s4QBLvXxG9kMON7OIIjrx4FKDuTAoZgtl-4aiQ_mt-ppt2lVf0pr03cYTvoBzJK85ofMnNeLsnrjA3oGB-xGxXSG5ZKkyutNo.X4ncv5rOTTBOE6hdclpWYg
$ curl -H "Accept: application/json" -H "Authorization: Bearer $TOKEN" http://localhost:3000/connect/userinfo
{"email":"cat@someplace.com","sub":"cat"}
```

Configuration
=============

One of the design issues I had trouble with was the how best to build a configurable server application. It's easy enough to come up with intelligent defaults for an OpenID Provider, but pretty much all the functionality needs to be pluggable [^haskell-di]. The `Broch.Server.Config` module contains data structures for settings which are used to initialize a server, and also functions which define pluggable behaviour

[^haskell-di]: Functional programmers enjoy making fun of other languages and their inadequacies in this department (just mention "dependency injection" and watch the reaction), but concrete examples of how to build a Haskell server with a pluggable configuration are thin on the ground. For a beginner it's not clear where to start. If you're using a framework like Yesod, then it uses typeclasses to implement different functionality you might want in your application, and you can override specific functions if you wish. However, I'd already decided I didn't want to tie the project to any specific framework and I wasn't overly keen on using typeclasses for everything. The approach I settled on was inspired by this [Stack Overflow answer](http://stackoverflow.com/a/14329487/241990).

``` haskell
data Config m s = Config
    { issuerUrl                  :: Text
    , keyRing                    :: KeyRing m
    , responseTypesSupported     :: [ResponseType]
    , algorithmsSupported        :: SupportedAlgorithms
    , clientAuthMethodsSupported :: [ClientAuthMethod]
    , claimsSupported            :: [Text]
    , createClient               :: CreateClient m
    , getClient                  :: LoadClient m
    , createAuthorization        :: CreateAuthorization m s
    , getAuthorization           :: LoadAuthorization m
    , authenticateResourceOwner  :: AuthenticateResourceOwner m
    , createApproval             :: CreateApproval m
    , getApproval                :: LoadApproval m
    , createAccessToken          :: CreateAccessToken m
    , decodeAccessToken          :: DecodeAccessToken m
    , decodeRefreshToken         :: DecodeRefreshToken m
    , getUserInfo                :: LoadUserInfo m
    }
```

The functions are type aliases, for example
``` haskell

type LoadClient m = ClientId -> m (Maybe Client)

```

The implementations can then be written in any way, as long as they end up satisfying the required type. They can use partial application, for example, to pass other dependencies such as connection pools.

Request Processing
==================

The majority of the functionality for processing authorization and token requests is decoupled from the HTTP interface (WAI). The web handlers extract the request data, bundle the parameters up in a map, then delegate the detailed work to other functions. This makes it easier to test the core functionality and to use it with a different web front-end.

Authorization Endpoint
----------------------

The result of an authorization request can be one of

* A redirect containing the authorization information (an authorization code, access token or whatever other data is required by the grant request)
* A redirect to enable user authentication, before continuing processing the original authorization request
* An error returned to the user agent, due to a potentially malicious client request
* A redirect error, where the error information is returned to the client in the URL.

The authorization web handler authenticates the user and then delegates to the function [`processAuthorizationRequest`](TODO).

Token Endpoint
--------------

The token endpoint authenticates the client and then calls the function [`processTokenRequest`](TODO), return a JSON response as defined in the specification. This can be either a token response or an error response.

Dynamic Registration
--------------------

The server can optionally support [client registration](http://openid.net/specs/openid-connect-registration-1_0.html).


Discovery
---------

The discovery endpoint just provides a well-known location for clients to obtain a copy of the server's configuration and supported features, such as the algorithms which can be used for encoding JWTs.

UserInfo
--------

A client can optionally retrieve user details from the "user info" endpoint, by submitting the access token which was issued by the authorization server. This isn't strictly necessary, as OpenID Connect also isssues an ID token which can represent the authenticated user.

Other Topics
============

UI
--

The UI requirements are quite minimal and will usually consist of

* A login page of some kind, unless the authorization server uses some authentication mechanism which doesn't require one.
* A page to obtain the user's approval for the information requested by the client.

Default implementations are provided for both of these. The login page is used with the `passwordLoginHandler` and is a plain Blaze `Html` page. User approval is a function which takes the approval data and returns an `Html` page.

The

Client Authentication
---------------------

OAuth2 only mentions client authentication using a password/secret, either using a Basic authorization header or passing the credentials in the request body. Providers are also free to accept other forms of authentication.

OpenID Connect explicitly defines the [client authentication methods][client-auth-methods] which it supports. All of these are available in this implementation.

[client-auth-methods]: http://openid.net/specs/openid-connect-core-1_0.html#ClientAuthentication

Key Rotation
------------

The server's public keys can be obtained from the `jwks_uri` endpoint (which is returned in the server discovery data). It contains all the public keys which a client might need to validate server signatures and to encrypt data to send to the server.

The rotation of [signing](http://openid.net/specs/openid-connect-core-1_0.html#RotateSigKeys) and [encryption](http://openid.net/specs/openid-connect-core-1_0.html#RotateEncKeys) keys is also covered in the spec. The `KeyRing` data type stores two active key pairs -- one for signing and one for encryption. It also has a function to rotate the keys, expiring the previous active keys and generating news ones. Unless the keys need to be invalidated immediately (for security reasons, for example), the expired signing keys will still be available from the `jwks_uri` endpoint for a configurable grace period, so that tokens created with earlier signing keys will still validate. The server will also retain decryption private keys internally for the grace period.

Customization
=============

* Enabling/Disabling features (discovery, registration)
* Different ID Token contents
* Adding a standard OAuth2 protected endpoint

Conclusion
==========

WIP.
