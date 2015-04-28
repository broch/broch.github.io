---
title: OAuth2 and OpenID Connect in Haskell
author: Luke
date: 2015-03-10
tags: haskell,identity,oauth2,openid-connect
---

I've been working on an implementation of the OpenID Connect specification. Since it was something I already knew quite a bit about from my previous job, it seemed like a good idea for a "real-world" Haskell project. The result is a project called "Broch" [^broch-origin]. Features which are implemented to various degrees include

[^broch-origin]: This follows from the contrived acronym "Basic Realization of OpenID Connect in Haskell", but I chose the name first and the acronym later. If someone can think of a better one, please let me know. Brochs are tall, round iron age buildings and I enjoyed playing in the ruins of some of them when I was young. They are of simple design, solidly engineered and secure. All good goals for an identity management system to aspire to, even an implementation of OAuth2/OpenID Connect.

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

[^jose-jwt]: JWTs are implemented in a separate project [`jose-jwt`](http://hackage.haskell.org/package/jose-jwt).

[client-reg]: http://openid.net/specs/openid-connect-registration-1_0.html
[jwt-bearer]: https://tools.ietf.org/html/draft-ietf-oauth-jwt-bearer-12

It's still far from being a production-ready OpenID Connect Provider, but you can easily get a prototype server up and running with default settings and the plan is that the important things should be easily configurable. This article is more about the Haskell implementation than the specification, so doesn't explain OAuth2 and OpenID Connect concepts in any detail.

# A Minimal Server

To run a server, you need to create a configuration instance (`Broch.Server.Config`) and pass it to the `brochServer` function. The only things you *have to* supply are

* An "issuer" for the OpenID Provider. This is the external URL used to access your server, for example `https://myopenidserver.com`.
* A user authentication function.
* A means of supplying user account information for OpenID authentication requests.

Some standard options for authentication and user management are provided -- you just need to select them in your configuration. Everything else in a simple test server can use default settings, in-memory storage and provided login handlers.

The configured server is built on WAI and can be run using the warp web server:

``` haskell

import Data.Default.Generics (def)
import qualified Data.Text as T
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Handler.Warp
import Web.ClientSession (getDefaultKey)

import Broch.Model (Client(..), GrantType(..), Scope(..))
import Broch.Scim
import Broch.Server.Config
import Broch.Server (brochServer, authenticatedSubject, passwordLoginHandler)
import Broch.Server.Internal (routerToApp)
import Broch.Server.Session (defaultLoadSession)

main :: IO ()
main = do
    csKey  <- getDefaultKey
    inMemory <- inMemoryConfig "http://localhost:3000"
    let config = inMemory { authenticateResourceOwner = authenticate, getUserInfo = loadUserInfo }
    router <- brochServer config (authenticatedSubject, passwordLoginHandler (authenticateResourceOwner config))
    let waiApp = routerToApp (defaultLoadSession 3600 csKey) (issuerUrl config) router
    run 3000 $ logStdoutDev waiApp
  where
    testClient = def
        { clientId = "123"
        , clientSecret = Just "abc123"
        , authorizedGrantTypes = [AuthorizationCode]
        , redirectURIs = ["http://c123.client"]
        , allowedScope = [OpenID]
        }
    authenticate username password
        | username == password = return (Just username)
        | otherwise            = return Nothing

    loadUserInfo uid _ = return $ def
        { sub = uid
        , email = Just (T.concat [uid, "@someplace.com"])
        }
```

The web code is similar to that in [an earlier article on WAI](http://broch.io/posts/build-your-own-wai-framework/), but also includes the concept of a session, since users have to be able to authenticate to the authorization server [^default-session]. The `brochServer` function converts the configuration into a routing table of handler functions. This is then converted into a WAI `Application`.

We've added a single client which is allowed to use the authorization code flow and will use basic authentication at the token endpoint.

Neither OAuth2 nor OpenID Connect define how authentication of the end user should take place at the authorization server, so user account data and authentication are decoupled from the core OpenID/OAuth2 functionality. Here we have used an authentication function which merely compares the username and password for equality, so there aren't actually any user accounts -- you can authenticate with any name. For "user info" requests, we've just added a function `loadUserInfo` to make up the data. In a real implementation, you would have a specific user data type and would write functions to manage user accounts and convert the data to the claims returned for a user info request [^user-claims]. The `Scim` module includes an out of the box option, based on the SCIM specification [^scim].

[^default-session]: Here we are using the default session implementation which is based on the [`clientsession`](http://hackage.haskell.org/package/clientsession) package.
[^scim]: The aim is to build a full implementation of [the SCIM 2 spec](http://www.simplecloud.info/), but this is a work in progress.
[^user-claims]: OpenID Connect defines a specific set of [claims](http://openid.net/specs/openid-connect-core-1_0.html#Claims), which unfortunately aren't directly compatible with SCIM.

If you want to use a database, there's a [`Persistent`](TODO) backend provided out of the box, which uses the Scim module. We could swap from using in-memory to Persistent just by using

``` haskell
persistConfig <- persistBackend pool <$> inMemoryConfig issuer
```
where `pool` is a Persist `ConnectionPool` instance.

# Authorization Code Flow Walk-Through

Using the server above, we can go through a typical flow to authenticate a user. We'll use `curl` to take the place of the client.

TODO: curl commands and UI interaction

# Configuration

One of the things I had trouble with was the finding the best approach for building a configurable server application. It's easy enough to come up with intelligent defaults for an OpenID Provider, but pretty much all the functionality needs to be pluggable [^haskell-di]. The `Broch.Server.Config` module contains data structures for settings which are used to initialize a server, and also functions which define pluggable behaviour

[^haskell-di]: Functional programmers enjoy making fun of other languages and their inadequacies in this department. Just mention "dependency injection" and watch the reaction, but concrete examples of how to build a Haskell server with a pluggable configuration are rather thin on the ground, so for a beginner it's not clear where to start. If you're using a framework like Yesod, then it uses typeclasses to implement different functionality you might want in your application, and you can override specific functions if you wish. However, I'd already decided I didn't want to tie the project to any specific framework and I wasn't overly keen on typeclass-mixing. The approach I finally settled on was inspired by this [Stack Overflow answer](http://stackoverflow.com/a/14329487/241990).

``` haskell
data Config m s = Config
    { issuerUrl                  :: Text
    , publicKeys                 :: [Jwk]
    , signingKeys                :: [Jwk]
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

# Request Processing

The majority of the functionality for processing authorization and token requests is decoupled from the HTTP interface (WAI). The web handlers extract the request data, bundle the parameters up in a map, then delegate the detailed work to other functions. This makes it easier to test the core functionality and to use it with a different web front-end.

## Authorization Endpoint

The result of an authorization request can be one of

* A redirect containing the authorization information (an authorization code, access token or whatever other data is required by the grant request)
* A redirect to enable user authentication, before continuing processing the original authorization request
* An error returned to the user agent, due to a potentially malicious client request
* A redirect error, where the error information is returned to the client in the URL.

The authorization web handler authenticates the user and then delegates to the function [`processAuthorizationRequest`](TODO).

## Token Endpoint

The token endpoint authenticates the client and then calls the function [`processTokenRequest`](TODO), while return a JSON response as defined in the specification a token response or an error response.

## Dynamic Registration

The server can optionally support client registration.

## Discovery

The discovery endpoint just provides a well-known location for clients to obtain a copy of the server's configuration and supported features, such as the algorithms which can be used for encoding JWTs.

## UserInfo

A client can optionally retrieve user details from the "user info" endpoint, by submitting the access token which was issued by the authorization server. This isn't strictly necessary, as OpenID Connect also issues an ID token which can represent the authenticated user.

# Other Topics

## UI

TODO: Improve look of Blaze UI.

TODO: UI needs to be completely decoupled from handlers. This should also allow the login page to be defined externally. Routing also really needs to be more composable for this to work, to allow paths to be added into the existing table.

## Client Authentication

OAuth2 only mentions client authentication using a password/secret, either using a Basic authorization header or passing the credentials in the request body. Providers are also free to accept other forms of authentication.

OpenID Connect explicitly defines the client authentication methods which it supports. All of these are available in this implementation.

## Key Rotation

## Customization

* Enabling/Disabling features
* Different ID Token contents
* Adding a standard OAuth2 protected endpoint
