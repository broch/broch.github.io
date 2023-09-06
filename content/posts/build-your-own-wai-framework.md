---
title: Build Your Own Haskell Web Framework on WAI
author: Luke
date: 2015-04-19
thumb: /images/haskell-logo.svg
thumb_alt: The Haskell logo
description: Investigating the design of WAI-based Haskell web frameworks.
tags: [Haskell,WAI]
---

This article shows how you can build on top of the basic request/response handling functionality provided by [WAI][wai] and the [Warp server][warp], to support some of the requirements you might have in a typical web application. The content is mostly gleaned from my research into the code of several WAI-based web frameworks to try to understand how they work. Building a web application was one of the things I tackled when I didn't really know Haskell well enough, so hopefully this will be useful if you're at a similar stage and would like to understand what's going on in a bit more depth. I'll outline some of the features these frameworks add, build a similar (but simplified) implementation, and also provide links to the source code of some real-world frameworks built on WAI (such as [Scotty][scotty], [Spock][spock] and [Yesod][yesod]) for comparison.

Whether you need to use an additional framework on top of WAI will very much depend on your requirements, how complicated your application is and whether you want to track the extra dependencies in your project. Frameworks cater for general cases (making the types more complex for a beginner) and they have a lot of features. You should certainly try out something like Spock or Scotty as they are easy to get started with. For a simple application, or one where you need finer control over handling requests, you might then consider a customized approach. On the other hand, you might overlook something important which the framework authors didn't [^warp-dos] -- the code in this article is only meant to be a rough outline. If you *do* decide to "build your own," please think hard before releasing it to Hackage. There are more than enough WAI frameworks out there already [^wai-frameworks].

[wai]: http://hackage.haskell.org/package/wai
[warp]: http://hackage.haskell.org/package/warp
[scotty]: http://hackage.haskell.org/package/scotty
[spock]: http://www.spock.li/
[yesod]: http://www.yesodweb.com/
[simple]: http://hackage.haskell.org/package/simple
[wheb]: http://hackage.haskell.org/package/Wheb

[^warp-dos]: Warp doesn't automatically limit the request size, for example, so someone can crash your application by sending a very large request. For example, you can use the curl command `curl -v --data-urlencode 'username@my_giant_file.txt' localhost:3000/login` to send a large file as a parameter. See also, Yesod's `maximumContentLength` setting, which it uses to [limit the request body size](https://github.com/yesodweb/yesod/blob/yesod-core/1.4.6/yesod-core/Yesod/Core/Internal/Request.hs#L55).

[^wai-frameworks]: [Scotty][scotty], [Yesod][yesod], [Hails](http://hackage.haskell.org/package/hails), [Apiary](http://hackage.haskell.org/package/apiary), [Spock][spock], [Wheb][wheb], [Simple][simple]. For a more complete list, you can look through [Warp's reverse dependencies](http://packdeps.haskellers.com/reverse/warp).

### Basic WAI

WAI ("web application interface") is a Haskell HTTP request/response API. Theoretically it is server-agnostic but in practice it is really only implemented by the warp server.

Request handling in WAI is defined by the `Application` type [^yesod-book-wai] :

```haskell
type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
```

The [`Request`][wai-request] gives access to the request headers, query string, request body and so on, while the `Response -> IO ResponseReceived` callback allows us to send a response we have created. A typical WAI example you might come across will show how to send a simple response:

[wai-request]: http://hackage.haskell.org/package/wai-3.0.2.1/docs/Network-Wai-Internal.html#t:Request

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)

app :: Application
app _ respond = respond $
  responseLBS status200 [("Content-Type", "text/plain")] "hello"

main = run 3000 app
```

So from a web developer's perspective, a WAI application is a single function which is called for each request and sends back a response. The code runs in the IO monad and there's no out-of-the-box support for performing redirects, cookie handling, managing sessions or supporting different response types such as text, HTML or JSON. Web frameworks like Scotty and Yesod build these features on top of WAI using their own custom handler monads, meaning you won't usually call the WAI functions directly in your code. Frameworks also provide some kind of routing DSL, usually based on the request path and method (GET, POST etc.), so you can map different requests to different handler functions.

[^yesod-book-wai]: For a good overview of WAI, see the [Yesod Book](http://www.yesodweb.com/book-1.4/web-application-interface).

### The Handler Monad

The handler monad provides convenient (read-only) access to the request (headers, parameters) and also provides functions to build the response. This is typically achieved using a combination of `ReaderT` and `StateT` monad transformers [^rwst] [^apiary-action]. So we could start with something like

[^rwst]: The [RWST](https://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-RWS-Strict.html#g:2) monad transformer is another possibility and is used by the [Spock Framework](https://github.com/agrafix/Spock/blob/0.7.5.1/src/Web/Spock/Internal/Wire.hs#L89). In this case the "writer" part of the monad is ignored.
[^apiary-action]: For an example which builds its own monad from scratch, see Apiary's [`ActionT`](http://hackage.haskell.org/package/apiary-1.2.0/docs/src/Control-Monad-Apiary-Action-Internal.html#ActionT) or Simple's [`ControllerT`](https://github.com/alevy/simple/blob/v0.9.0.0/simple/src/Web/Simple/Controller/Trans.hs#L51).

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types (ResponseHeaders, Status)
import Network.Wai (Request)


type Params = Map Text [Text]

data RequestData = RequestData
    { waiReq :: Request
    , queryParams :: Params
    , postParams  :: Params
    }

data ResponseState = ResponseState
    { resStatus :: Status
    , resHeaders :: ResponseHeaders
    , content :: BL.ByteString
    }

type Handler a = ReaderT RequestData (StateT ResponseState IO) a
```

`RequestData` provides access to the original WAI request as well as the parsed request parameters and is accessed via the reader monad. `ResponseState` stores the status code, headers and response content. Here we're assuming the only requirement is to handle simple content we can create as a `ByteString`, so we're forgetting about streaming responses or serving up files directly. Similarly we're ignoring file uploads in the request data [^files].

[^files]: You'll see these extra request and response data options in Scotty's [`ActionEnv` and `Content`](https://github.com/scotty-web/scotty/blob/0.9.0/Web/Scotty/Internal/Types.hs#L108) types, for example.

### Routing

Our application will consist of request handler functions written in the `Handler` monad. We also need some way of mapping different requests to the correct handlers. Frameworks generally include a DSL to do this, often using Sinatra-style verb/path combinations, including support for capturing URL parameters and converting parameters to specific types.

A very simple routing option is to just pattern match on the `pathInfo` property of the WAI `Request`, which is of type `[Text]`:

```haskell
type Router = [Text] -> Handler ()
```

We can then build our application as a simple routing table:

```haskell
myAppRouter :: Router
myAppRouter path = case path of
  ["home"]    -> myHomePageHandler
  ["login"]   -> loginHandler
  ["logout"]  -> logoutHandler
  ["user", u] -> userHandler u
  _           -> notFound
```

For a given request, the router will give us a corresponding handler which we can run. The type is `Handler ()` since the handler doesn't return anything. The `ResponseState` retrieved from the State monad gives us all we need to send the response. This isn't a very flexible approach, but it's very easy to understand and fine as a first option if we don't need to be able to compose routers and so on. You can find routing packages on Hackage but that's a topic for another time.

### Running the Handler

What does it actually mean to run the handler? Before we look at the code, we need to make some minor changes to the `Handler` type to support short-circuiting.

#### Short-Circuiting in the Handler Monad

In a web application, if we redirect to a different URL, we generally want the response to complete at that point. For example, if we have a request which requires an authenticated user, we might redirect them to a login page if they haven't logged in, but if they're already authenticated, we'd want the handler code to proceed.
Another obvious short-circuiting case is when something goes wrong during execution and we want to immediately send an error response.
If the monad doesn't short-circuit, then the only alternative is to use nested `if/else` or `case` statements to control which code is executed [^redirect-type].

[^redirect-type]: If we look at the type signatures for the `redirect` functions in existing frameworks, the handler monad is parameterized with an arbitrary type. In Scotty, for example, the type is `redirect :: Text -> ActionM a` so we can immediately deduce that `redirect` *must* short-circuit since it can't return an arbitrary value.

You might also want the monad to short-circuit whenever you write the response content. In "real world" frameworks the behaviour varies so you need to know how each of them work [^rw-short-circuit].

[^rw-short-circuit]: Scotty [doesn't complete the response](https://github.com/scotty-web/scotty/blob/0.9.0/Web/Scotty/Action.hs#L273) when you write the content using a function like `text` or `json` whereas [Spock does](https://github.com/agrafix/Spock/blob/0.7.5.1/src/Web/Spock/Internal/CoreAction.hs#L210). A list of Yesod handler functions which short-circuit can be found in the [Routing and Handlers](http://www.yesodweb.com/book-1.4/routing-and-handlers) chapter of the Yesod book.

So how do we make our monad short-circuit? One option is to add the `EitherT` monad transformer to our existing monad. If you're not familiar with `EitherT`, the behaviour is analogous to the familiar `Either` type [^eithert]. If we call [`left`](http://hackage.haskell.org/package/either-4.3.2/docs/Control-Monad-Trans-Either.html#v:left) (or equivalently `throwError` since `EitherT` is a `MonadError` instance), the monad will short-circuit [^either-except].

[^eithert]: `EitherT` can be found in the [`either`](http://hackage.haskell.org/package/either) package and is also re-exported by the `errors` package.

[^either-except]: You may notice that `ExceptT` is [used in practice](https://github.com/scotty-web/scotty/blob/master/Web/Scotty/Internal/Types.hs#L137) instead of `EitherT`. However, this requires version 2.2.1 or later of the `mtl` library, which in turn requires the use of `transformers 0.4.*`. GHC 7.8 comes with transformers 0.3 so you can end up with conflicting versions in your project if it depends on GHC and cabal will complain. `EitherT` does the same job, more or less, so we stick with that for now.


```haskell
data HandlerResult = Redirect ByteString     -- Redirect to a URL
                   | ResponseComplete        -- Send the response
                   | HandlerError ByteString -- Send an internal error response
                     deriving (Show, Eq)

type Handler a = EitherT HandlerResult (ReaderT RequestData (StateT ResponseState IO)) a
```

When we call `runEitherT` followed by `runReaderT` and `runStateT`, the result is of type `IO (Either HandlerResult (), ResponseState)`.

#### The `runHandler` Function

As things stand now, we have a WAI `Request` object passed as an argument to the `Application` type. To process it, we lookup the handler in our `Router` and then:

- Create a `RequestData` from the `Request`
- Create an initial `ResponseState`
- Run the hander to get back the `Either HandlerExcept ()` result and the final `ResponseState`

The complete `runHandler` function looks like this:


```haskell
import Network.Wai.Parse

runHandler :: Request -> Handler () -> IO Response
runHandler req h  = do
    (pParams, _) <- parseRequestBody lbsBackEnd req
    let initRes = ResponseState status200 [] ""
        rd = RequestData
              { waiReq      = req
              , queryParams = toMap $ fmap (\(n, v) -> (n, fromMaybe "" $ v)) $ queryString req
              , postParams  = toMap pParams
              }

    (result, res) <- runStateT (runReaderT (runEitherT h) rd) initRes
    let hdrs = resHeaders res
    return $ case result of
        Left ResponseComplete   -> responseLBS (resStatus res) hdrs (content res)
        Left (Redirect url)     -> responseLBS status302 ((hLocation, url) : hdrs) ""
        Left (HandlerError msg) -> responseLBS internalServerError500 hdrs (BL.fromStrict msg)
        Right _ -> error "Not handled"

toMap :: [(ByteString, ByteString)] -> Params
toMap = M.unionsWith (++) . map (\(x, y) -> M.singleton (TE.decodeUtf8 x) [TE.decodeUtf8 y])
```

The function [`parseRequestBody`](https://github.com/yesodweb/wai/blob/wai-extra/3.0.3/wai-extra/Network/Wai/Parse.hs#L174) is part of the [`wai-extra`][wai-extra] library. It attempts to parse the request body as [HTML Form data](http://www.w3.org/TR/html401/interact/forms.html#h-17.13.4), returning a tuple containing a list of submitted parameters and a list of uploaded files. Since we aren't supporting file uploads we ignore the second element of the tuple. If the request `content-type` is neither `application/x-www-form-urlencoded` nor `multipart/form-data`, then both these arrays will be empty and we need to read and parse the request body ourselves. We'll look at this below.

[wai-extra]: http://hackage.haskell.org/package/wai-extra

Note that we're taking the approach that *all* responses should short-circuit and assume it's a programmer error if the handler doesn't redirect, write a response or return an error message. This might be confusing if you're used to the `Left` constructor of `Either` being the "error" case, but it's really just the case that short-circuits [^scotty-error].

[^scotty-error]: It's also confusing that the naming conventions often reinforce this. For example, Scotty's [`ActionError`](https://github.com/scotty-web/scotty/blob/0.9.0/Web/Scotty/Internal/Types.hs#L76) type deals with both redirects and errors.

### Functions in the Handler Monad

The handler monad is not very useful by itself. We want to hide the details behind a convenient API for reading request properties and creating the response. We'll look at some simple examples, but you can obviously write whatever functions best suit your needs.

#### Reading the request

When processing a request, we typically want to read parameters and/or the request body. Most frameworks do not differentiate between different types of request parameters, but let's suppose we want to treat request body parameters separately from query string parameters [^sec-param]. We'll also assume that it's an error to send duplicate values of the same parameter:

[^sec-param]: For example, we might want to report an error if sensitive data like a password is sent in a URL. We couldn't do this using Scotty's `param` function, for instance.

```haskell
postParam :: Text -> Handler Text
postParam name = asks postParams >>= lookupParam name

queryParam :: Text -> Handler Text
queryParam name = asks queryParams >>= lookupParam name

lookupParam :: Text -> Params -> Handler Text
lookupParam name params = case M.lookup name params of
    Just [v] -> return v
    _        -> throwError $ HandlerError $ B.concat ["Missing or duplicate parameter", TE.encodeUtf8 name]
```

WAI's `Request` record type has a field called `requestBody` which is of type `IO ByteString`. It produces the complete body a chunk at a time, returning an empty `ByteString` when the body is completely consumed. There's also a convenience function to do this, which we can wrap to create our `body` function:

```haskell
body :: Handler BL.ByteString
body = asks waiReq >>= liftIO . strictRequestBody
```

Note that the body can only be read once. It may already have been read by the function `parseRequestBody` which we used above and in that case, the `body` function would return an empty value [^scotty-body].

[^scotty-body]: Scotty [reads the request body](https://github.com/scotty-web/scotty/blob/0.9.0/Web/Scotty/Route.hs#L137) and stores it along with the other request data so that it can be accessed more than once.

#### Building the response

For the response, we'll start by writing functions to:

- redirect to another URL
- set the status
- set the content as text, JSON, HTML

The redirect function just takes a URL as a `ByteString` and short-circuits with the corresponding `HandlerResult` value:

```haskell
redirect :: ByteString -> Handler a
redirect = throwError . Redirect
```

The `runHandler` function we wrote above does the rest of the work, setting the status code to 302 and the `Location` header to the supplied URL.

Setting the response status to a different value is easily done by changing the state:

```haskell
status :: Status -> Handler ()
status s = modify $ \rs -> rs { resStatus = s }
```

and we can write the response content as text, JSON or (Blaze) HTML using the following functions:

```haskell
import Data.Aeson
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

text :: Text -> Handler ()
text t = setContentType "text/plain; charset=utf-8" >> (rawBytes . BL.fromStrict $ TE.encodeUtf8 t)

json :: ToJSON a => a -> Handler ()
json j = setContentType "application/json" >> rawBytes (encode j)

html :: Html -> Handler ()
html h = setContentType "text/html; charset=utf-8" >> rawBytes (renderHtml h)

rawBytes :: BL.ByteString -> Handler ()
rawBytes b = modify (\rs -> rs { content = b }) >> throwError ResponseComplete

setHeader :: HeaderName -> ByteString -> Handler ()
setHeader name value = modify $ \rs -> rs { resHeaders = (name, value) : resHeaders rs }

setContentType :: ByteString -> Handler ()
setContentType = setHeader "Content-Type"
```

## Exception Handling

So far we've assumed that every `Handler` will produce a value of type `Either HandlerResult ()`, but what happens if the code throws an exception instead? We can test this easily by just adding the following route to our `myAppRouter` above:

```haskell
["eek"]  -> error "eek!"
```

Requesting the URL `/eek` from a browser returns the text response "Something went wrong" with a 500 response code. This is the default response produced by Warp's internal error handler and it is easily customized [^warp-exception]. Alternatively we can catch the exception ourselves. We still need a function to convert our router into an `Application`, so we can do it there:

```haskell
routerToApplication :: Router -> Application
routerToApplication route req respond =
  (runHandler req $ route pathInfo req)
    `catch` λ(e :: SomeException) -> return $ responseLBS internalServerError500 [] $ "Internal error"
```

[^warp-exception]: The [`setOnExceptionResponse`](http://hackage.haskell.org/package/warp-3.0.5/docs/Network-Wai-Handler-Warp.html#v:setOnExceptionResponse) setting can be used for customization. The exception is caught and the response sent in the [`serveConnection`](https://github.com/yesodweb/wai/blob/warp/3.0.5/warp/Network/Wai/Handler/Warp/Run.hs#L282) function. The exception is then re-thrown to the `fork` function which [calls the exception handler](https://github.com/yesodweb/wai/blob/warp/3.0.5/warp/Network/Wai/Handler/Warp/Run.hs#L256) configured with [`setOnException`](http://hackage.haskell.org/package/warp-3.0.5/docs/Network-Wai-Handler-Warp.html#v:setOnException) and cleans up resources.


## Conclusion

Even though WAI is not really a standard web interface supported by multiple servers, it *is* common to multiple frameworks so an understanding WAI and Warp is useful if you are likely to be developing Haskell web applications.

In this article we've built a simple set of functions with which we can write web handlers which would look quite similar to those of a framework like Scotty, and you should now hopefully have a clearer idea of how they work. The full code can be downloaded [here](BuildYourOwnWai.hs). For a more complex example, you can also see this kind of code in use in a project I've been working on which is an implementation of the [OpenID Connect specification][openid-connect] in Haskell [^broch]. I'll hopefully find time to write up more articles on this topic as the development proceeds.

[openid-connect]: http://openid.net/developers/specs/

[^broch]: The project is also on [github](https://github.com/tekul/broch). It's a work in progress but also includes session handling, for example.
