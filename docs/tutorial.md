# Tutorial

## Setup

Enable `OverloadedStrings` and use `(&)` for left-to-right request building.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Function ((&))
import Network.HTTP.Dispatch
```

Create one client per application or subsystem and reuse it. A client owns the
connection pool and TLS settings.

```haskell
main = do
  client <- newClient
  runApplication client
```

`newClient` honors proxy environment variables. Its managers are reclaimed
automatically when the client becomes unreachable. Use `newSessionClient` when
requests should share cookies.

## Basic requests

```haskell
users <- send client (get "https://api.example.com/users")

created <- send client $
  post "https://api.example.com/users"
    & withJsonBody newUser
    & bearerAuth token
    & expect2xx
```

The standard verbs do not assume whether a body is legal or required. This
keeps extension APIs and dynamic clients possible. Add the appropriate body
explicitly.

## Paths and queries

```haskell
request =
  get "https://api.example.com/v1"
    & pathSegment accountId
    & pathSegment "events"
    & queryParamText "from" startDate
    & queryParam "tag" "haskell"
    & queryParam "tag" "http"
    & queryFlag "include_deleted"
```

Path segments are UTF-8 percent-encoded once. Query ordering, repeated keys,
flags, and empty values are preserved.

## Headers and authentication

`withHeader` appends rather than replaces. This matters for headers that may be
repeated. Use `setHeader` for one replacement or `setHeaders` to replace all
headers.

```haskell
request =
  get url
    & withHeader ("Accept", "application/json")
    & withHeader ("X-Trace-Id", traceId)
    & basicAuth username password
```

`bearerAuth` and `proxyBasicAuth` are also available. Authorization, cookies,
proxy authorization, and host overrides are stripped on every redirect. This
safe default prevents HTTPS downgrade and cross-port leaks. Disable automatic
redirects and handle the target explicitly when a same-origin redirect must
retain credentials.

## Bodies

```haskell
strictRequest = post url & withBody bytes
lazyRequest   = post url & withLazyBody lazyBytes
jsonRequest   = post url & withJsonBody value
formRequest   = post url & withFormBody [("name", "Owain"), ("active", "true")]
```

Multipart values use the constructors re-exported from `http-client`:

```haskell
upload =
  post url
    & withMultipartBody
        [ partBS "description" "release artifact"
        , partFileSource "archive" "dist/release.tar.gz"
        ]
```

`partFileSource` streams the file. Its filename is reduced to the basename by
the underlying multipart implementation.

For a custom producer, use `withRequestBody replayable body`. A known-length or
chunked `RequestBodyStream` should normally be marked `False` unless each retry
can recreate the exact stream safely.

## Status handling and decoding

All statuses are responses by default.

```haskell
result <- trySend client (get url)
case result of
  Right response | responseStatus response == 404 -> handleMissing
  Right response -> handleResponse response
  Left err -> handleFailure err
```

Add `expect2xx` or `expectStatus [200, 202]` when other statuses should become
`UnexpectedStatus`. The error retains the complete buffered response body.

Decode after transport and status handling:

```haskell
case trySend client request of
  ...

response <- send client request
case decodeJson response of
  Left (DecodeError reason) -> ...
  Right typedResponse -> useValue (responseBody typedResponse)
```

This keeps malformed JSON distinct from DNS, TLS, timeout, status, and size
failures.

## Streaming downloads

Buffered responses are limited to 16 MiB after decompression. Raise the bound
with `maximumResponseBytes` or remove it with `unlimitedResponseBody` only when
the response is trusted and memory use is acceptable.

For large bodies, stream instead:

```haskell
result <- withStreamingResponse client (get url & expect2xx) $ \response ->
  consumeReader (responseBody response)
```

Never return the `BodyReader` from the callback. The socket is bracketed to the
callback lifetime, including cancellation and exceptions.

## Timeouts and redirects

```haskell
request =
  get url
    & withTimeout 10000000
    & withRedirects 3
```

Timeouts are expressed in microseconds. They cover connection and response
headers through `http-client`, then act as an inactivity timeout for each body
read. They are not a total wall-clock deadline for a steadily progressing
stream. Apply an application-level deadline when the whole operation needs a
fixed budget.

Redirect behavior follows `http-client`: counts are bounded, relative
locations are resolved, and status-specific method/body rules are applied.
Sensitive headers are stripped on every redirect by this package.

## Proxies and TLS

The default client reads `HTTP_PROXY`, `HTTPS_PROXY`, and `NO_PROXY` through
`http-client`. Its `NO_PROXY` behavior is comma-separated host/domain suffix
matching, not wildcard, port, or CIDR matching. Override a single request with
`withProxy (Proxy host port)` or `withoutProxy`. Add proxy credentials with
`proxyBasicAuth`.

For custom certificate authorities, mutual TLS, connection limits, header
limits, or a proxy selector, create `ClientOptions` from
`defaultClientOptions tlsManagerSettings`, update its `managerSettings`, then
pass it to `newClientWith`.

```haskell
import Network.HTTP.Client.TLS (tlsManagerSettings)
```

## Safe retries

```haskell
policy = defaultRetryPolicy
  { retryLimit = 3
  , retryBaseDelayMicros = 100000
  , retryMaxDelayMicros = 5000000
  }

request = get url & retrying policy
```

`retryLimit` counts retries after the first attempt. Delay uses exponential
backoff with full jitter and respects server `Retry-After` values up to the
configured maximum. Async exceptions are not caught by retry handling.

Only configured methods and replayable bodies retry. To retry a POST safely,
include `POST` in `retryMethods`, ensure the server supports an idempotency key,
add that key, and mark a recreated body `retryable` when necessary.

## Advanced transport access

`inspectRequest` compiles the DSL without sending it. It is useful in tests.
`modifyClientRequest` applies an IO transformation to the final
`http-client Request` for features not yet covered by the DSL, such as custom
TLS routing, signing, redaction sets, early hints, or destination policy hooks.
