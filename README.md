# http-dispatch

`http-dispatch` is a small, composable HTTP client DSL for Haskell. It keeps
the common path obvious while exposing the production controls that real
clients need.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Function ((&))
import Network.HTTP.Dispatch

main :: IO ()
main = do
  client <- newClient
  response <- send client $
    get "https://api.example.com/users"
      & pathSegment "owain lewis"
      & queryParam "include" "teams"
      & bearerAuth "token"
      & withTimeout 10000000
      & expect2xx
  print (responseStatus response, responseBody response)
```

The DSL is a thin layer over
[`http-client`](https://hackage.haskell.org/package/http-client). There is no
lens dependency, custom effect system, or second transport stack.

## Why use it?

- A `Client` reuses connections and handles HTTP and HTTPS with one manager.
- Plain functions compose with `(&)` from `Data.Function`.
- Invalid URLs, transport failures, status policies, body limits, and decode
  failures have distinct errors.
- Non-2xx responses remain ordinary responses unless you add `expect2xx` or
  `expectStatus`.
- Buffered responses have a safe 16 MiB default limit. Streaming is bracketed
  and constant-memory.
- Proxy environment variables and `NO_PROXY` suffix exclusions are delegated
  to `http-client`. Explicit and
  disabled per-request proxies are supported.
- Retries are off by default, bounded, jittered, respect `Retry-After`, and are
  restricted to configured methods and replayable bodies.
- JSON, URL-encoded forms, multipart uploads, file streams, strict, lazy, and
  custom streaming bodies are supported.
- Redirects strip explicit authorization, cookie, proxy authorization, and host
  headers on every hop.
- Cookie persistence is opt-in through `newSessionClient`.

## Requests

Every builder returns an opaque `HTTPRequest`, so the library can add transport
controls without breaking record construction.

```haskell
request =
  post "https://api.example.com/v1/events"
    & withJsonBody event
    & withHeader ("Idempotency-Key", key)
    & basicAuth username password
    & withRedirects 3
    & maximumResponseBytes (2 * 1024 * 1024)
    & expect2xx
```

Available request controls include:

- methods: `get`, `head`, `post`, `put`, `patch`, `delete`, `options`, and
  `customMethod`;
- URL construction: `pathSegment`, `queryParam`, `queryParamText`, and
  `queryFlag`;
- bodies: `withBody`, `withLazyBody`, `withRequestBody`, `withJsonBody`,
  `withFormBody`, and `withMultipartBody`;
- auth: `basicAuth`, `bearerAuth`, and `proxyBasicAuth`;
- transport: `withProxy`, `withoutProxy`, `withTimeout`, `withoutTimeout`,
  `withBodyTimeout`, `withoutBodyTimeout`,
  `withRedirects`, `withoutRedirects`, `withCookieJar`, and `rawResponseBody`;
- policy: `expect2xx`, `expectStatus`, `acceptAnyStatus`, `retrying`,
  `maximumResponseBytes`, and `unlimitedResponseBody`;
- advanced escape hatch: `modifyClientRequest`.

Query items preserve order and repeated keys. `queryFlag "debug"` produces
`?debug`, while `queryParam "debug" ""` produces `?debug=`. `pathSegment`
accepts `Text` and percent-encodes UTF-8 exactly once.

## Responses and errors

`send` returns a buffered response and throws `HTTPError`. Use `trySend` when
errors should be values.

```haskell
result <- trySend client request
case result of
  Left (InvalidUrl target reason) -> ...
  Left (TransportError cause) -> ...
  Left (UnexpectedStatus response) -> ...
  Left (ResponseTooLarge limit) -> ...
  Left BodyTimeout -> ...
  Left (DecodeError reason) -> ...
  Left (RequestBuildError reason) -> ...
  Right response -> ...
```

`decodeJson` and `decodeText` preserve the response status, headers, cookies,
and HTTP version while changing only its body type.

## Streaming

Use `withStreamingResponse` for large or unbounded responses. The `BodyReader`
is valid only inside the callback. The connection is always released when the
callback returns or throws.

```haskell
result <- withStreamingResponse client request $ \response ->
  copyBodyReaderToFile (responseBody response) "archive.tar"
```

For streamed uploads, pass an `http-client` `RequestBodyStream`,
`RequestBodyStreamChunked`, or `RequestBodyIO` to `withRequestBody`. The Boolean
argument declares whether the body can be recreated safely for a retry. Use
`False` for one-shot producers.

## Clients, proxies, TLS, and cookies

`newClient` owns long-lived TLS-capable managers. They are reclaimed
automatically after the client becomes unreachable. It delegates `HTTP_PROXY`,
`HTTPS_PROXY`, and `NO_PROXY` suffix matching to `http-client`. Use
`newClientWith` with customized `tlsManagerSettings` or another
`ManagerSettings` value for custom trust stores, client certificates,
connection limits, and header limits. Select `ProxyEnvironment`,
`ProxyFromRequest`, or `ProxyFromManager` with `clientProxyPolicy`. The last
option preserves a custom selector already present in `managerSettings`.

Use `withProxy` or `withoutProxy` for a single request. These overrides are
supported by `newClientWith` and `clientFromManagers`; `clientFromManager`
rejects them because an arbitrary manager may replace the request proxy. Proxy
authentication is added with `proxyBasicAuth`. HTTPS proxying and CONNECT
behavior are delegated to `http-client-tls`.

`newClient` does not retain cookies. `newSessionClient` creates a concurrent,
delta-merged cookie session backed by `http-client`'s RFC 6265 jar. A request-level
`withCookieJar` overrides the session jar for that request. Cookie jars apply
domain, path, expiry, and `Secure` rules again at each redirect, so a matching
jar cookie may be attached even though an explicit `Cookie` header is stripped.
Use `withoutRedirects` and validate the next target when no credential may be
forwarded automatically.

## Retries

Retries are deliberately disabled by default.

```haskell
policy = defaultRetryPolicy
  { retryLimit = 3
  , retryBaseDelayMicros = 100000
  , retryMaxDelayMicros = 5000000
  }

request = get url & retrying policy
```

The default retry status set is 408, 429, 502, 503, and 504. The default method
set is HEAD, GET, PUT, DELETE, TRACE, and OPTIONS. Streaming bodies are marked
non-replayable unless explicitly declared otherwise. POST and PATCH require an
explicit method policy and a replayable body, which prevents accidental
duplicate writes.

For streaming responses, retryable statuses and transport failures are handled
before the callback begins. Once the callback starts it is invoked at most once;
body-read failures are returned and are never retried because the callback may
already have produced side effects.

## Security boundaries

TLS certificates and hostnames are verified by default. Explicit sensitive
headers are stripped on every redirect, including same-origin redirects. Cookie
jars still apply their RFC matching rules to the new target. For a strict
no-credential-forwarding boundary, disable automatic redirects and validate the
target before issuing a new request. Exception rendering is delegated to
`http-client`, which redacts authorization by default.

This package is not, by itself, an SSRF firewall. Applications that send
requests to user-controlled destinations should validate schemes, hosts,
resolved addresses, ports, redirects, and proxy behavior against an allowlist.
Use `modifyClientRequest` and custom manager hooks for application-specific
destination enforcement.

The transport is HTTP/1.1. The public request and response types are kept
opaque/generic so a mature HTTP/2 backend can be added later without forcing a
new DSL.

## Migrating from 1.x

Version 2 is a major API redesign. The familiar `get`, `delete`, `options`,
`raw`, and `http` entry points remain. The typo `resposeBody` remains as a
deprecated alias for `responseBody`.

Important changes:

- `HTTPRequest` is opaque instead of a four-field record.
- `HTTPResponse` is parameterized by its body type and contains cookies and the
  HTTP version.
- `post`, `put`, and `patch` now take only a URL. Add a body with `withBody` or a
  typed body helper.
- modifiers take the value first and request last so they compose with `(&)`.
- `responseStatus` is still an `Int`. `responseStatusMessage` is now retained.
- buffered responses default to a 16 MiB decompressed body limit.
- a reusable client is the primary API. `http` remains a one-shot convenience
  backed by the shared TLS manager.

`Network.HTTP.Dispatch.Compat` provides deprecated `legacyPost`, `legacyPut`,
`legacyPatch`, and legacy modifier argument orders for incremental migrations.

See [`docs/tutorial.md`](docs/tutorial.md) for complete examples.
