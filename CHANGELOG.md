# Changelog

## 2.0.0.0

- Replaced the public request record with an opaque, evolvable request DSL.
- Added reusable TLS clients, environment and request proxies, opt-in cookie
  sessions, timeout and redirect controls, status policies, bounded buffering,
  and bracketed streaming.
- Added JSON, form, multipart, file, lazy, and streaming request bodies.
- Added custom methods, UTF-8 path segments, ordered query parameters, basic,
  bearer, and proxy authentication.
- Added explicit error types and typed response bodies.
- Added safe opt-in retries with replayability and method checks, exponential
  full-jitter backoff, and `Retry-After` support.
- Kept the misspelled `resposeBody` accessor and common v1 entry points as
  compatibility paths. Added `Network.HTTP.Dispatch.Compat` for incremental
  migration.
- Replaced CircleCI and the obsolete Stack snapshot with a current Cabal and
  GitHub Actions build matrix.
