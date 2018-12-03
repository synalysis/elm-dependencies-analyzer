# Changelog

## 2018-12-03
- Better reporting of which packages are in conflict.
  - Before: `1.0.1 <= v < 2.0.0 needed by elm/bytes 2.0.0 (and 2 others)`
    - most relevant package `elm/http` hidden behind `(and 2 others)`
  - After: `1.0.1 <= v < 2.0.0 needed by elm/http 2.0.0`
    - only the relevant package is reported
