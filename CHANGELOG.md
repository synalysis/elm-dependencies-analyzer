# Changelog

## 2018-12-06
- Packages are now shown in two sections: direct & indirect
  - Section is based on parsed `elm.json`.
  - New packages are shown at end of each section, marked with "NEW".
- New packages can also be marked direct.
- FIXED: Showing red background was ignoring selected versions of
  indirect packages.

## 2018-12-03
- Better reporting of which packages are in conflict.
  - Before: `1.0.1 <= v < 2.0.0 needed by elm/bytes 2.0.0 (and 2 others)`
    - most relevant package `elm/http` hidden behind `(and 2 others)`
  - After: `1.0.1 <= v < 2.0.0 needed by elm/http 2.0.0`
    - only the relevant package is reported
