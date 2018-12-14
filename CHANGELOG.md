# Changelog

Only a selection of changes are listed here.

## 2018-12-14
- Automatic reporting of (some) internal errors

## 2018-12-09
- Include test-dependencies
  - Packages are now shown in four sections:
    direct, indirect, test-direct & test-indirect
- Create and show new `elm.json`, with download option.

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
