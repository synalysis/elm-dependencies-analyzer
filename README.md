# elm-dependencies-analyzer

A little program Markus Laire created for analyzing package dependencies of application elm.json.

[This is my first Elm program of over 1000 lines, and code quality clearly reflects that. :D]

Live version: https://www.elm-dependencies-analyzer.net/

## Usage

### Loading elm.json

1) Open your application `elm.json` or copy-paste it into the editor.
   - You can also load an example with some conflicting packages with "Load example".
2) Press "Analyze" to parse JSON and load package information.
   - Loaded information is cached for 24 hours in browser and in backend cache.
     If cache has become stale, loading can take a moment while cache is being refreshed, but subsequent loads will go faster.

Once `elm.json` has been analyzed, table on right will show all relevant packages.
For each package the version in `elm.json` and all newer versions will be shown.

### Packages

- Packages are shown in four sections based on `elm.json`:
  direct, indirect, test-direct and test-indirect
- Direct packages have checkmark and black font color.
- Indirect packages have gray font color.
- Use checkboxes to change whether package is considered direct package or not.
- Test-dependencies are marked with `TEST`.
- Packages which are mentioned in `elm.json`,
  but not needed by current direct packages, are marked with strikethrough.
- Additional packages which are not mentioned in `elm.json`,
  are marked with `NEW` and listed at end of each section.

### Selected versions

- Selected version of each package is shown with blue background. To change selected version, click on any other version.
- If there are any dependency conflicts with selected versions, those are listed under the table.
- If there are no conflicts with selected versions,
  then any non-selected version which is incompatible with selected versions, will be shown with red background.

### Incompatible versions

- When hovering cursor over any package version,
  all versions which are incompatible with that one will be marked with red border.

## Limitations

- Only application `elm.json` can be analyzed, not package `elm.json`.
  - If you would find it useful to also support package `elm.json`, please open an issue.
- Moving packages between normal/test dependencies is not fully supported yet.

## Building

1) [Install Elm 0.19](https://guide.elm-lang.org/install.html) 
2) Run `elm make src/*.elm --optimize --output=elm.js` in project base directory.
   - Optionally you can also run minifier like `uglifyjs`.
     See included `make.sh` for some building options.
     (This is just a generic script for running `elm make`, not specific to this project.)
3) Open `index.html` in browser.

### Backend cache

- `cacheUrl` in `Backend.elm` sets the backend cache to be used.
- My cache at `https://www.elm-dependencies-analyzer.net/backend/cache.php?` is available
  publicly for now. I intend to keep this cache public, but if I start getting too much traffic,
  I could make it private.
- The backend script is included in this repository as `cache.php`, so you could also just run your own cache.

### Backend error reporting

- `logErrorUrl` in `Backend.elm` enables/disables automatic error reporting,
  and sets the URL to be used when enabled.
- For custom builds this should be set to either `Nothing` (disabled) or to your own URL.
   - (Error reports from custom builds aren't that useful to me,
     as I can't know what has been changed in code.)
