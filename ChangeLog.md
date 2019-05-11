# Changelog for muse

## Unreleased changes
### Fixed 
- correct application of headword predicates: now each predicate must be
  satisified by at least one headword in order for the definition entry to
  number amongst the search results.

## [0.2.0] - 2019-03-13
### Added
- new subcommand "lastRead" which pretty prints the most recent "Read" entry
- add option "lastRead --suppress-newline" to omit trailing newline for use in
  vim script
- conform better to semver--past versions will not be altered, but in future
  additions new subcommands will warrant a MINOR version increment. Forgive
  past violations.

### Fixed
- `reldur` parser now takes all combinations and omissions of year, month, day;
  e.g., '3d', '2y1d', and '3d2m' used not to succeed.

## [0.1.5] - 2019-03-06
### Added
- `--version` switch
- fix quotation parser bug which caused greedy whitespace consumption; now
  indentation-based attribution tagging is (more) reliable
- due to the above, auto-attribution of quotes is functional--it's no longer
  necessary to manually attribute quotes that are children of an attributed
  "read" entry 

## [0.1.4] - 2018-12-17
### Added
- acid-state persistence
- quote, dump, dialogue, and commentary body search
- def and phrase headword and meaning search
- auto-tag nested entries, making optional quote attribution in reading
  session
- WIP CLI to new search options
- test coverage for db filters
- timestamped, and as yet rather ugly, pretty printed output 

### Removed
- file-system based persistence
- default chronological output

