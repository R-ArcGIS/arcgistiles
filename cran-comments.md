## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes for the reviewer

This is a new submission.

Examples that reach ArcGIS services are guarded with
`@examplesIf curl::has_internet()` and run against public services that need no
authentication. Examples for the asynchronous tile export operations are wrapped
in `\dontrun{}` because they submit a server side job and require a token.
