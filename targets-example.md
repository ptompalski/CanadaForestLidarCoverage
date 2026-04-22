# targets example

This branch has a `targets` example in `_targets.R`.

The old sequential update runner has been moved to `R/archive/__RUN_UPDATES.R`.
The current update workflow is defined as staged targets:

- online source checks and downloads
- preprocessing
- combined coverage and multitemporal processing
- maps and tables
- Quarto website rendering

The dated output version is controlled explicitly with `COVERAGE_VERSION`.
The New Brunswick lidar index is checked online using HTTP headers from the
GeoNB SHP ZIP download. The ZIP is downloaded and unzipped only when the remote
`ETag`, `Last-Modified`, or `Content-Length` changes.

Useful commands:

```r
install.packages("targets")

Sys.setenv(COVERAGE_VERSION = "20260421")
targets::tar_manifest()
targets::tar_make()
targets::tar_read(processing)
targets::tar_outdated()
```

You can also source `R/__RUN_TARGETS.R`. Edit the `coverage_version` value in
that script, or set `COVERAGE_VERSION` first.

The main idea is that `targets` tracks scripts, input files, output files, and
the version parameter. If none of those change, completed stages can be skipped
on the next run.

To clean the local `targets` cache:

```r
targets::tar_destroy()
```
