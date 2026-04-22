# targets example

This branch has a `targets` example in `_targets.R`.

It does not replace `R/__RUN_UPDATES.R` yet. The example defines the full update
workflow as staged targets:

- preprocessing
- combined coverage and multitemporal processing
- maps and tables
- Quarto website rendering

The dated output version is controlled explicitly with `COVERAGE_VERSION`.

Useful commands:

```r
install.packages("targets")

Sys.setenv(COVERAGE_VERSION = "20260421")
targets::tar_manifest()
targets::tar_make()
targets::tar_read(processing)
targets::tar_outdated()
```

You can also source `R/__RUN_TARGETS_EXAMPLE.R`. Edit the `coverage_version`
value in that script, or set `COVERAGE_VERSION` first.

The main idea is that `targets` tracks scripts, input files, output files, and
the version parameter. If none of those change, completed stages can be skipped
on the next run.

To clean the local `targets` cache:

```r
targets::tar_destroy()
```
