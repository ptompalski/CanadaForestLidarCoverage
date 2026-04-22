# targets example

This branch has a small `targets` example in `_targets.R`.

It does not replace `R/__RUN_UPDATES.R`. The example starts with one part of the
workflow: it uses the existing pre-processed jurisdiction layers and runs
`R/1100_combineAll_noOverlaps.R` with output paths redirected to
`scratch/targets-example`.

Useful commands:

```r
install.packages("targets")

targets::tar_manifest()
targets::tar_make()
targets::tar_read(combined_coverage_outputs)
targets::tar_outdated()
```

The main idea is that `targets` tracks the input files and output files. If the
pre-processed layers do not change, the combined coverage step can be skipped on
the next run.

To clean the local `targets` cache:

```r
targets::tar_destroy()
```
