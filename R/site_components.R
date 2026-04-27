library(htmltools)

get_latest_coverage_date <- function() {
  coverage_info <- fs::dir_info("layers/ALS_coverage_layer/main", recurse = FALSE) |>
    dplyr::filter(grepl("^ALS_coverage_all_.*\\.rds$", basename(path))) |>
    dplyr::arrange(desc(modification_time)) |>
    dplyr::slice(1)

  format(as.Date(coverage_info$modification_time[[1]]), "%B %d, %Y")
}

site_footer <- function(coverage_file_date = NULL) {
  if (is.null(coverage_file_date)) {
    coverage_file_date <- get_latest_coverage_date()
  }

  htmltools::browsable(
    htmltools::tags$section(
      class = "column-screen homepage-footer-wrap",
      htmltools::tags$footer(
        class = "homepage-footer",
        htmltools::tags$div(
          class = "homepage-footer-inner",
          htmltools::tags$div(
            htmltools::tags$p(class = "homepage-footer-title", "ALS data coverage in Canadian forests"),
            htmltools::tags$p(class = "homepage-footer-update", paste("Last coverage update:", coverage_file_date))
          ),
          htmltools::tags$nav(
            class = "homepage-footer-links",
            htmltools::tags$a(href = "https://github.com/ptompalski/CanadaForestLidarCoverage", "GitHub repository"),
            htmltools::tags$a(href = "log.html", "Update log"),
            htmltools::tags$a(href = "contact.html", "Contact")
          )
        )
      )
    )
  )
}

site_header <- function(home = FALSE) {
  header_class <- if (isTRUE(home)) "homepage-fixed-header" else "homepage-fixed-header is-scrolled"

  htmltools::browsable(
    htmltools::tags$header(
      class = header_class,
      htmltools::tags$div(
        class = "homepage-fixed-header-inner",
        htmltools::tags$a(
          href = if (isTRUE(home)) "#overview" else "index.html",
          class = "homepage-site-mark",
          htmltools::tags$span("Canada Forest ALS")
        ),
        htmltools::tags$nav(
          class = "homepage-topnav",
          htmltools::tags$a(href = if (isTRUE(home)) "#interactive-map" else "index.html#interactive-map", "Map"),
          htmltools::tags$a(href = if (isTRUE(home)) "#summary" else "index.html#summary", "Summary"),
          htmltools::tags$a(href = if (isTRUE(home)) "#publication" else "index.html#publication", "Publication"),
          htmltools::tags$a(href = if (isTRUE(home)) "#data-sources" else "index.html#data-sources", "Data sources"),
          htmltools::tags$a(href = "contact.html", "Contact"),
          htmltools::tags$a(
            href = "https://github.com/ptompalski/CanadaForestLidarCoverage",
            class = "homepage-topnav-iconlink",
            `aria-label` = "GitHub repository",
            title = "GitHub repository",
            htmltools::HTML('<svg viewBox="0 0 16 16" aria-hidden="true" focusable="false"><path fill="currentColor" d="M8 0C3.58 0 0 3.58 0 8a8 8 0 0 0 5.47 7.59c.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.5-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82a7.5 7.5 0 0 1 4 0c1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8 8 0 0 0 16 8c0-4.42-3.58-8-8-8Z"></path></svg>')
          )
        )
      )
    )
  )
}
