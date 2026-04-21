source("R/config/packages.R")

source("R/9999_functions.R")
for (function_file in sort(list.files("R/functions", pattern = "\\.R$", full.names = TRUE))) {
  source(function_file)
}

source("R/config/paths.R")

fname <- function(x) {
  basename(tools::file_path_sans_ext(x))
}

source("R/config/theme.R")
source("R/config/reference_tables.R")
