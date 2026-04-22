# RUN updates

source("R/0000_setup.R")

preprocess_scripts <- c(
    "R/1001_preprocess_AB.R",
    "R/1001_preprocess_BC.R",
    "R/1001_preprocess_NB.R",
    "R/1001_preprocess_NS.R",
    "R/1001_preprocess_ON.R",
    "R/1001_preprocess_PEI.R",
    "R/1001_preprocess_QC_v2.R",
    "R/1001_preprocess_SK.R"
)

processing_scripts <- c(
    "R/1100_combineAll_noOverlaps.R",
    "R/1500_combineALL_withOverlaps.R"
)

map_table_scripts <- c(
    "R/2000_maps_setup.R",
    "R/2000_maps_main.R",
    "R/2000_maps_focused.R",
    "R/2000_maps_updateLog.R",
    "R/2000_maps_animation.R",
    "R/3001_coverageManagedUnmanaged.R",
    "R/3001_theTable.R",
    "R/3001_theTable_v2.R",
    "R/3003_coverageMultiTemporal.R",
    "R/3004_acquisition_area_over_time.R"
)

# initial preprocessing, by jurisdiction. Need to run when data updated.
source_scripts(preprocess_scripts)

# process data
source_scripts(processing_scripts)

# generate maps, tables, graphics...
source_scripts(map_table_scripts)

library(quarto)
map(list.files(pattern = ".qmd$"), quarto_render)
# quarto_render("log.qmd")
# quarto::quarto_render("index.qmd")
# quarto::quarto_render("data.qmd")
# quarto::quarto_render("multitemporal.qmd")
# quarto::quarto_render("test.qmd")
