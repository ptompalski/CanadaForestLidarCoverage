# RUN updates

source("R/0000_setup.R")

# initial preprocessing, by jurisdiction. Need to run when data updated.
source("R/1001_preprocess_AB.R")
source("R/1001_preprocess_BC.R")
source("R/1001_preprocess_NB.R")
source("R/1001_preprocess_NS.R")
source("R/1001_preprocess_ON.R")
source("R/1001_preprocess_PEI.R")
source("R/1001_preprocess_QC_v2.R")
source("R/1001_preprocess_SK.R")


# process data
source("R/1100_combineAll_noOverlaps.R")
source("R/1500_combineALL_withOverlaps.R")

# generate maps, tables, graphics...
source("R/2000_maps_setup.R")
source("R/2000_maps_main.R")
source("R/2000_maps_focused.R")
source("R/2000_maps_updateLog.R")
source("R/2000_maps_animation.R")
source("R/3001_coverageManagedUnmanaged.R")
source("R/3001_theTable.R")
source("R/3001_theTable_v2.R")
source("R/3003_coverageMultiTemporal.R")
source("R/3004_acquisition_area_over_time.R")


library(quarto)
map(list.files(pattern = ".qmd$"), quarto_render)
quarto_render("log.qmd")
quarto::quarto_render("index.qmd")
quarto::quarto_render("data.qmd")
quarto::quarto_render("multitemporal.qmd")
quarto::quarto_render("test.qmd")
