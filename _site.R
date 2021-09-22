## Libraries ----
## load any libraries needed across website pages
suppressPackageStartupMessages({
  library(readr)
  library(magrittr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(knitr)
  library(httr)
  library(geojsonio)
  library(leaflet)
  library(formattable)
})


## General ----

## knitr options for all webpages
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

assess_year <- 2021 # CHANGE BHI ASSESSMENT YEAR HERE!
dir_assess <- "index" # SET BHI ASSESSMENT FOLDER HERE!


bhi_gh <- "https://github.com/OHI-Baltic/bhi"
bhiprep_gh <- "https://github.com/OHI-Baltic/bhi-prep"

bhi_gh_raw <- stringr::str_replace(
  bhi_gh,
  pattern = "github.com",
  replacement = "raw.githubusercontent.com"
)
bhiprep_gh_raw <- stringr::str_replace(
  bhiprep_gh,
  pattern = "github.com",
  replacement = "raw.githubusercontent.com"
)
