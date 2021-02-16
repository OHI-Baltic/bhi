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
  library(timevis)
  library(widgetframe)
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

# bhi_gh_api <- stringr::str_replace(
#   bhi_gh,
#   pattern = "github.com",
#   replacement = "api.github.com/repos"
# ) %>% paste(
#   "git/trees/master?recursive=1", sep = "/"
# )
# bhiprep_gh_api <- stringr::str_replace(
#   bhiprep_gh,
#   pattern = "github.com",
#   replacement = "api.github.com/repos"
# ) %>% paste(
#   "git/trees/master?recursive=1", sep = "/"
# )


## read in variables if they exist
scores_csv <- file.path(bhi_gh_raw, "master", "index", "scores.csv")
layers_csv <- file.path(bhi_gh_raw, "master", "index",  "layers.csv")
conf_csv <- file.path(bhi_gh_raw, "master", "index", "conf", "goals.csv")

if(RCurl::url.exists(scores_csv)){
  scores <- readr::read_csv(scores_csv)
}
if(RCurl::url.exists(layers_csv)){
  layers <- readr::read_csv(layers_csv)
}
if(RCurl::url.exists(conf_csv)){
  weight <- readr::read_csv(conf_csv) %>%
    select(goal, weight)
}
