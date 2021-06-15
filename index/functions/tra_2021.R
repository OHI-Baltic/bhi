
TRA <- function(layers){

  ## From code in 'functions.R TRA' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  ## Status ----

  tra_status <- ohicore::AlignDataYears(layer_nm="cw_tra_status", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score)

  # tra_status <- read.csv(here::here("index", "layers", "_bhi2021.csv")) %>%
  #   dplyr::mutate(dimension = as.character(dimension)) %>%
  #   dplyr::select(region_id = bhi_id, score, dimension) %>%
  #   dplyr::mutate(region_id = paste("BHI", stringr::str_pad(region_id, 3, "left", 0), sep = "-"))


  ## Trend ----

  tra_trend <- ohicore::AlignDataYears(layer_nm="cw_tra_trend_scores", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score)

  # tra_trend <- read.csv(here::here("index", "layers", "_bhi2021.csv")) %>%
  #   dplyr::mutate(dimension = as.character(dimension)) %>%
  #   dplyr::select(region_id = bhi_id, score, dimension) %>%
  #   dplyr::mutate(region_id = paste("BHI", stringr::str_pad(region_id, 3, "left", 0), sep = "-"))

  ## Return trash status and trend scores ----

  tra_status_and_trend <- dplyr::bind_rows(
    mutate(tra_status, dimension = "status", goal = "TRA"),
    mutate(tra_trend, dimension = "trend", goal = "TRA")
  )

  scores <- select(tra_status_and_trend, region_id, goal, dimension, score)

  return(scores)

} ## End TRA function
