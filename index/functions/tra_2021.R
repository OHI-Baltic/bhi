
TRA <- function(layers){

  ## From code in 'functions.R TRA' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  ## Status ----

  tra_status <- ohicore::AlignDataYears(layer_nm="cw_tra_status", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::select(region_id = bhi_id, score, dimension) %>%
    dplyr::mutate(region_id = paste("BHI", stringr::str_pad(region_id, 3, "left", 0), sep = "-"))


  ## Trend ----

  tra_trend <- read.csv(file.path(dir_assess, "layers", "cw_trash_trend_bhi2021.csv")) %>%
    dplyr::mutate(dimension = "trend") %>%
    dplyr::select(bhi_id, score = future_trend, dimension) %>%
    dplyr::mutate(region_id = paste("BHI", stringr::str_pad(bhi_id, 3, "left", 0), sep = "-"))


  ## Return trash status and trend scores ----

  tra_status_and_trend <- dplyr::bind_rows(
    mutate(tra_status, dimension = "status", goal = "TRA"),
    mutate(tra_trend, dimension = "trend", goal = "TRA")
  )

  scores <- select(tra_status_and_trend, region_id, goal, dimension, score)

  return(scores)

} ## End TRA function
