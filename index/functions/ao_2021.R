
AO <- function(layers){

  ## From code in 'functions.R AO' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  ## Status ----

  ao_status <- ohicore::AlignDataYears(layer_nm="ao_stock_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(region_id = paste(
      "BHI", stringr::str_pad(bhi_id, 3, "left", 0), sep = "-"
    )) %>%
    dplyr::select(region_id, score)


  ## Trend ----

  ao_trend <- ohicore::AlignDataYears(layer_nm="ao_stock_slope", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(region_id = paste(
      "BHI", stringr::str_pad(bhi_id, 3, "left", 0), sep = "-"
    )) %>%
    dplyr::select(region_id, score)


  ## Return artisial fishing opportunities status and trend scores ----

  ao_status_and_trend <- dplyr::bind_rows(
    ao_status %>%
      tidyr::complete(region_id = paste0(
        "BHI-", stringr::str_pad(c(1:3, 5:43), 3, "left", 0)
      )) %>%
      dplyr::mutate(dimension = "status", goal = "AO"),
    ao_trend %>%
      tidyr::complete(region_id = paste0(
        "BHI-", stringr::str_pad(c(1:3, 5:43), 3, "left", 0)
      )) %>%
      dplyr::mutate(dimension = "trend", goal = "AO")
  )
  scores <- select(ao_status_and_trend, region_id, goal, dimension, score)

  return(scores)

} ## End AO function
