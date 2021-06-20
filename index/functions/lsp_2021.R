
LSP <- function(layers){

  ## From code in 'functions.R LSP' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  ## Status ----

  lsp_status <- ohicore::AlignDataYears(layer_nm="lsp_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = , score, dimension) %>%
    dplyr::mutate(region_id = paste("BHI", stringr::str_pad(region_id, 3, "left", 0), sep = "-"))


  ## Trend ----

  ## for now have NA Lasting special places trends...
  lsp_trend <- data.frame(
    region_id = paste("BHI", stringr::str_pad(c(1:3, 5:43), 3, "left", 0), sep = "-"),
    score = rep(NA, 42)
  )


  ## Return lasting special places status and trend scores ----

  lsp_status_and_trend <- dplyr::bind_rows(
    mutate(lsp_status, goal = "LSP"),
    mutate(lsp_trend, goal = "LSP",  dimension = "trend")
  )
  scores <- select(lsp_status_and_trend, region_id, goal, dimension, score)

  return(scores)

} ## End LSP function
