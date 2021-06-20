
MAR <- function(layers){

  ## From code in 'functions.R MAR' of v2015 BHI assessment, see bhi-1.0-archive github repo

  scen_year <- layers$data$scenario_year

  mar_status <- data.frame(
    region_id = paste0("BHI-", stringr::str_pad(c(1:3, 5:43), 3, "left", 0)),
    goal = "MAR",
    dimension = "status",
    score = rep(NA, 42)
  )

  mar_trend <- data.frame(
    region_id = paste0("BHI-", stringr::str_pad(c(1:3, 5:43), 3, "left", 0)),
    goal = "MAR",
    dimension = "trend",
    score = rep(NA, 42)
  )

  scores <- dplyr::bind_rows(mar_status, mar_trend)

  return(scores)


} # End MAR function
