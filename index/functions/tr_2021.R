
TR <- function(layers){

  ## From code in 'functions.R TR' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## scores calculated in prep file (bhi-prep repository, data and prep TR subfolders)
  ## possible alternative: base metric on EU blue growth reports?

  scen_year <- layers$data$scenario_year


  tr_accomm <- ohicore::AlignDataYears(layer_nm="tr_accommodations", layers_obj=layers) %>%
    dplyr::select(year = scenario_year, region_id, accom_per_area)
  ## remove region 4 (avg accom already same across regions 4 and 8)
  ## assign same accommodations per area for regions 36 and 43, for this assessment...
  tr_accomm <- tr_accomm %>%
    dplyr::filter(region_id == 36) %>%
    dplyr::mutate(region_id = 43) %>%
    dplyr::bind_rows(filter(tr_accomm, region_id != 4))


  tr_gva <- ohicore::AlignDataYears(layer_nm="tr_coastal_tourism_gva", layers_obj=layers) %>%
    dplyr::select(year = scenario_year, region_id, cntry_tourism_gva)
  ## country tourism gva same for regions 36 and 43, just filter to remove region 4
  tr_gva <- tr_gva %>%
    dplyr::filter(region_id == 36) %>%
    dplyr::mutate(region_id = 43) %>%
    dplyr::bind_rows(filter(tr_gva, region_id != 4))


  coastal_tourism_value <- full_join(tr_accomm, tr_gva, by = c("year", "region_id")) %>%
    mutate(tourism_gva_per_accom = cntry_tourism_gva/accom_per_area)


  ## status ----

  ## scaling so that lower point is fixed
  ## max score of 1 corresponds to max ratio of revenue to coastal accomm. per area, across all data
  mn <- min(coastal_tourism_value$tourism_gva_per_accom, na.rm = TRUE)
  mx <- max(coastal_tourism_value$tourism_gva_per_accom, na.rm = TRUE)

  tr_status <- coastal_tourism_value %>%
    mutate(status = ((1-mn)/(mx-mn))*tourism_gva_per_accom + (1 - (1-mn)/(mx-mn)*mx))


  ## trend ----

  trendyrs <- (scen_year-4):scen_year

  tr_trend <- tr_status %>%
    filter(year %in% trendyrs) %>%
    group_by(region_id) %>%
    do(trend_mdl = lm(status ~ year, data = .)) %>%
    mutate(expectedchange5yrs = coef(trend_mdl)["year"]*5)


  ## return scores ----

  scores <- dplyr::bind_rows(
    tr_status %>%
      filter(year == scen_year) %>%
      mutate(score = status*100) %>%
      select(region_id, score) %>%
      dplyr::mutate(region_id = paste(
        "BHI", stringr::str_pad(region_id, 3, "left", 0), sep = "-"
      )) %>%
      tidyr::complete(region_id = paste0(
        "BHI-",
        stringr::str_pad(c(1:3, 5:43), 3, "left", 0)
      )) %>%
      mutate(dimension = "status", goal = "TR"),
    tr_trend %>%
      rename(score = expectedchange5yrs) %>%
      select(region_id, score) %>%
      dplyr::mutate(region_id = paste(
        "BHI", stringr::str_pad(region_id, 3, "left", 0), sep = "-"
      )) %>%
      tidyr::complete(region_id = paste0(
        "BHI-",
        stringr::str_pad(c(1:3, 5:43), 3, "left", 0)
      )) %>%
      mutate(dimension = "trend", goal = "TR")
  )

  return(scores)

} ## end TR function


