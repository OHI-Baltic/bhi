
ECO <- function(layers){

  ## From code in 'functions.R ECO' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  growth_rates <- AlignDataYears(layer_nm="le_eco_bluegrowth_rates", layers_obj=layers) %>%
    select(year = scenario_year, region_id, sector, annual_growth_rate)
  gva <- AlignDataYears(layer_nm="le_eco_yearly_gva", layers_obj=layers) %>%
    select(year = scenario_year, region_id, sector, gva_sector_prop, country_blueecon_gva)

  # growth_rates <- read.csv(here::here("index", "layers", "le_eco_bluegrowth_rates_bhi2021.csv"))
  # gva <- read.csv(here::here("index", "layers", "le_eco_yearly_gva_bhi2021.csv"))

  eco_status <- full_join(growth_rates, gva, by = c("region_id", "sector", "year")) %>%
    ## try smoothing growth rate data first with 3-year rolling average
    arrange(region_id, sector, year) %>%
    group_by(region_id, sector) %>%
    mutate(ma3yr = zoo::rollapply(annual_growth_rate, 3, mean, na.rm = TRUE, align = "right", fill = NA)) %>%
    ungroup() %>%
    mutate(sector_status = case_when(
      ma3yr <= -1.5 ~ 0,
      ma3yr > -1.5 & ma3yr < 1.5 ~ 100/3*ma3yr + 50,
      ma3yr >= -1.5 ~ 100
    )) %>%

    ## calculate status first by sector,
    ## then average scores across sectors proportional to contribution to marine gva
    # mutate(sector_status = case_when(
    #   annual_growth_rate <= -1.5 ~ 0,
    #   annual_growth_rate > -1.5 & annual_growth_rate < 1.5 ~ 100/3*annual_growth_rate + 50,
    #   annual_growth_rate >= -1.5 ~ 100
    # )) %>%
    group_by(region_id, year) %>%
    summarize(status = weighted.mean(sector_status, gva_sector_prop, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(status = ifelse(is.nan(status), NA, status))


  ## TREND ----

  trend_years <- (scen_year-4):scen_year

  eco_trend <- eco_status %>%
    dplyr::group_by(region_id) %>%
    dplyr::filter(year %in% trend_years) %>%
    dplyr::do(mdl = lm(status ~ year, data = .)) %>%
    dplyr::summarize(
      region_id = region_id,
      trend = coef(mdl)["year"] * 5
    ) %>% # trend multiplied by 5 to predict 5 yrs out
    dplyr::ungroup() %>%
    dplyr::mutate(trend = round(trend, 2))


  ## RETURN SCORES ----

  scores <- rbind(
    ## status scores
    eco_status %>%
      dplyr::filter(year == scen_year) %>%
      dplyr::select(region_id, score = status) %>%
      dplyr::mutate(dimension = "status", goal = "ECO"),
    ## trend scores
    eco_trend %>%
      dplyr::select(region_id, score = trend) %>%
      dplyr::mutate(dimension = "trend", goal = "ECO")
  )

  return(scores)

} ## End ECO function
