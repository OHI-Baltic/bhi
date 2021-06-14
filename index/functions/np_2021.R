
NP <- function(layers){

  ## From code in 'functions.R NP' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## this similar to the function as used for FIS,
  ## but as only sprat data is used here, FIS weighting by catch step is eliminated
  ## consider updating for v2022 to include: sediment extraction, seaweeds, ornamentals, corals, shells, sponges?

  ## OHI global includes exposure, risk, sustainability estimates by product:
  ## exposure (based on habitat types), risk (from fishing activities harmfulness ratings)
  ## sustainability coefficient for each natural product: 1 - mean(c(exposure, risk))
  ## status for all production years for each region, a weighted mean of all products produced

  scen_year <- layers$data$scenario_year


  bbmsy <- AlignDataYears(layer_nm="np_bbmsy", layers_obj=layers) %>%
    dplyr::mutate(metric="bbmsy")

  ffmsy <- AlignDataYears(layer_nm="np_ffmsy", layers_obj=layers) %>%
    dplyr::mutate(metric="ffmsy")

  landings <- AlignDataYears(layer_nm="np_catch", layers_obj=layers)


  # bbmsy <- read.csv(here::here("index", "layers", "np_bbmsy_bhi2021.csv"))
  # bbmsy <- read.csv(here::here("index", "layers", "np_bbmsy_2xtrigger_bhi2021.csv"))
  # ffmsy <- read.csv(here::here("index", "layers", "np_ffmsy_bhi2021.csv"))
  # metric_scores <- rbind(
  #   mutate(bbmsy, metric = "bbmsy"),
  #   mutate(ffmsy, metric = "ffmsy")
  # )


  ## combine bbmsy and ffmsy into single object
  metric_scores <- rbind(
    dplyr::select(bbmsy, -np_bbmsy_year, -layer_name, year = scenario_year),
    dplyr::select(ffmsy, -np_ffmsy_year, -layer_name, year = scenario_year)
  )
  metric_scores <- metric_scores %>%
    dplyr::mutate(metric = as.factor(metric)) %>%
    tidyr::pivot_wider(names_from = metric, values_from = value)


  ## BSCORES
  ## STEP 1: converting B/Bmsy and F/Fmsy to B-scores ----
  ## note: these parameters can be changed (see data prep for exploration)!
  lowerB <- 0.95
  upperB1 <- 1.3
  upperB2 <- 5

  B_scores <- metric_scores %>%
    mutate(
      score_type = "B_score",

      score = ifelse(
        bbmsy < lowerB, (1/lowerB)*bbmsy,
        ifelse(
          lowerB <= bbmsy & bbmsy < upperB1, 1,
          ifelse(
            bbmsy >= upperB1,
            (bbmsy - upperB2)/(upperB1-upperB2), NA
          )
        )
      )
    ) %>%
    mutate(
      score = ifelse(
        score <= 0.1, 0.1,
        ifelse(score > 1, 1, score)
      )
    )

  ## FSCORES
  ## STEP 2: converting B/Bmsy and F/Fmsy to F-scores ----

  lowerF <- 0.5
  upperF1 <- 1
  upperF2 <- 4.5
  m <- (upperF2-1)/lowerB

  # install.packages("pracma")
  norm1 = pracma::cross(
    c(lowerB, upperF1, 1) - c(0, 1, 0),
    c(lowerB, upperF2, 0) - c(0, 1, 0)
  )
  norm2 = pracma::cross(
    c(lowerB, 0, (upperF2-lowerF-1)/(upperF2-1)) - c(lowerB, lowerF, 1),
    c(0, 1-(upperF2-lowerF), 1) - c(lowerB, lowerF, 1)
  )

  F_scores <- metric_scores %>%
    mutate(
      score_type = "F_score",

      score = ifelse(
        ## when bbmsy < lowerB :
        bbmsy < lowerB,

        ## will be space near zero where scores start going back down from 1:
        ## on y-axis towards zero if upperF2-lowerF < 1, on x-axis towards zero if upperF2-upperF1 > 1
        ifelse(
          ffmsy > m*bbmsy + 1, 0,
          ifelse(
            m*bbmsy + 1 >= ffmsy & ffmsy > m*bbmsy + (1-(upperF2-upperF1)),
            ## http://mathworld.wolfram.com/Plane.html n1x + n2y + n3z - (n1x0 + n2y0 + n3z0) = 0
            (norm1[2] - norm1[1]*bbmsy - norm1[2]*ffmsy)/norm1[3],
            ifelse(
              m*bbmsy + (1-(upperF2-upperF1)) >= ffmsy & ffmsy > m*bbmsy + (1-(upperF2-lowerF)), 1,
              ((norm2[1]*lowerB + norm2[2]*lowerF + norm2[3]) - (norm2[1]*bbmsy + norm2[2]*ffmsy))/norm2[3]
            )
          )
        ),
        ## when bbmsy >= lowerB :
        ifelse(
          ffmsy > upperF1,
          (upperF2-ffmsy)/(upperF2-upperF1),
          ifelse(
            upperF1 >= ffmsy & ffmsy > lowerF,
            1,
            ffmsy/(upperF2-1) + (upperF2-lowerF-1)/(upperF2-1)
          )
        )
      )
    ) %>%
    ## set scores less than 0.1 to 0.1, greater than 1 to 1
    ## 0.1 rather than zero because one zero w/ geometric mean results in a zero score
    mutate(
      score = ifelse(
        score <= 0.1, 0.1,
        ifelse(score > 1, 1, score)
      )
    )


  ## STEP 3: averaging the F and B-scores to get the stock status score ----

  status <- rbind(B_scores, F_scores) %>%
    group_by(region_id, stock, year) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>% # hist(scores$score)
    left_join(
      spread(rbind(B_scores, F_scores), key = "score_type", value = "score"),
      by = c("year", "stock", "region_id")
    ) %>%
    filter(!is.na(score)) %>% # remove missing data
    data.frame()


  ## STEP 5: calculate trend and return score results ----

  ## trend calculations
  ## could replace trend calc w/ ohicore::CalculateTrend, but then coeff is normalized by min trend year...
  ## need more years of data in scenario_data_years though for this to work
  # trend <- ohicore::CalculateTrend(status, trend_years)

  trend_years <- (max(status$year)-4):max(status$year)

  trend <- status %>%
    dplyr::group_by(region_id) %>%
    dplyr::filter(year %in% trend_years) %>%
    dplyr::do(mdl = lm(score ~ year, data = .)) %>%
    dplyr::summarize(
      region_id = region_id,
      trend = coef(mdl)["year"] * 5
    ) %>% # trend multiplied by 5 to predict 5 yrs out
    dplyr::ungroup() %>%
    dplyr::mutate(trend = round(trend, 2))

  scores <- rbind(
    ## status scores
    status %>%
      dplyr::filter(year == scen_year) %>%
      dplyr::mutate(status = round(score * 100, 1)) %>%
      dplyr::select(region_id, score = status) %>%
      tidyr::complete(region_id = paste0(
        "BHI-",
        stringr::str_pad(c(1:3, 5:43), 3, "left", 0)
      )) %>%
      dplyr::mutate(dimension = "status"),

    ## trend scores
    trend %>%
      dplyr::select(region_id, score = trend) %>%
      tidyr::complete(region_id = paste0(
        "BHI-",
        stringr::str_pad(c(1:3, 5:43), 3, "left", 0)
      )) %>%
      dplyr::mutate(dimension = "trend")

  ) %>% dplyr::mutate(goal = "NP")

  return(scores)

} ## End NP function
