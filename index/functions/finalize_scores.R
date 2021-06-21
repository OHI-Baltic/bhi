
FinalizeScores <- function(layers, conf, scores){

  ## From code in 'functions.R FinalizeScores' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## modified from functions.R template to aggregte to EEZs and Subbasins of Baltic

  ## resilience and pressures for supragoals??
  supragoal_prs_res <- scores %>%
    dplyr::left_join(conf$goals, by = "goal") %>%
    dplyr::select("region_id", "dimension", "goal", "score", "parent", "weight") %>%
    dplyr::filter(!(is.na(parent)|parent == "NA"), dimension %in% c("pressures", "resilience")) %>%
    dplyr::group_by(region_id, parent, dimension) %>%
    dplyr::summarize(score = weighted.mean(score, weight)) %>%
    dplyr::select(region_id, dimension, goal = parent, score)

  scores <- dplyr::bind_rows(scores, supragoal_prs_res)


  ## Calculate Scores for EEZs and SUBBASINS by area weighting ----

  ## regions_lookup_complete.csv does not need to be updated
  ## unless BHI regions are changed or additional subregions are created
  rgns_complete <- read.csv(file.path(dir_assess, "layers", "rgns_complete.csv")) %>%
    dplyr::mutate(eez_id = case_when(
      eez == "Sweden" ~ "BHI-301",
      eez == "Denmark" ~ "BHI-302",
      eez == "Germany" ~ "BHI-303",
      eez == "Poland" ~ "BHI-304",
      eez == "Russia" ~ "BHI-305",
      eez == "Lithuania" ~ "BHI-306",
      eez == "Latvia" ~ "BHI-307",
      eez == "Estonia" ~ "BHI-308",
      eez == "Finland" ~ "BHI-309"
    )) %>%
    dplyr::mutate(sea = "BHI-000") %>%
    dplyr::mutate(subbasin_id = paste("BHI", subbasin_id, sep = "-"))

  cat(sprintf("Calculating scores for EEZ and SUBBASIN AREAS by area weighting...\n"))


  ## For EEZs ----
  scores_eez <- scores %>%
    dplyr::filter(region_id %in% paste0(
      "BHI-", stringr::str_pad(c(1:3, 5:43), 3, "left", 0)
    )) %>%

    ## merge to the area (km2) of each region
    dplyr::left_join(rgns_complete, by = "region_id") %>%
    dplyr::group_by(goal, dimension, eez_id) %>%

    ## calculate weighted mean by area
    dplyr::summarise(score = weighted.mean(score, region_area_km2, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(score = ifelse(is.nan(score), NA, score)) %>%

    ## select eez ids
    dplyr::select(goal, dimension, score, region_id = eez_id)


  ## For SUBBASINS ----
  scores_subbasin <- scores %>%
    dplyr::filter(region_id %in% paste0(
      "BHI-", stringr::str_pad(c(1:3, 5:43), 3, "left", 0)
    )) %>%

    ## merge to the area (km2) of each region
    dplyr::left_join(rgns_complete, by = "region_id") %>%
    dplyr::group_by(goal, dimension, subbasin_id) %>%

    ## calculate weighted mean by area
    dplyr::summarise(score = weighted.mean(score, region_area_km2, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(score = ifelse(is.nan(score), NA, score)) %>%

    ## select subbasin ids
    dplyr::select(goal, dimension, score, region_id = subbasin_id)


  ## For Full BALTIC SEA ----
  scores_baltic <- scores %>%
    dplyr::filter(region_id %in% paste0(
      "BHI-", stringr::str_pad(c(1:3, 5:43), 3, "left", 0)
    )) %>%

    ## merge to the area (km2) of each region
    dplyr::left_join(rgns_complete, by = "region_id") %>%
    dplyr::group_by(goal, dimension, sea) %>%

    ## calculate weighted mean by area
    dplyr::summarise(score = weighted.mean(score, region_area_km2, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(score = ifelse(is.nan(score), NA, score)) %>%

    ## select subbasin ids
    dplyr::select(goal, dimension, score, region_id = sea)


  ## combine scores with EEZ and SUBBASIN scores ----
  scores <- dplyr::bind_rows(
    scores %>%
      dplyr::filter(region_id %in% paste0(
        "BHI-", stringr::str_pad(c(1:3, 5:43), 3, "left", 0)
      )),
    scores_eez, scores_subbasin, scores_baltic
  )


  ## Add NAs to missing combos of region_id, goal, dimension ----
  allrgns <- paste0("BHI-", c(stringr::str_pad(c(0:3, 5:43), 3, "left", 0), 301:309, 501:517))
  explst <- list(
    region_id = allrgns,
    dimension = c("pressures", "resilience", "status", "trend", "future", "score"),
    goal = c(conf$goals$goal, "Index")
  )
  d <- expand.grid(explst, stringsAsFactors = FALSE)


  ## RETURN SCORES ----
  ## merge NAs dataframe with scores, and arrange
  scores <- dplyr::left_join(d, scores, by = c("goal",  "dimension", "region_id")) %>%
    dplyr::arrange(goal, dimension, region_id) %>%
    dplyr::mutate(score = ifelse(dimension == "trend", round(score, 3), round(score, 1))) %>%
    dplyr::mutate(score = ifelse(is.nan(score), NA, score))


  return(scores)

} ## End FinalizeScores function
