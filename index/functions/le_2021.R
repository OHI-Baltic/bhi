
LE <- function(layers, scores){

  ## From code in 'functions.R LE' of v2015 BHI assessment, see bhi-1.0-archive github repo

  scen_year <- layers$data$scenario_year

  le_scores <- scores %>%
    dplyr::filter(
      goal %in% c("LIV", "ECO"),
      dimension %in% c("status", "trend", "future", "score")
    ) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::mutate(goal = "LE") %>%
    dplyr::ungroup() %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    dplyr::mutate(score = ifelse(region_id %in% c("BHI-019", "BHI-022", "BHI-030", "BHI-033"), NA, score))

  return(rbind(scores, le_scores))

} ## End LE function
