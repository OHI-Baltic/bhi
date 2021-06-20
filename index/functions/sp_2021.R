
SP <- function(layers, scores){

  ## From code in 'functions.R SP' of v2015 BHI assessment, see bhi-1.0-archive github repo

  scen_year <- layers$data$scenario_year


  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP

  sp_scores <- scores %>%
    dplyr::filter(
      goal %in% c("ICO", "LSP"),
      dimension %in% c("status", "trend", "future", "score")
    ) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::mutate(goal = "SP") %>%
    dplyr::ungroup() %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    dplyr::mutate(score = ifelse(is.nan(score), NA, score))

  return(rbind(scores, sp_scores))

} ## End SP function
