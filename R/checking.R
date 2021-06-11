## Libraries
source(here::here("R", "setup.R"))
library(ggplot2)
library(dplyr)
library(plotly)
library(git2r)


## Functions

#' map scores to compare between two scores.csv tables for two specified years
#'
#' @param scores1 first scores table (dataframe) to use in comparison
#' @param year1 year of data within first scores table to use
#' @param scores2 second scores table
#' @param year2 year within second scores table
#' @param dim a string specifying one dimension to investigate
#' @param goals a string or vector of strings specifying goal(s) to investigate
#' @param sf_bhirgns sf object with BHI regions, for mapping
#'
#' @return score maps and a difference map side-by-side; one row per goal when multiple goals are specified
map_comparison <- function(scores1, year1, scores2, year2, dim = "score", goals = "Index", sf_bhirgns){

  require(dplyr)
  require(ggplot2)
  require(sf)

  scores1_yrs <- unique(scores1$year)
  scores2_yrs <- unique(scores2$year)

  if(!(year1 %in% scores1_yrs & year2 %in% scores2_yrs)){
    return("years to compare must be in respective score data")

  } else {

    g <- goals

    join_scores <- full_join(
      filter(scores1, dimension %in% dim, goal %in% g, year == year1),
      filter(scores2, dimension %in% dim, goal %in% g, year == year2),
      by = c("region_id", "dimension", "goal")
    )

    comparison_tab <- join_scores %>%
      rename(
        scores_1 = score.x, year1 = year.x,
        scores_2 = score.y, year2 = year.y
      ) %>%
      select(scores_1, scores_2, bhi_id = region_id) %>%
      mutate(scores_diff = scores_1 - scores_2) %>%
      tidyr::pivot_longer(
        cols = starts_with("scores"),
        names_to = "metric",
        values_to = "value"
      ) %>%
      mutate(metric = case_when(
        metric == "scores_1" ~ "Scores from Dataframe 1",
        metric == "scores_2" ~ "Scores from Dataframe 2",
        metric == "scores_diff" ~ "Difference between Scores"
      ))


    if(object.size(sf_bhirgns) > 2.5E6){
      sf_bhirgns <- rmapshaper::ms_simplify(sf_bhirgns)
    }

    ## make scores maps
    scores_maps <- sf_bhirgns %>%
      left_join(filter(comparison_tab, metric != "scores_diff"), by = "bhi_id") %>%
      ggplot() +
      geom_sf(
        aes(fill = value),
        color = "snow",
        size = 0.1,
        alpha = 0.7
      ) +
      scale_fill_viridis_c() +
      facet_wrap(~metric, ncol = 2)


    ## make scores diff maps
    diff_maps <- sf_bhirgns %>%
      left_join(filter(comparison_tab, metric == "scores_diff"), by = "bhi_id") %>%
      ggplot() +
      geom_sf(
        aes(fill = value),
        color = "dimgrey",
        size = 0.1,
        alpha = 0.7
      ) +
      scale_fill_gradient2(
        low = "lightcoral",
        mid = "thistle",
        high = "royalblue"
      ) +
      facet_wrap(~metric, ncol = 1)


    maps <- gridExtra::grid.arrange(
      scores_maps, diff_maps,
      ncol = 2,
      widths = c(2.1, 1.1)
    )

    return(maps)
  }
}

#' compare scores between two scores.csv tables for two specified years
#'
#' @param scores1 first scores table (dataframe) to use in comparison
#' @param year1 year of data within first scores table to use
#' @param scores2 second scores table
#' @param year2 year within second scores table
#' @param dim a string or vector of strings specifying dimension(s) to investigate
#' @param goals a string or vector of strings specifying goal(s) to investigate
#'
#' @return returns a list with two objects: 1. a faceted plot and 2. a summary table

compare_scores <- function(scores1, year1, scores2, year2, dim = "score", goals = "Index"){

  require(dplyr)
  require(ggplot2)

  scores1_yrs <- unique(scores1$year)
  scores2_yrs <- unique(scores2$year)

  if(!(year1 %in% scores1_yrs & year2 %in% scores2_yrs)){
    return("years to compare must be in respective score data")

  } else {

    g <- goals

    join_scores <- full_join(
      filter(scores1, dimension %in% dim, goal %in% g, year == year1),
      filter(scores2, dimension %in% dim, goal %in% g, year == year2),
      by = c("region_id", "dimension", "goal")
    )

    comparison_tab <- join_scores %>%
      rename(
        scores1 = score.x, year1 = year.x,
        scores2 = score.y, year2 = year.y
      ) %>%
      mutate(
        scores_diff = scores1 - scores2,
        yr_diff = ifelse(year1 - year2 > 0, year1 - year2, 1),
        yearly_diff = scores_diff/yr_diff
      )

    ## comparision summary table
    comparison_summary <- comparison_tab %>%
      group_by(goal, dimension) %>%
      summarise(
        mean_diff = mean(scores_diff, na.rm = TRUE),
        sd_diff = sd(scores_diff, na.rm = TRUE),
        number_na = sum(is.na(scores_diff)),
        mean_yearly_diff = mean(yearly_diff, na.rm = TRUE),
        biggest_diff = max(abs(scores_diff), na.rm = TRUE)
      )

    ## create plot(s) for comparing two datasets
    pal <- RColorBrewer::brewer.pal(6, "Set2")
    n_facet <- ceiling(length(goal)/3)

    comparison_plot <- ggplot(data = na.omit(comparison_tab)) +
      geom_point(
        aes(scores1, scores2, color = dimension, label = region_id),
        alpha = 0.5, size = 3
      ) +
      scale_color_manual(values = pal) +
      geom_abline(
        slope = 1,
        intercept = 0,
        color = "gray70"
      ) +
      theme_bw() +
      theme(legend.position = c(0.9, 0.12)) +
      labs(
        x = sprintf("\n Scores for %s from Table 1", year1),
        y = sprintf("Scores for %s from Table 2 \n", year2)
      ) +
      facet_wrap(~goal, ncol = n_facet) # split up by (sub)goals

    return(list(comparison_plot, comparison_summary, comparison_tab))
  }
}


#' change plot
#'
#' This function compares BHI scores from the current analysis and a previous commit
#' Written originally by Melanie Frazier for the ohi-global assessment
#'
#' @param repo repository name, e.g. 'bhi' or  'bhi-1.0-archive'
#' @param assessmt name of assessment subfolder that contains the 'scores.csv' file of interest
#' @param commit 7 digit sha number identifying the commit. Otherwise, it is compared to the previous commit
#'
#' @return list of two objects: first is the interactive html image, second is a dataframe recording the differences

change_plot <- function(repo = "bhi", assessmt = basename(dir_assess), commit = "previous"){
  ## from ohicore/R/score_check.R by @Melsteroni
  ## find full original script in R folder of ohicore github repo

  r <- here::here()

  if(commit == "previous"){
    c <- as.character(git2r::commits(git2r::repository(r))[[1]])[1]
  } else { c <- commit }

  tmp <- git2r::remote_url(git2r::repository(r))
  org <- stringr::str_split(tmp, "/")[[1]][4]
  path <- file.path(r, assessmt, "scores.csv")

  data_old <- ohicore::read_git_csv(
    paste(org, repo, sep = .Platform$file.sep),
    substr(c, 1, 7), path
  ) %>% dplyr::select(goal, dimension, region_id, old_score = score)

  ## join old data with new data, and calculate change
  data <- read.csv(path) %>%
    dplyr::left_join(data_old, by = c("goal", "dimension", "region_id")) %>%
    dplyr::mutate(change = score - old_score)

  ## create plot
  plotly_fig <- plotly::ggplotly(

    ggplot2::ggplot(data_new, aes(x = goal, y = change, color = dimension)) +
      geom_jitter(
        aes(text = paste0("rgn = ", region_id)),
        position = position_jitter(width = 0.2, height = 0),
        shape = 19,
        size = 1
      ) +
      theme_bw() +
      labs(title = paste(assessmt, commit, sep = " "), y = "Change in score", x = "") +
      scale_x_discrete(
        limits = c(
          "Index", "AO", "FP", "FIS", "MAR", "BD",
          "CW", "CON", "TRA", "EUT", "SP", "LSP", "ICO",
          "LE", "ECO", "LIV", "TR", "CS", "NP"
        )) +
      scale_colour_brewer(palette = "Dark2")
  )

  return(list(plotly_fig, data))

  ## to save, could use htmlwidgets package:
  ## htmlwidgets::saveWidget(plotly::as.widget(plotly_fig), "name_file.html", selfcontained = TRUE)
}
