library(dplyr)
library(ggplot2)
library(cowplot)
library(formattable)


## LIKELY FUTURES VIOLIN PLOT ----

scores <- readr::read_csv(here::here("baltic2019draft", "scores.csv")) %>%
  filter(dimension %in% c("future", "status"), goal != "Index") %>%
  tidyr::pivot_wider(names_from = dimension, values_from = score) %>%
  mutate(future_minus_current = future - status) %>%
  mutate(future_minus_current = ifelse(is.nan(future_minus_current), NA, future_minus_current)) %>%
  left_join(read_csv(here("baltic2019draft", "conf", "goals.csv")) %>% select(goal, name), by = "goal")

scores$goal <- factor(
  scores$goal,
  c(
    "MAR","FIS","FP","CW","CON","EUT","TRA",
    "SP","LSP","ICO","LE","ECO","LIV",
    "AO","TR","CS","NP", "BD"
  )
)

rangeplotdf <- left_join(
  scores %>%
    filter(region_id %in% 501:517) %>%
    group_by(goal) %>%
    summarize(
      minscore = min(future_minus_current, na.rm = TRUE),
      maxscore = max(future_minus_current, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      minscore = ifelse(goal == "MAR", NA, minscore),
      maxscore = ifelse(goal == "MAR", NA, maxscore)
    ),
  scores %>%
    filter(region_id == 0) %>%
    select(status, future_minus_current, goal, name),
  by = "goal"
)
rangeplotdf <- rangeplotdf %>%
  mutate(name_label = sprintf("%s (%s)", name, round(status)))

rangeplotdf$name_label <- factor(
  rangeplotdf$name_label,
  arrange(distinct(rangeplotdf, goal, name_label), goal)$name_label
)


goals_pal <- tibble::tibble(
  goal = c(
    "MAR","FIS","FP","CW","CON","EUT","TRA",
    "SP","LSP","ICO","LE","ECO","LIV",
    "AO","TR","CS","NP", "BD"
  ),
  color = c(
    "#549dad","#4ead9d","#53b3ac","#89b181","#60a777","#7ead6d","#9aad7e",
    "#97a4ba","#9fb3d4","#7c96b4","#9a97ba","#7d7ca3","#9990b4",
    "#ddca36","#b6859a","#d0a6a1","#ccb4be","#88b1a6"
  )
)


## violin plot with distributionss of projected changes by goal
violinplot <- ggplot(rangeplotdf) +
  geom_violin(
    data = scores %>%
      filter(region_id %in% 501:517) %>%
      left_join(select(rangeplotdf, name, name_label), by = "name"),
    aes(x = name_label, y = future_minus_current, fill = goal),
    trim = FALSE,
    color = NA, alpha = 0.6, width = 1.5, show.legend = FALSE
  ) +
  geom_point(
    aes(name_label, future_minus_current, color = goal),
    size = 1.7, show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = name_label, ymin = minscore, ymax = maxscore, color = goal),
    width = 0.4, size = 0.4, alpha = 0.8, show.legend = FALSE
  ) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = "\nLikely Future minus Status\n(Current status shown in parentheses)\n") +
  scale_y_continuous(limits = c(-15, 42), breaks = seq(-15, 42, 5)) +
  coord_flip() +
  scale_fill_manual(values = c(rev(goals_pal$color[1:17]), "grey")) +
  scale_color_manual(values = c("grey", rev(goals_pal$color[1:17]))) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 15)
  )
# theme(
#   axis.text.x = element_text(size = 14, angle = 90, hjust = 1),
#   axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5),
#   axis.title = element_text(size = 15, angle = 180)
# )


## FLOWERMAP ----

## contrived data filled with  NAs for Index, so can  have empty polygons
## replace "Index" scores since map_general function requires using one of existing goals
scores_map <- scores <- readr::read_csv(here::here("baltic2019draft", "scores.csv")) %>%
  filter(goal != "Index") %>%
  rbind(
    data.frame(
      goal = rep("Index", 69),
      dimension = "score",
      region_id = c(0:42, 301:309, 501:517),
      score = rep(NA, 69)
    )
  )


dir_B <- file.path(dirname(here::here()), "bhi-data", "BHI 2.0")
subbasin_shp <- sf::st_read(file.path(dirname(dir_B), "Shapefiles", "HELCOM_subbasins_holasbasins"))
subbasin_simple <- rmapshaper::ms_simplify(input = subbasin_shp) %>%
  sf::st_as_sf() %>%
  select(subbasin = Name)
# subbasin_simple <- rmapshaper::ms_simplify(input = subbasin_simple) %>%
#   sf::st_as_sf()
subbasin_shp <- sf::st_buffer(subbasin_shp, 0)


balticmap <- ggplot2::ggplot(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")) +
  geom_sf(size = 0.7, color = "white", fill = "#f2f3f2") +
  geom_sf(data = subbasin_simple, size = 0.4, color = "ghostwhite", fill = NA, alpha = 0.2) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#d1dbeb", color = "#E2EEF3")) +
  scale_x_continuous(limit = c(4, 32)) +
  scale_y_continuous(limit = c(53.5, 66)) +
  theme(panel.grid.major = element_line(color = "#d1dbeb")) +
  labs(x = NULL, y = NULL) +
  ## scalebar
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey40", "white"),
    height = unit(0.1, "cm"),
    width_hint = 0.3,
    text_cex = 0.5
  ) +
  ## north arrow
  ggspatial::annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_y = unit(0.25, "in"),
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("white", "grey40"),
      text_col = "grey40"
    )
  ) +
  ## subbasin labels can uses either ggrepel or regular geom_text
  ggrepel::geom_text_repel(
    data = cbind(subbasin_shp, sf::st_coordinates(sf::st_centroid(subbasin_shp))) %>%
      sf::st_drop_geometry(),
    aes(X, Y, label = Name),
    size = 3.4,
    color = "#8b98a6"
  )


## make flowerplotss if needed...
## can create from bhi-shiny

## flowerplots for basins, make backgrounds transparent
flowerplotloc <- file.path(dirname(here::here()), "bhi-shiny", "dashboard", "figures")

## dataframe of basin flowerplots info
## filenames without png extension and xy placement positions for plotting
basinsinfo <- data.frame(
  ## left align
  flowerplot517base_bothnian_bay = c(x = 0.83, y = 0.96, x0 = 0.68, y0 = 0.96, h = 0, v = 0.4),
  flowerplot515base_bothnian_sea = c(x = 0.70, y = 0.76, x0 = 0.56, y0 = 0.70, h = 0, v = 0.25),
  flowerplot513base_gulf_of_finland = c(x = 0.90, y = 0.65, x0 = 0.82, y0 = 0.54, h = 0, v = 0),
  flowerplot511base_gulf_of_riga = c(x = 0.92, y = 0.44, x0 = 0.70, y0 = 0.36, h = 0, v = 0.4),
  flowerplot509base_eastern_gotland_basin = c(x = 0.78, y = 0.27, x0 = 0.55, y0 = 0.245, h = 0, v = 0.3),
  flowerplot507base_bornholm_basin = c(x = 0.86, y = 0.11, x0 = 0.43, y0 = 0.13, h = 0, v = 0.4),
  flowerplot508base_gdansk_basin = c(x = 0.65, y = 0.125, x0 = 0.525, y0 = 0.045, h = 0, v = 0.5),
  flowerplot510base_western_gotland_basin = c(x = 0.33, y = 0.31, x0 = 0.44, y0 = 0.30, h = 0.5, v = -0.25),
  flowerplot501base_kattegat = c(x = 0.09, y = 0.21, x0 = 0.20, y0 = 0.21, h = 0.4, v = -0.25),
  flowerplot506base_arkona_basin = c(x = 0.02, y = 0, x0 = 0.26, y0 = 0.04, h = 0.4, v = -0.15)
)

plotlines_df <- basinsinfo %>%
  filter(rownames(basinsinfo) %in% c("x", "y", "x0", "y0")) %>%
  cbind(var = c("x", "y", "x0", "y0")) %>%
  tidyr::gather("basin", "value", -var) %>%
  mutate(var = as.character(var)) %>%
  tidyr::spread(var, value)

flowermain <- magick::image_composite(
  file.path(flowerplotloc, "flowerplot0_baltic_sea.png") %>%
    magick::image_read() %>%
    magick::image_scale(1830),
  file.path(flowerplotloc, "flower_curvetxt_baltic_sea.png") %>%
    magick::image_read() %>%
    magick::image_scale(1390),
  offset = "+225-15"
)
flowermain <- flowermain %>%
  magick::image_crop(magick::geometry_area(1460, 1450, 210, 65)) %>%
  magick::image_border(color = "grey80", geometry = "1x1")


## combine everything into one figure using cowplot!
## start with base map and add main flowerplot,
## main flowerplot	## and add plotlines for other flowerplots

flowermap <- ggdraw(balticmap) +
  ## arrows from small plots pointing to basins

  draw_plot(
    ggplot(data = plotlines_df) +
      geom_curve(
        aes(x = x, xend = x0, y = y, yend = y0),
        data = plotlines_df,
        arrow = arrow(length = unit(0.15, "cm")),
        curvature = 0.35,
        size = 0.25,
        color = "#8b98a6"
      ) +
      theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank()
      ) +
      xlim(0, 1) +
      ylim(0, 1) +
      labs(x = NULL, y = NULL),
    x = 0.08, y = 0.03,
    width = 0.88, height = 0.97
  ) +
  ## main flowerplot in a box with labels
  draw_image(
    flowermain,
    x = 0.027, y = 0.535,
    width = 0.48, height = 0.447
  )

## now loop through to add all other basin plots
for(basin in names(basinsinfo)){
  p <- basinsinfo[c("x", "y", "h", "v"), basin]
  flowermap <- flowermap +
    draw_image(
      file.path(flowerplotloc, paste0(basin, ".png")) %>%
        magick::image_read() %>%
        magick::image_trim() %>%
        magick::image_transparent(color = "white"),
      x = p[1]*0.8+0.1, y = p[2]*0.85+0.05,
      width = 0.14, height = 0.14,
      hjust = p[3], vjust = p[4]
    )
}

ggsave(
  "flowermap.png", device = "png",
  path = here("baltic2019draft", "reports", "maps"),
  dpi = 320, width = 26, height = 22.7, units = c("cm")
)



## LIKELY FUTURES TABLE ----

#' create trend table
#'
#' requires a dataframe of OHI scores filtered to the region of interest
#' reads in information from supplement/tables/
#'
#' @param rgn_scores scores dataframe e.g. output of ohicore::CalculateAll (typically from calculate_scores.R),
#' filtered to region
#' @param thresholds two element vector with thresholds values indicating where colors and up/down arrows should switch
#'
#' @return result is a formattable table, saved only if save location is specified as TRUE or a filepath

future_dims_table <- function(rgn_scores, thresholds = c(-0.1, 0.1), include_vals){

  ## wrangle scores with basin info into form for table ----
  status_df <- rgn_scores %>%
    filter(dimension == "status", goal != "Index", region_id %in% 501:517) %>%
    select(-dimension)
  future_df <- rgn_scores %>%
    filter(dimension == "future", goal != "Index", region_id %in% 501:517) %>%
    select(-dimension)
  # dim_df <- left_join(rename(status_df, status = score), rename(future_df, future = score), by = c("region_id", "goal")) %>%
  #   mutate(score = future - status) %>%
  #   select(region_id, goal, score)

  rgns <- readr::read_csv("https://raw.githubusercontent.com/OHI-Science/bhi-prep/master/supplement/lookup_tabs/rgns_complete.csv") %>%
    select(region_id = subbasin_id, Subbasin = subbasin, order = subbasin_order) %>%
    distinct(region_id, Subbasin, order)

  wrangle_subbasin_tabs <- function(df){
    rgns %>%
      select(region_id, Subbasin) %>%
      dplyr::right_join(df, by = "region_id") %>%
      tidyr::pivot_wider(names_from = goal, values_from = score) %>%
      left_join(rgns, by = c("region_id", "Subbasin")) %>%
      arrange(order) %>%
      select(-CS, -MAR, -order) %>%
      ungroup()
  }
  table_df <- wrangle_subbasin_tabs(future_df) %>%
    select(-region_id)
  formatting_df <- wrangle_subbasin_tabs(status_df) %>%
    select(-region_id)


  ## row formatter to include arrow icons ----
  # goals_formatter <- formatter("span", style = x ~ style(
  #   color = ifelse(is.na(x), "white",
  #                  ifelse(x < thresholds[1], "firebrick",
  #                         ifelse(x > thresholds[2], "darkcyan", "gainsboro")))),
  #   x ~ icontext(ifelse(x < thresholds[1], "circle-arrow-down",
  #                       ifelse(x > thresholds[2], "circle-arrow-up", "circle-arrow-right")),
  #                round(x, digits = 2)))
  #
  # general_formatter <- formatter("span", style = x ~ style(
  #   color = ifelse(is.na(x), "white",
  #                  ifelse(x < thresholds[1], "firebrick",
  #                         ifelse(x > thresholds[2], "darkcyan", "gainsboro")))),
  #   x ~ icontext(ifelse(x < thresholds[1], "circle-arrow-down",
  #                       ifelse(x > thresholds[2], "circle-arrow-up", "circle-arrow-right")),
  #                round(x, digits = 2)))

  ## create the table ----
  # tab <- formattable(table_df, align = c("l", rep("c", ncol(table_df)-1)), list(
  #   `Subbasin` = formatter("span", style = ~ style(color = "grey")),
  #   `AO` = general_formatter, `BD` = general_formatter, `CON` = general_formatter,
  #   `CW` = general_formatter, `ECO` = general_formatter, `EUT` = general_formatter,
  #   `FIS` = general_formatter, `FP` = general_formatter, `ICO` = general_formatter,
  #   `LE` = general_formatter, `LIV` = general_formatter, `LSP` = general_formatter,
  #   `MAR` = general_formatter, `NP` = general_formatter, `SP` = general_formatter,
  #   `TR` = general_formatter, `TRA` = general_formatter
  # ))

  format_by_goal <- function(g){
    formatter("span", style = x ~ style(
      color = ifelse(is.na(x), "white", ifelse(x < formatting_df[, g], "firebrick", "darkcyan"))),
      x ~ icontext(ifelse(x < formatting_df[, g], "circle-arrow-down", "circle-arrow-up"), round(x, digits = 2)))
  }

  tab <- formattable(table_df, align = c("l", rep("c", ncol(table_df)-1)), list(
    `Subbasin` = formatter("span", style = ~ style(color = "grey")),
    `AO` = format_by_goal("AO"), `BD` = format_by_goal("BD"), `CON` = format_by_goal("CON"),
    `CW` = format_by_goal("CW"), `ECO` = format_by_goal("ECO"), `EUT` = format_by_goal("EUT"),
    `FIS` = format_by_goal("FIS"), `FP` = format_by_goal("FP"), `ICO` = format_by_goal("ICO"),
    `LE` = format_by_goal("LE"), `LIV` = format_by_goal("LIV"), `LSP` = format_by_goal("LSP"),
    `MAR` = format_by_goal("MAR"), `NP` = format_by_goal("NP"), `SP` = format_by_goal("SP"),
    `TR` = format_by_goal("TR"), `TRA` = format_by_goal("TRA")
  ))
  # for(g in setdiff(colnames(table_df), "subbasin")){
  #   print(data.frame(table_df$subbasin, table_df[,g], formatting_df[,g], table_df[,g] >= formatting_df[,g]))
  # }

  return(tab)
}



