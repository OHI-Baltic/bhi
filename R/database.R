library(RSQLite)
library(DBI)
library(stringr)
library(dplyr)



## creating bhi database structure ----

## in R console:
bhidbconn <- dbConnect(SQLite(), dbname = "dashboard/data/bhi.db")
sql <- read_lines("rebuild/bhidb.sql") %>% 
  str_remove("\t") %>% 
  paste(collapse = "") %>% 
  str_split("(?<=;)") %>% 
  unlist()
for(s in sql[str_length(sql) > 1]){
  dbExecute(bhidbconn, s)
}
## in terminal:
## cd dashboard/data
## pwd
## sqlite3 bhi.db
## .read bhidb.sql
## .schema
## .exit


## making some bhi-shiny specific dataframes to save in db ----

## pressure table
prs_tab <- readr::read_csv(file.path("dashboard", "data", "plotconf.csv")) %>% 
  filter(!is.na(preindex_function)) %>% 
  select(goal, GoalName = name) %>% 
  left_join(
    read_csv(sprintf("%s/conf/pressures_matrix.csv", gh_raw_bhi))[c(1, 4:15)],
    by = "goal"
  ) %>% 
  select(-goal) %>% 
  tidyr::pivot_longer(cols = sp_invasives:ss_wgi, names_to = "Layer", values_to = "Weight")


## resilience table
res_tab <- readr::read_csv(file.path("dashboard", "data", "plotconf.csv")) %>% 
  filter(!is.na(preindex_function)) %>%
  select(goal, GoalName = name) %>%
  left_join(
    read_csv(sprintf("%s/conf/resilience_matrix.csv", gh_raw_bhi))[c(1, 3:22)],
    by = "goal"
  ) %>%
  select(-goal) %>%
  tidyr::pivot_longer(cols = wgi_all:res_reg_pop, names_to = "layer", values_to = "Weight") %>%
  left_join(
    read_csv(sprintf("%s/conf/resilience_categories.csv", gh_raw_bhi)),
    by  = "layer"
  ) %>%
  mutate(Weight = ifelse(is.na(Weight), NA, weight)) %>% 
  select(
    GoalName, Layer = layer, Weight, 
    Category = category, CategoryType = category_type, Subcategory = subcategory
  )


## put scores, regions and subbasins dataframes in database also
regions_df <- readr::read_csv(file.path(dir_main, "data", "regions.csv")) %>% 
  select(
    RegionID = region_id, Subbasin = subbasin, EEZ = eez, RegionName = region_name,
    RegionAreaKM2 = area_km2, RegionOrder = order
  )
subbasins_df <- readr::read_csv(file.path(dir_main, "data", "basins.csv")) %>% 
  select(
    HelcomID = helcom_id, SubbasinID = subbasin_id, Subbasin = subbasin, 
    SubbasinAreaKM2 = area_km2, SubbasinOrder = order
  )
scores_df <- readr::read_csv(file.path(dir_main, "data", "scores.csv")) %>% 
  select(
    RegionID = region_id, Dimension = dimension, Goal = goal,
    Score = score, Year = year
  )



## function to clean tables to put in the database ----

clean_tab <- function(){
  
}


## entering data in bhi database ----

## 1. connect to database
## 2. cleaning datasets as enter them into the database
## 3. disconnect from database

bhidbconn <- dbConnect(SQLite(), dbname = "dashboard/data/bhi.db")


listtables <- list.files("path to files", full.names = TRUE)

for(tab in listtables){
  
  tabresult <- clean_tab(read.csv(tab, stringsAsFactors = F))
  dbWriteTable(
    conn = bhidbconn,
    name = stringr::str_extract(tab, "[a-z0-9_]*(?=.csv)"),
    value = tabresult[["data"]],
    row.names = FALSE,
    append = TRUE,
    binary = TRUE
  )
}

dbDisconnect(bhidbconn)



########################----------------------------------########################


clean_tab <- function(con = dbcon, tab){
  
  ## DATA
  df <- tbl(con, tab) %>%
    collect() %>%
    mutate_if(
      is.character,
      function(x){Encoding(x) = "latin1"; textclean::replace_non_ascii(x)}
    )
  
  ## COLNAMES
  colnm <- DBI::dbListFields(con, tab)
  ## fixed column names:
  ## replace spaces with underscores, remove dot, square brackets, non-ascii chars, etc...
  editcolnm <- colnames(df) %>%
    str_replace_all(" |\\.", "_") %>%
    str_replace_all("/", "_per_") %>%
    str_remove_all("\\. |\\[|\\]|_$") %>%
    textclean::replace_non_ascii() %>%
    str_replace(pattern = " mu ", "u") %>%
    str_to_lower()
  
  coltypes <- c()
  for(col in colnm){
    coltypes <- c(coltypes, typeof(df[[col]]))
  }
  colnames(df) <- editcolnm
  
  ## GEOSPATIAL STUFF
  ## where relevant combine lat long into a single geospatial column
  ## check lat long exist and whether decimal deg or min/sec etc
  chksp <- FALSE
  latlon <- data.frame(
    lon = c("longitude", "long", "lon"),
    lat = c("latitude", "lat", "lat")
  )
  for(i in 1:nrow(latlon)){
    lon <- editcolnm[grep(as.character(latlon[i, "lon"]), editcolnm)]
    if(any(grep(as.character(latlon[i, "lon"]), editcolnm))){
      break
    }
  }
  for(j in 1:nrow(latlon)){
    lat <- editcolnm[grep(as.character(latlon[i, "lat"]), editcolnm)]
    if(any(grep(as.character(latlon[i, "lat"]), editcolnm))){
      break
    }
  }
  if(i == j & length(lon) != 0 & length(lat) != 0){
    if(is.numeric(df[[lon]]) & is.numeric(df[[lat]])){
      chkna <- nrow(df %>% filter(is.na(!!sym(lon))) %>% filter(is.na(!!sym(lat)))) == 0
      chklat <- max(df[[lat]], na.rm = TRUE) < 85 & min(df[[lat]], na.rm = TRUE) > -85
      chklon <- max(df[[lon]], na.rm = TRUE) < 185 & min(df[[lon]], na.rm = TRUE) > -180
      chksp <- chklon & chklat & chkna
    }
  }
  if(chksp){
    df <- sf::st_as_sf(x = df, coords = c(lon, lat), remove = FALSE)
  }
  
  return(
    list(
      data = df,
      colnames = data.frame(original = colnm, revised = editcolnm, typ = coltypes) %>%
        mutate(
          tabname = tab,
          newtabname = ifelse(
            str_detect(db, "[0-9]"),
            paste0("level", str_extract(db, "[0.9]"), "_", tab), tab
          )
        )
    )
  )
}

## check before migrating BHI-database to srcdb-PostgreSQL ----
## loop through tables, cleaning and checking
chkresults <- list()
for(tab in DBI::dbListTables(dbcon)){
  tabresult <- clean_tab(dbcon, tab)
  chkresults[["colnames"]][[tab]] <- tabresult[["colnames"]]
}
## IMPORTANT save this info about how changed column names and data types
trackcols <- purrr::map(magrittr::extract(chkresults, "colnames"), bind_rows)$colnames
readr::write_csv(trackcols, file.path("/Users/eleanorecampbell/Desktop", "trackDBmigrate.csv"))


